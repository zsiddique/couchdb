% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
-module(couch_os_daemons).
-behaviour(gen_server).

-export([start_link/0, info/0, info/1, config_change/2]).
-export([check_app_password/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-include("couch_db.hrl").

-record(daemon, {
    port,
    name,
    cmd,
    kill,
    app_port=nil,
    app_pw=nil,
    app_dir=nil,
    app_couch=nil,
    status=running,
    cfg_patterns=[],
    errors=[],
    buf=[]
}).

-define(PORT_OPTIONS, [stream, {line, 1024}, binary, exit_status, hide]).
-define(TIMEOUT, 5000).
-define(NODEJS_EXTRA, "couchjs-extra").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

info() ->
    info([]).

info(Options) ->
    gen_server:call(?MODULE, {daemon_info, Options}).

check_app_password(Pw) ->
    gen_server:call(?MODULE, {check_app_password, Pw}).

config_change(Section, Key) ->
    gen_server:cast(?MODULE, {config_change, Section, Key}).

init(_) ->
    process_flag(trap_exit, true),
    ok = couch_config:register(fun ?MODULE:config_change/2),
    Table = ets:new(?MODULE, [protected, set, {keypos, #daemon.port}]),
    reload_daemons(Table),
    {ok, Table}.

terminate(_Reason, Table) ->
    [stop_port(D) || D <- ets:tab2list(Table)],
    ok.

handle_call({daemon_info, Options}, _From, Table) when is_list(Options) ->
    case lists:member(table, Options) of
        true ->
            {reply, {ok, ets:tab2list(Table)}, Table};
        _ ->
            {reply, {ok, Table}, Table}
    end;
handle_call({check_app_password, Password}, _From, Table) ->
    Result = check_app_password(Password, Table),
    {reply, Result, Table};
handle_call(Msg, From, Table) ->
    ?LOG_ERROR("Unknown call message to ~p from ~p: ~p", [?MODULE, From, Msg]),
    {stop, error, Table}.

handle_cast({config_change, Sect, Key}, Table) ->
    restart_daemons(Table, Sect, Key),
    case Sect of
        "os_daemons" -> reload_daemons(Table);
        _ -> ok
    end,
    {noreply, Table};
handle_cast(stop, Table) ->
    {stop, normal, Table};
handle_cast(Msg, Table) ->
    ?LOG_ERROR("Unknown cast message to ~p: ~p", [?MODULE, Msg]),
    {stop, error, Table}.

handle_info({'EXIT', Port, Reason}, Table) ->
    case ets:lookup(Table, Port) of
        [] ->
            ?LOG_INFO("Port ~p exited after stopping: ~p~n", [Port, Reason]);
        [#daemon{status=stopping}] ->
            true = ets:delete(Table, Port);
        [#daemon{name=Name, status=restarting}=D] ->
            ?LOG_INFO("Daemon ~P restarting after config change.", [Name]),
            true = ets:delete(Table, Port),
            {ok, Port2, App} = start_port(D#daemon.cmd),
            {APort, APw, ADir, ACouch} = App,
            true = ets:insert(Table, D#daemon{
                port=Port2, status=running, kill=undefined, buf=[],
                app_port=APort, app_pw=APw, app_dir=ADir, app_couch=ACouch
            });
        [#daemon{name=Name, status=halted}] ->
            ?LOG_ERROR("Halted daemon process: ~p", [Name]);
        [D] ->
            ?LOG_ERROR("Invalid port state at exit: ~p", [D])
    end,
    {noreply, Table};
handle_info({Port, closed}, Table) ->
    handle_info({Port, {exit_status, closed}}, Table);
handle_info({Port, {exit_status, Status}}, Table) ->
    case ets:lookup(Table, Port) of
        [] ->
            ?LOG_ERROR("Unknown port ~p exiting ~p", [Port, Status]),
            {stop, {error, unknown_port_died, Status}, Table};
        [#daemon{name=Name, status=restarting}=D] ->
            ?LOG_INFO("Daemon ~P restarting after config change.", [Name]),
            true = ets:delete(Table, Port),
            {ok, Port2, App} = start_port(D#daemon.cmd),
            {APort, APw, ADir, ACouch} = App,
            true = ets:insert(Table, D#daemon{
                port=Port2, status=running, kill=undefined, buf=[],
                app_port=APort, app_pw=APw, app_dir=ADir, app_couch=ACouch
            }),
            {noreply, Table};
        [#daemon{status=stopping}=D] ->
            % The configuration changed and this daemon is no
            % longer needed.
            ?LOG_DEBUG("Port ~p shut down.", [D#daemon.name]),
            true = ets:delete(Table, Port),
            {noreply, Table};
        [D] ->
            % Port died for unknown reason. Check to see if it's
            % died too many times or if we should boot it back up.
            case should_halt([now() | D#daemon.errors]) of
                {true, _} ->
                    % Halting the process. We won't try and reboot
                    % until the configuration changes.
                    Fmt = "Daemon ~p halted with exit_status ~p",
                    ?LOG_ERROR(Fmt, [D#daemon.name, Status]),
                    D2 = D#daemon{status=halted, errors=nil, buf=nil},
                    true = ets:insert(Table, D2),
                    {noreply, Table};
                {false, Errors} ->
                    % We're guessing it was a random error, this daemon
                    % has behaved so we'll give it another chance.
                    Fmt = "Daemon ~p is being rebooted after exit_status ~p",
                    ?LOG_INFO(Fmt, [D#daemon.name, Status]),
                    true = ets:delete(Table, Port),
                    {ok, Port2, App} = start_port(D#daemon.cmd),
                    {APort, APw, ADir, ACouch} = App,
                    true = ets:insert(Table, D#daemon{
                        port=Port2, status=running, kill=undefined,
                        errors=Errors, buf=[],
                        app_port=APort, app_pw=APw, app_dir=ADir, app_couch=ACouch
                    }),
                    {noreply, Table}
            end;
        _Else ->
            throw(error)
    end;
handle_info({Port, {data, {noeol, Data}}}, Table) ->
    [#daemon{buf=Buf}=D] = ets:lookup(Table, Port),
    true = ets:insert(Table, D#daemon{buf=[Data | Buf]}),
    {noreply, Table};
handle_info({Port, {data, {eol, Data}}}, Table) ->
    [#daemon{buf=Buf}=D] = ets:lookup(Table, Port),
    Line = lists:reverse(Buf, Data),
    % The first line echoed back is the kill command
    % for when we go to get rid of the port. Lines after
    % that are considered part of the stdio API.
    case D#daemon.kill of
        undefined ->
            true = ets:insert(Table, D#daemon{kill=?b2l(Line), buf=[]});
        _Else ->
            D2 = case (catch ?JSON_DECODE(Line)) of
                {invalid_json, Rejected} ->
                    ?LOG_ERROR("Ignoring OS daemon request: ~p", [Rejected]),
                    D;
                JSON ->
                    {ok, D3} = handle_port_message(D, JSON),
                    D3
            end,
            true = ets:insert(Table, D2#daemon{buf=[]})
    end,
    {noreply, Table};
handle_info({Port, Error}, Table) ->
    ?LOG_ERROR("Unexpectd message from port ~p: ~p", [Port, Error]),
    stop_port(Port),
    [D] = ets:lookup(Table, Port),
    true = ets:insert(Table, D#daemon{status=restarting, buf=nil}),
    {noreply, Table};
handle_info(Msg, Table) ->
    ?LOG_ERROR("Unexpected info message to ~p: ~p", [?MODULE, Msg]),
    {stop, error, Table}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal API

check_app_password(Password, Table) ->
    Daemons = ets:tab2list(Table),
    CheckPw = fun(_Daemon, ok) ->
        true;
    (#daemon{cmd=?NODEJS_EXTRA, app_pw=AppPw}, false) ->
        Prefix = <<"_nodejs:">>,
        AppPwBin = ?l2b(AppPw),
        AppAuth = <<Prefix/binary, AppPwBin/binary>>,
        case Password of
        AppAuth ->
            true;
        _ ->
            false
        end;
    (#daemon{cmd=Cmd, app_pw=AppPw}, false) ->
        false
    end,
    lists:foldl(CheckPw, false, Daemons).

%
% Port management helpers
%

start_port(?NODEJS_EXTRA) ->
    AppPort = 10000 + random:uniform(10000) * 2,  % Even number 10000 - 20000
    AppPw = ?b2l(couch_uuids:random()),

    PrivDir = couch_util:priv_dir(),
    AppDir = filename:join(PrivDir, "nodejs"),

    CouchPort = couch_config:get("httpd", "port"),
    AppCouch = "http://127.0.0.1:" ++ CouchPort,

    App = {AppPort, AppPw, AppDir, AppCouch},
    Env = [ {"_couchdb_app_port", integer_to_list(AppPort)}
          , {"_couchdb_app_password", AppPw}
          , {"_couchdb_app_dir", AppDir}
          , {"_couchdb_app_couch", AppCouch}
          ],

    {ok, Port} = start_port(?NODEJS_EXTRA, Env),
    {ok, Port, App};

start_port(Command) ->
    {ok, Port} = start_port(Command, []),
    {ok, Port, {nil, nil, nil, nil}}.

start_port(Command, EnvPairs) ->
    PrivDir = couch_util:priv_dir(),
    Spawnkiller = filename:join(PrivDir, "couchspawnkillable"),
    Opts = case lists:keytake(env, 1, ?PORT_OPTIONS) of
        false ->
            ?PORT_OPTIONS ++ [ {env,EnvPairs} ];
        {value, {env,OldPairs}, SubOpts} ->
            AllPairs = lists:keymerge(1, EnvPairs, OldPairs),
            SubOpts ++ [ {env,AllPairs} ]
    end,
    Port = open_port({spawn, Spawnkiller ++ " " ++ Command}, Opts),
    {ok, Port}.


stop_port(#daemon{port=Port, kill=undefined}=D) ->
    ?LOG_ERROR("Stopping daemon without a kill command: ~p", [D#daemon.name]),
    catch port_close(Port);
stop_port(#daemon{port=Port}=D) ->
    ?LOG_DEBUG("Stopping daemon: ~p", [D#daemon.name]),
    os:cmd(D#daemon.kill),
    catch port_close(Port).


handle_port_message(#daemon{port=Port}=Daemon, [<<"get">>, Section]) ->
    KVs = couch_config:get(Section),
    Data = lists:map(fun({K, V}) -> {?l2b(K), ?l2b(V)} end, KVs),
    Json = iolist_to_binary(?JSON_ENCODE({Data})),
    port_command(Port, <<Json/binary, "\n">>),
    {ok, Daemon};
handle_port_message(#daemon{port=Port}=Daemon, [<<"get">>, Section, Key]) ->
    Value = case couch_config:get(Section, Key, null) of
        null -> null;
        String -> ?l2b(String)
    end,
    Json = iolist_to_binary(?JSON_ENCODE(Value)),
    port_command(Port, <<Json/binary, "\n">>),
    {ok, Daemon};
handle_port_message(Daemon, [<<"register">>, Sec]) when is_binary(Sec) ->
    Patterns = lists:usort(Daemon#daemon.cfg_patterns ++ [{?b2l(Sec)}]),
    {ok, Daemon#daemon{cfg_patterns=Patterns}};
handle_port_message(Daemon, [<<"register">>, Sec, Key])
                        when is_binary(Sec) andalso is_binary(Key) ->
    Pattern = {?b2l(Sec), ?b2l(Key)},
    Patterns = lists:usort(Daemon#daemon.cfg_patterns ++ [Pattern]),
    {ok, Daemon#daemon{cfg_patterns=Patterns}};
handle_port_message(#daemon{name=Name}=Daemon, [<<"log">>, Msg]) ->
    handle_log_message(Name, Msg, <<"info">>),
    {ok, Daemon};
handle_port_message(#daemon{name=Name}=Daemon, [<<"log">>, Msg, {Opts}]) ->
    Level = couch_util:get_value(<<"level">>, Opts, <<"info">>),
    handle_log_message(Name, Msg, Level),
    {ok, Daemon};
handle_port_message(#daemon{name=Name}=Daemon, Else) ->
    ?LOG_ERROR("Daemon ~p made invalid request: ~p", [Name, Else]),
    {ok, Daemon}.


handle_log_message(Name, Msg, _Level) when not is_binary(Msg) ->
    ?LOG_ERROR("Invalid log message from daemon ~p: ~p", [Name, Msg]);
handle_log_message(Name, Msg, <<"debug">>) ->
    ?LOG_DEBUG("Daemon ~p :: ~s", [Name, ?b2l(Msg)]);
handle_log_message(Name, Msg, <<"info">>) ->
    ?LOG_INFO("Daemon ~p :: ~s", [Name, ?b2l(Msg)]);
handle_log_message(Name, Msg, <<"error">>) ->
    ?LOG_ERROR("Daemon: ~p :: ~s", [Name, ?b2l(Msg)]);
handle_log_message(Name, Msg, Level) ->
    ?LOG_ERROR("Invalid log level from daemon: ~p", [Level]),
    ?LOG_INFO("Daemon: ~p :: ~s", [Name, ?b2l(Msg)]).

%
% Daemon management helpers
%

reload_daemons(Table) ->
    % List of daemons we want to have running.
    % The nodejs helper is mandatory.
    Configured1 = lists:sort(couch_config:get("os_daemons")),
    Configured = lists:keystore("nodejs_couchdb", 1, Configured1,
        {"nodejs_couchdb", ?NODEJS_EXTRA}),
    
    % Remove records for daemons that were halted.
    MSpecHalted = #daemon{name='$1', cmd='$2', status=halted, _='_'},
    Halted = lists:sort([{N, C} || [N, C] <- ets:match(Table, MSpecHalted)]),
    ok = stop_os_daemons(Table, find_to_stop(Configured, Halted, [])),
    
    % Stop daemons that are running
    % Start newly configured daemons
    MSpecRunning = #daemon{name='$1', cmd='$2', status=running, _='_'},
    Running = lists:sort([{N, C} || [N, C] <- ets:match(Table, MSpecRunning)]),
    ok = stop_os_daemons(Table, find_to_stop(Configured, Running, [])),
    ok = boot_os_daemons(Table, find_to_boot(Configured, Running, [])),
    ok.


restart_daemons(Table, Sect, Key) ->
    restart_daemons(Table, Sect, Key, ets:first(Table)).

restart_daemons(_, _, _, '$end_of_table') ->
    ok;
restart_daemons(Table, Sect, Key, Port) ->
    [D] = ets:lookup(Table, Port),
    HasSect = lists:member({Sect}, D#daemon.cfg_patterns),
    HasKey = lists:member({Sect, Key}, D#daemon.cfg_patterns),
    case HasSect or HasKey of
        true ->
            stop_port(D),
            D2 = D#daemon{status=restarting, buf=nil},
            true = ets:insert(Table, D2);
        _ ->
            ok
    end,
    restart_daemons(Table, Sect, Key, ets:next(Table, Port)).
    

stop_os_daemons(_Table, []) ->
    ok;
stop_os_daemons(Table, [{Name, Cmd} | Rest]) ->
    [[Port]] = ets:match(Table, #daemon{port='$1', name=Name, cmd=Cmd, _='_'}),
    [D] = ets:lookup(Table, Port),
    case D#daemon.status of
        halted ->
            ets:delete(Table, Port);
        _ ->
            stop_port(D),
            D2 = D#daemon{status=stopping, errors=nil, buf=nil},
            true = ets:insert(Table, D2)
    end,
    stop_os_daemons(Table, Rest).
    
boot_os_daemons(_Table, []) ->
    ok;
boot_os_daemons(Table, [{Name, Cmd} | Rest]) ->
    {ok, Port, App} = start_port(Cmd),
    {APort, APw, ADir, ACouch} = App,
    true = ets:insert(Table, #daemon{port=Port, name=Name, cmd=Cmd,
        app_port=APort, app_pw=APw, app_dir=ADir, app_couch=ACouch
    }),
    boot_os_daemons(Table, Rest).
    
% Elements unique to the configured set need to be booted.
find_to_boot([], _Rest, Acc) ->
    % Nothing else configured.
    Acc;
find_to_boot([D | R1], [D | R2], Acc) ->
    % Elements are equal, daemon already running.
    find_to_boot(R1, R2, Acc);
find_to_boot([D1 | R1], [D2 | _]=A2, Acc) when D1 < D2 ->
    find_to_boot(R1, A2, [D1 | Acc]);
find_to_boot(A1, [_ | R2], Acc) ->
    find_to_boot(A1, R2, Acc);
find_to_boot(Rest, [], Acc) ->
    % No more candidates for already running. Boot all.
    Rest ++ Acc.

% Elements unique to the running set need to be killed.
find_to_stop([], Rest, Acc) ->
    % The rest haven't been found, so they must all
    % be ready to die.
    Rest ++ Acc;
find_to_stop([D | R1], [D | R2], Acc) ->
    % Elements are equal, daemon already running.
    find_to_stop(R1, R2, Acc);
find_to_stop([D1 | R1], [D2 | _]=A2, Acc) when D1 < D2 ->
    find_to_stop(R1, A2, Acc);
find_to_stop(A1, [D2 | R2], Acc) ->
    find_to_stop(A1, R2, [D2 | Acc]);
find_to_stop(_, [], Acc) ->
    % No more running daemons to worry about.
    Acc.

should_halt(Errors) ->
    RetryTimeCfg = couch_config:get("os_daemon_settings", "retry_time", "5"),
    RetryTime = list_to_integer(RetryTimeCfg),

    Now = now(),
    RecentErrors = lists:filter(fun(Time) ->
        timer:now_diff(Now, Time) =< RetryTime * 1000000
    end, Errors),

    RetryCfg = couch_config:get("os_daemon_settings", "max_retries", "3"),
    Retries = list_to_integer(RetryCfg),

    {length(RecentErrors) >= Retries, RecentErrors}.
