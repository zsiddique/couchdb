% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_debug).
-include("couch_db.hrl").

-export([handle_debug_req/1]).

-import(couch_httpd, [header_value/2, send_method_not_allowed/2]).


handle_debug_req(#httpd{method='GET', path_parts=[_Debug]}=Req) -> ok
    , ok = couch_httpd:verify_is_server_admin(Req)
    , Procs = couch_query_servers:debug_ports("javascript")
    , Pids = [ Pid || {Pid, _Port} <- Procs ]
    , couch_httpd:send_json(Req, 200, {[ {ok,true}, {pids,Pids}, {procs,{Procs}} ]})
    ;

handle_debug_req(#httpd{method='GET', path_parts=[_Debug, Pid | _Rest]=Path}=Req) -> ok
    , ok = couch_httpd:verify_is_server_admin(Req)
    %, io:format("Must look up: ~p for: ~p\n", [Pid, Path])
    , Procs = couch_query_servers:debug_ports("javascript")
    , case lists:keyfind(Pid, 1, Procs)
        of {Pid, Port} -> ok
            % Proxy to socket.io. Use the normal httpd_proxy for everything except the
            % websockets request.
            , InspectorPort = Port + 1
            , case Path
                of [<<"_debug">>, Pid, <<"socket.io">>, <<"1">>, <<"websocket">>, _Key] -> ok
                    , relay(Req, InspectorPort)
                ; _ -> ok
                    , Url = io_lib:format("http://127.0.0.1:~w/_debug/", [InspectorPort])
                    %, io:format("Proxy: ~p\nPath = ~p\n", [Url, Path])
                    , couch_httpd_proxy:handle_proxy_req(Req, ?l2b(Url))
                end
        ; false -> ok
            , io:format("No such pid: ~p\n", [Pid])
            , couch_httpd:send_json(Req, 404, {[ {error,pid_not_found}, {pid,Pid} ]})
        end
    ;

handle_debug_req(Req) -> ok
    , send_method_not_allowed(Req, "GET")
    .

relay(Req, Port) when is_integer(Port) -> ok
    , io:format("socket.io connection to port ~w\n", [Port])
    , case gen_tcp:connect("127.0.0.1", Port, [binary, {packet,0}, {active,false}])
        of {ok, InspectorSocket} -> ok
            , io:format("Connected to inspector on :~w\n", [Port])
            , relay(Req, InspectorSocket)
        ; {error, Error} -> ok
            , Resp = {[ {error,inspector_proxy}, {result, Error} ]}
            , couch_httpd:send_json(Req, 502, Resp)
        end
    ;

relay(#httpd{mochi_req=MochiReq}=Req, InspectorSocket) -> ok
    , Client = MochiReq:get(socket)
    , ReqBytes = request_to_iolist(Req)
    , Size = iolist_size(ReqBytes)
    , io:format("Send ~w byte request to inspector\n", [Size])
    , ok = gen_tcp:send(InspectorSocket, ReqBytes)
    , relay(Client, InspectorSocket, Size, 0)
    .

relay(Client, Remote, FromClient, FromRemote) -> ok
    %, io:format("Relay client=~w remote=~w\n", [FromClient, FromRemote])
    , inet:setopts(Client, [{packet,0}, {active,once}, {nodelay,true} ])
    , inet:setopts(Remote, [{packet,0}, {active,once}, {nodelay,true} ])
    , receive
        {_Type, Client, Data} -> ok
            %, io:format("Client: ~p\n", [Data])
            , gen_tcp:send(Remote, Data)
            , relay(Client, Remote, FromClient + size(Data), FromRemote)
        ; {_Type, Remote, Data} -> ok
            %, io:format("Remote ~w bytes: ~p\n", [size(Data), Data])
            , gen_tcp:send(Client, Data)
            , relay(Client, Remote, FromClient, FromRemote + size(Data))
        ; {tcp_closed, Client} -> ok
            , io:format("Client disconnected\n")
            , gen_tcp:close(Remote)
            , {ok, ok}
        ; {tcp_closed, Remote} -> ok
            % XXX
            , io:format("Remote disconnected\n")
            %, gen_tcp:controlling_process(Remote)
            %, catch Owner ! {stream_close, bad_reqid}
            , gen_tcp:close(Client)
            %, {ok, FromClient, FromRemote}
            , {ok, ok}
        ; Else -> ok
            , ?LOG_ERROR("Relay error: ~p", [Else])
        end
    %, couch_httpd:send_json(Req, 200, {[ {ok,true}, {todo,<<"To do">>} ]})
    .

request_to_iolist(#httpd{method=Method, mochi_req=MochiReq}) -> ok
    , Path = MochiReq:get(raw_path)
    , Version = case MochiReq:get(version)
        of {1,1} -> "1.1"
        ;  _     -> "1.0"
        end
    , Action = io_lib:format("~s ~s HTTP/~s", [Method, Path, Version])
    , MochiHeaders = mochiweb_headers:to_list(MochiReq:get(headers))
    , Headers = [ [couch_util:to_binary(Key), ": ", Val, "\r\n"] || {Key, Val} <- MochiHeaders]
    , [Action, "\r\n", Headers, "\r\n"]
    .


%
% Utilities
%


% vim: sts=4 sw=4 et
