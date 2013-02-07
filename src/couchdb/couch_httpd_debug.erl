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
    , couch_httpd:send_json(Req, 200, {[ {ok,true}, {pids, Pids} ]})
    ;

handle_debug_req(#httpd{method='GET', path_parts=[_Debug, Pid | _Rest]=Path}=Req) -> ok
    , ok = couch_httpd:verify_is_server_admin(Req)
    , io:format("Must look up: ~p for: ~p\n", [Pid, Path])
    , Procs = couch_query_servers:debug_ports("javascript")
    , case lists:keyfind(Pid, 1, Procs)
        of {Pid, Port} -> ok
            , InspectorPort = Port + 1
            %, proxy_to_inspector(Req, InspectorPort)
            , R = couch_httpd:send_json(Req, 200, {[ {ok,true}, {todo,<<"To do">>} ]})
            , io:format("send_json returns: ~p\n", [R])
            , R
        ; false -> ok
            , io:format("No such pid: ~p\n", [Pid])
            , couch_httpd:send_json(Req, 404, {[ {error,pid_not_found}, {pid,Pid} ]})
        end
    ;

handle_debug_req(Req) -> ok
    , send_method_not_allowed(Req, "GET")
    .

relay(#httpd{mochi_req=MochiReq}=Req, InspectorSocket) -> ok
    , ReqBytes = request_to_iolist(Req)
    , Client = MochiReq:get(socket)
    , gen_tcp:send(InspectorSocket, ReqBytes)
    , relay(Client, InspectorSocket, 0, 0)
    .

relay(Client, Remote, BytesIn, BytesOut) -> ok
    %, io:format("Relay in=~w out=~w\n", [BytesIn, BytesOut])
    , inet:setopts(Client, [{packet,0}, {active,once}])
    , inet:setopts(Remote, [{packet,0}, {active,once}])
    , receive
        {_Type, Client, Data} -> ok
            %, io:format("  ~w bytes from client\n", [size(Data)])
            , gen_tcp:send(Remote, Data)
            , relay(Client, Remote, BytesIn + size(Data), BytesOut)
        ; {_Type, Remote, Data} -> ok
            %, io:format("  ~w bytes from inspector\n", [size(Data)])
            , gen_tcp:send(Client, Data)
            , relay(Client, Remote, BytesIn, BytesOut + size(Data))
        ; {tcp_closed, _} -> ok
            , io:format("Relay finished in=~w out=~w\n", [BytesIn, BytesOut])
            , gen_tcp:close(Client)
            , gen_tcp:close(Remote)
            , {ok, BytesIn, BytesOut}
            , ok
        ; Else -> ok
            , ?LOG_ERROR("Relay error: ~p", [Else])
        end
    %, couch_httpd:send_json(Req, 200, {[ {ok,true}, {todo,<<"To do">>} ]})
    .

proxy_to_inspector(Req, Port) -> ok
    , io:format("Connect to port ~w\n", [Port])
    , case gen_tcp:connect("127.0.0.1", Port, [binary, {packet,0}, {delay_send,true}])
        of {ok, InspectorSocket} -> ok
            , io:format("Connected to inspector on :~w\n", [Port])
            , relay(Req, InspectorSocket)
        ; {error, Error} -> ok
            , Resp = {[ {error,inspector_proxy}, {result, Error} ]}
            , couch_httpd:send_json(Req, 502, Resp)
        end
    .

request_to_iolist(#httpd{method=Method, mochi_req=MochiReq}=Req) -> ok
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

%% Login handler with Browser ID.
%handle_id_req(#httpd{method='POST'}=Req) -> ok
%    , case couch_config:get("browserid", "enabled")
%        of "true" -> ok
%            , case couch_config:get("browserid", "audience", undefined)
%                of undefined -> ok
%                    , throw({error, no_browserid_audience})
%                ; Audience -> ok
%                    , handle_id_req(enabled, Audience, Req)
%                end
%        ; _ -> ok
%            % Browserid is disabled in the config.
%            , throw({error, browserid_not_enabled})
%        end
%    ;
%
%handle_id_req(_Req) ->
%    % Send 405
%    not_implemented.
%
%handle_id_req(enabled, Audience, #httpd{method='POST', mochi_req=MochiReq}=Req) ->
%    ReqBody = MochiReq:recv_body(),
%    Form = case MochiReq:get_primary_header_value("content-type") of
%        % content type should be json
%        "application/x-www-form-urlencoded" ++ _ ->
%            mochiweb_util:parse_qs(ReqBody);
%        "application/json" ++ _ ->
%            {Pairs} = ?JSON_DECODE(ReqBody),
%            lists:map(fun({Key, Value}) ->
%              {?b2l(Key), ?b2l(Value)}
%            end, Pairs);
%        _ ->
%            []%couch_httpd:send_json(Req, 406, {error, method_not_allowed})
%    end,
%    Assertion = couch_util:get_value("assertion", Form, ""),
%    case verify_id(Assertion, Audience) of
%    {error, _Reason} ->
%        % Send client an error response, couch_util:send_err ...
%        not_implemented;
%    {ok, Verified_obj} -> ok
%        , send_good_id(Req, Verified_obj)
%    end.


%
% Utilities
%


% vim: sts=4 sw=4 et
