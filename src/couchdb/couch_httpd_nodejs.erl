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

-module(couch_httpd_nodejs).
-include("couch_db.hrl").

-export([handle_node_req/1]).
-export([relay/2]).

-import(couch_httpd, [header_value/2, send_method_not_allowed/2]).


handle_node_req(#httpd{}=Req) -> ok
    , AppName = "nodejs_couchdb"
    , case couch_os_daemons:get_app_port(AppName)
        of nil -> ok
            , couch_httpd:send_json(Req, 502, {[ {error,no_app}, {name,AppName} ]})
        ; AppPort -> ok
            , handle_node_req(AppPort, Req)
        end
    .

handle_node_req(AppPort, #httpd{mochi_req=MochiReq, path_parts=[Node_js | _Rest]}=Req) -> ok
    % Forward the request to Node.js. TCP relay is preferred for requests for a
    % vhost, because socket.io works. However TCP cannot be used for direct
    % requests (/_nodejs) because the client may re-use the connection for
    % subsequent CouchDB requests, but they would go to Node instead.
    %, io:format("Node req:\n~p\n", [Req])
    , case MochiReq:get_header_value("x-couchdb-vhost-path")
        of undefined -> ok
            , Url = "http://127.0.0.1:" ++ integer_to_list(AppPort)
                    ++ "/" ++ ?b2l(Node_js)
            %, io:format("Manual proxy: ~p\n", [Url])
            , couch_httpd_proxy:handle_proxy_req(Req, ?l2b(Url))
        ; _Found -> ok
            %, io:format("TCP relay: ~p\n", [AppPort])
            , relay(Req, AppPort)
        end
    .

relay(Req, Port) when is_integer(Port) -> ok
    , case gen_tcp:connect("127.0.0.1", Port, [binary, {packet,0}, {active,false}])
        of {ok, AppSocket} -> ok
            %, io:format("Connected to app on :~w\n", [Port])
            , relay(Req, AppSocket)
        ; {error, Error} -> ok
            , Resp = {[ {error,nodejs_proxy}, {result, Error} ]}
            , couch_httpd:send_json(Req, 502, Resp)
        end
    ;

relay(#httpd{mochi_req=MochiReq}=Req, AppSocket) -> ok
    , Client = MochiReq:get(socket)
    , ReqBytes = request_to_iolist(Req)
    , Size = iolist_size(ReqBytes)
    %, io:format("Send ~w byte request to app\n", [Size])
    , ok = gen_tcp:send(AppSocket, ReqBytes)
    , relay(Client, AppSocket, Size, 0)
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
            %, io:format("Client disconnected\n")
            , gen_tcp:close(Remote)
            , {ok, ok}
        ; {tcp_closed, Remote} -> ok
            %, io:format("Remote disconnected\n")
            , gen_tcp:close(Client)
            , {ok, ok}
        ; Else -> ok
            , ?LOG_ERROR("Relay error: ~p", [Else])
        end
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
