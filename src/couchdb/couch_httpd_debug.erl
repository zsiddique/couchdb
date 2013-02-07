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


handle_debug_req(#httpd{method='GET'}=Req) -> ok
    , ok = couch_httpd:verify_is_server_admin(Req)
    , couch_httpd:send_json(Req, 200, {[ {ok, true} ]})
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
