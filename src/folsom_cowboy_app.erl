%%%
%%% Copyright 2011, Boundary
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
%%% File:      folsom_cowboy_app.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

%% @doc Application module for folsom_cowboy.

-module(folsom_cowboy_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(APP, folsom_cowboy).

start(_Type, _Args) ->
    Cb = application:ensure_all_started(cowboy),
    Cbku = application:ensure_all_started(cowboyku),

    case {Cb, Cbku} of
        {{ok, _}, {error, _}} ->
            Router = cowboy_router,
            Cowboy = cowboy;
        {{error, _}, {ok, _}} ->
            Router = cowboyku_router,
            Cowboy = cowboyku;
        _ ->
            Cowboy = Router = bleh,
            exit(no_cowboy_variant)
    end,

    Dispatch = Router:compile(folsom_cowboy:dispatch()),

    Env = {env, [{dispatch, Dispatch}] },

    ProtoOpts =
        case application:get_env(?APP, enable_jsonp, false) of
            true ->
                [Env, {onresponse, fun maybe_add_padding/4}];
            false ->
                [Env]
        end,

    {ok, _Pid} = Cowboy:start_http(folsom_cowboy_listener, env(num_acceptors),
                                   [{port, env(port)}, {ip, env(ip)}], ProtoOpts),

    error_logger:warning_msg("pid ~p", [_Pid]),
    folsom_cowboy_sup:start_link().

stop(_State) ->
    ok.

env(Name) ->
    case application:get_env(?APP, Name) of
        {ok, Val} ->
            Val
    end.

maybe_add_padding(Code, Headers, Body, Req) ->
    {Padding, Req2} = cowboy_req:qs_val(<<"jsonp">>, Req, undefined),

    case Padding of
        undefined ->
            Req2;
        _ ->
            case re:run(Padding, "^\\w+$", [global, notempty, unicode]) of
                nomatch ->
                    Body2 = mochijson2:encode(<<"Bad padding: ", Padding/binary>>),
                    Headers2 = lists:keyreplace(
                                 <<"content-length">>, 1, Headers,
                                 {<<"content-length">>, integer_to_list(iolist_size(Body2))}),
                    {ok, RE} = cowboy_req:reply(400, Headers2, Body2, Req2),
                    RE;
                _ ->
                    Padded = [Padding, "=", Body],

                    Headers2 = lists:keyreplace(
                                 <<"content-length">>, 1, Headers,
                                 {<<"content-length">>, integer_to_list(iolist_size(Padded))}),
                    {ok, Req3} = cowboy_req:reply(Code, Headers2, Padded, Req2),
                    Req3
            end
    end.
