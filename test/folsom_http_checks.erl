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
%%% File:      folsom_http_checks.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% this is a generic module for testing folsom's http endpoint wrappers
%%% the tests here should pass in folsom_webmachine, folsom_cowboy and
%%% any other http wrapper for folsom
%%% @end
%%%------------------------------------------------------------------

-module(folsom_http_checks).

-include_lib("eunit/include/eunit.hrl").

-export([run/0, run_jsonp/0]).

-define(SYSTEM_URL, "http://localhost:5565/_system").
-define(STATISTICS_URL, "http://localhost:5565/_statistics").
-define(MEMORY_URL, "http://localhost:5565/_memory").
-define(BASE_METRICS_URL, "http://localhost:5565/_metrics").
-define(HEALTH_URL, "http://localhost:5565/_health").
-define(PING_URL, "http://localhost:5565/_ping").
-define(PORT_URL, "http://localhost:5565/_port").
-define(PROCESS_URL, "http://localhost:5565/_process").

run() ->
    metrics_checks(),

    system_checks(),
    statistics_checks(),
    memory_checks(),
    ping_checks(),
    health_checks(),
    port_checks(),
    process_checks().

    %create_metric(),
    %populate_metric(),
    %delete_metric().

run_jsonp() ->
    metrics_jsonp_checks(),
    metrics_jsonp_invalid_pad_checks().

metrics_checks() ->
    Body1 = http_helpers:http_get(?BASE_METRICS_URL),
    List1 = fc_mochijson2:decode(Body1),
    true = lists:member(<<"counter">>, List1),
    ?debugFmt("http: ~p~n", [List1]),

    ?debugFmt("erlang: ~p~n", [folsom_metrics:get_metrics()]),

    Url1 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", counter])),
    Body2 = http_helpers:http_get(Url1),
    {struct, List2} = fc_mochijson2:decode(Body2),
    0 = proplists:get_value(<<"value">>, List2),

    Url2 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", gauge])),
    Body3 = http_helpers:http_get(Url2),
    {struct, List3} = fc_mochijson2:decode(Body3),
    2 = proplists:get_value(<<"value">>, List3),

    % check that json padding is not used when disabled
    Url3 = lists:append(io_lib:format("~s~s~p~s", [?BASE_METRICS_URL, "/", gauge, "?jsonp=TestM"])),
    Body4 = http_helpers:http_get(Url3),
    {struct, List4} = fc_mochijson2:decode(Body4),
    2 = proplists:get_value(<<"value">>, List4).

system_checks() ->
    % check _system stats
    Body1 = http_helpers:http_get(?SYSTEM_URL),
    {struct, List1} = fc_mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"allocated_areas">>, 1, List1).

statistics_checks() ->
    % check _statistics stats
    Body1 = http_helpers:http_get(?STATISTICS_URL),
    {struct, List1} = fc_mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"context_switches">>, 1, List1).

memory_checks() ->
    % check _memory stats
    Body1 = http_helpers:http_get(?MEMORY_URL),
    {struct, List1} = fc_mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"total">>, 1, List1).

ping_checks() ->
    % make sure ping works
    Body1 = http_helpers:http_get(?PING_URL),
    {struct, List1} = fc_mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"pong">>, 1, List1).

health_checks() ->
    % make sure ping works
    Body1 = http_helpers:http_get(?HEALTH_URL),
    {struct, List1} = fc_mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"health">>, 1, List1).

process_checks() ->
    % make sure process works
    Body1 = http_helpers:http_get(?PROCESS_URL),
    {struct, List1} = fc_mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"<0.0.0>">>, 1, List1).

port_checks() ->
    % make sure process works
    Body1 = http_helpers:http_get(?PORT_URL),
    {struct, List1} = fc_mochijson2:decode(Body1),

    % verify one of the many keys exist
    true = lists:keymember(<<"#Port<0.1>">>, 1, List1).

create_metric() ->
    Proplist = [
                {id, "http"},
                {type, "counter"}
               ],

    Body = fc_mochijson2:encode(Proplist),
    ok = http_helpers:http_put(?BASE_METRICS_URL, Body),

    ?debugFmt("erlang: ~p~n", [folsom_metrics:get_metrics()]),

    Result = http_helpers:http_get(lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "/http"]))),
    {struct, [{<<"value">>, 0}]} = fc_mochijson2:decode(Result).

populate_metric() ->
    Proplist = [
                {value, [{inc, 1}]}
               ],

    Body = fc_mochijson2:encode(Proplist),
    ok = http_helpers:http_put(lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "/http"])), Body),

    Result = http_helpers:http_get(lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "/http"]))),
    {struct, [{<<"value">>, 1}]} = fc_mochijson2:decode(Result).

delete_metric() ->
    Url1 = lists:append(io_lib:format("~s~s", [?BASE_METRICS_URL, "/http"])),
    ok = http_helpers:http_delete(Url1).

metrics_jsonp_checks() ->
    Raw1 = http_helpers:http_get(?BASE_METRICS_URL++"?jsonp=TestP"),
    ["TestP", Body1] = string:tokens(Raw1, "="),

    List1 = fc_mochijson2:decode(Body1),
    true = lists:member(<<"counter">>, List1),
    ?debugFmt("http: ~p~n", [List1]),

    ?debugFmt("erlang: ~p~n", [folsom_metrics:get_metrics()]),

    Url1 = lists:append(io_lib:format("~s~s~p", [?BASE_METRICS_URL, "/", counter])),
    Body2 = http_helpers:http_get(Url1),
    {struct, List2} = fc_mochijson2:decode(Body2),
    0 = proplists:get_value(<<"value">>, List2),

    Url2 = lists:append(io_lib:format("~s~s~p~s", [?BASE_METRICS_URL, "/", gauge, "?jsonp=TestM"])),
    Raw3 = http_helpers:http_get(Url2),
    ["TestM", Body3] = string:tokens(Raw3, "="),
    {struct, List3} = fc_mochijson2:decode(Body3),
    2 = proplists:get_value(<<"value">>, List3).

metrics_jsonp_invalid_pad_checks() ->
    error = http_helpers:http_get(?BASE_METRICS_URL++"?jsonp=Test{}").

