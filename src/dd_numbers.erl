%% Copyright 2012 Gert Meulyzer
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @doc
%%% A little module for the numbers api on numbersapi.com
%%% @end
%%% Created : 13 Jan 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_numbers).
-include("../include/dd_irc.hrl").
-export([trivia/1, math/1]).

trivia(Nr) when is_binary(Nr) ->
    trivia(binary_to_list(Nr));
trivia(Nr) ->
    case dd_helpers:http_get("http://numbersapi.com/"++Nr) of
        {error, _} -> none;
        {ok, {_,_, Result}} ->
            Result
    end.

math(Nr) when is_binary(Nr) ->
    math(binary_to_list(Nr));
math(Nr) ->
    case dd_helpers:http_get("http://numbersapi.com/"++Nr++"/math") of
        {error, _} -> none;
        {ok, {_,_, Result}} ->
            Result
    end.

