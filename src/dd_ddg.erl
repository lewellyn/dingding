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
%% limitations under the License
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @doc
%%% Module for interfacing with DuckDuckGo
%%% @end
%%% Created : 16 Nov 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_ddg).
-include("../include/dd_irc.hrl").

%% debug exports
-export([definition/1, related_topics/2]).

%% various stuff I'll need later:
%% edoc_lib:escape_uri/1
%% http://api.duckduckgo.com/?q=<STRING>&format=json
%% mochijson:decode(STRING)
%%

-spec definition(string() | binary()) -> binary().
definition(SearchString) when is_binary(SearchString) ->
    definition(binary_to_list(SearchString));
definition(SearchString) ->
    case dd_helpers:http_get("http://api.duckduckgo.com/?format=json&q="++edoc_lib:escape_uri(SearchString)) of
        {error, _} -> none;
        {ok, {_,_, Result}} ->
            {struct, PL} = mochijson:decode(Result),
            Def = proplists:get_value("Definition", PL),
            Source = proplists:get_value("DefinitionSource", PL),
            case {Def, Source} of
                {"", ""} -> iolist_to_binary(["Cannot find definition for ", SearchString]);
                {_, ""} -> iolist_to_binary(["{", SearchString, ", \"", Def, "\"}"]);
                _ -> iolist_to_binary(["{", SearchString, ", \"", Def, " (from ", Source, ")", "\"}"])
            end
    end.

-spec related_topics(string() | binary(), integer()) -> 'none' | binary().
related_topics(Str, Count) when is_binary(Str) ->
    related_topics(binary_to_list(Str), Count);
related_topics(Str, Count) ->
    case dd_helpers:http_get("http://api.duckduckgo.com/?format=json&q="++edoc_lib:escape_uri(Str)) of
        {error, _} -> none;
        {ok, {_,_, Result}} ->
            {struct, PL} = mochijson:decode(Result),
            {array, Lst} = proplists:get_value("RelatedTopics", PL),
            [ iolist_to_binary(["{", Str, ", \"", ?U(Res),"\"}"]) || Res <- 
            lists:sublist(
              lists:takewhile(fun(X) -> X =/= undefined end,
                              [ proplists:get_value("Text", X) || {struct,X} <- Lst]), 
              Count) ]
    end.
