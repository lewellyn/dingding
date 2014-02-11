%% Copyright 2013 Gert Meulyzer

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
%%% Providing translations through the mymemory.translated.net
%%% @end
%%% Created : 12 Feb 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_translation).
-include("../include/dd_irc.hrl").
-export([translate/3]).

-spec translate(string(), binary(), binary()) -> binary() | none.
translate(Phrase, SourceLang, TargetLang) ->
    URL = io_lib:format("http://mymemory.translated.net/api/get?q=~s&langpair=~s|~s",
                        [edoc_lib:escape_uri(binary_to_list(Phrase)),
                         binary_to_list(SourceLang),
                         binary_to_list(TargetLang)]),
    case dd_helpers:http_get(URL) of
        {error, _} -> none;
        {ok, {_, _, Result}} ->
            Resp = mochijson:decode(Result),
            get_translation_from_response(Resp)
    end.

-spec get_translation_from_response(term()) -> binary().
get_translation_from_response(Response) ->
    {struct, [{"responseData",
               {struct, [{"translatedText", Translation}]}},_,_,_]} = Response,
    ?U(Translation).

