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
%%% A module to get information from IMDB through imdbapi.org
%%% @end
%%% Created : 12 Jan 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_imdb).
-include("../include/dd_irc.hrl").
-export([movie_info_by_id/1, movie_info/1, info_if_imdb_url/1, scrape/1,
        movie_sort/2, oldest_movie/1, newest_movie/1]).

-define(ImdbUrlPattern, "http://www.imdb.com/title/tt(\\d*)/").

-spec get_proplist_for_title(string() | binary()) -> none | [tuple()].
get_proplist_for_title(Title) when is_binary(Title) ->
    get_proplist_for_title(binary_to_list(Title));
get_proplist_for_title(Title) ->
    case dd_helpers:http_get("http://imdbapi.org/?yg=0&limit=5&q="++Title) of
        {error, _} -> none;
        {ok, {_,_, Raw}} ->
            {array, StructList} = mochijson:decode(Raw),
            StructList
    end.

-spec movie_info(string()) -> none | binary().
movie_info(Name) ->
    Plist = get_proplist_for_title(Name),
    case Plist of
        none -> none;
        _ ->
            movie_line(hd(Plist))
    end.

movie_sort({struct, Movie1}, {struct, Movie2}) ->
    Year1 = proplists:get_value("year", Movie1),
    Year2 = proplists:get_value("year", Movie2),
    Year1 =< Year2.

oldest_movie(Name) ->
    StructList = get_proplist_for_title(Name),
    Sorted = lists:sort(fun movie_sort/2, StructList),
    movie_line(hd(Sorted)).

newest_movie(Name) ->
    StructList = get_proplist_for_title(Name),
    Sorted = lists:sort(fun movie_sort/2, StructList),
    movie_line(hd(lists:reverse(Sorted))).

movie_line({struct, Plist}) ->
    movie_line(Plist);
movie_line(Plist) ->
    Rating = proplists:get_value("rating", Plist),
    Plot   = proplists:get_value("plot_simple", Plist),
    Year   = proplists:get_value("year", Plist),
    Title  = proplists:get_value("title", Plist),
    URL    = proplists:get_value("imdb_url", Plist),
    iolist_to_binary(io_lib:format("~s (~p): [~p/10] < ~s > ~s~n", [Title, Year, Rating, URL, Plot])).
    %%io:format("~s (~p): [~p/10] < ~s > ~s~n", [Title, Year, Rating, URL, Plot]).


-spec get_proplist_for_id(string() | binary()) -> none | [tuple()].
get_proplist_for_id(Id) when is_binary(Id) ->
    get_proplist_for_id(binary_to_list(Id));
get_proplist_for_id(IdStr) ->
    case dd_helpers:http_get("http://imdbapi.org/?id=tt"++IdStr) of
        {error, _} -> none;
        {ok, {_,_, Raw}} ->
            {struct, Plist} = mochijson:decode(Raw),
            Plist
    end.
    
-spec movie_info_by_id(string()) -> none | binary().
movie_info_by_id(Id) ->
    Plist = get_proplist_for_id(Id),
    case Plist of
        none -> none;
        _ -> movie_line(Plist)
    end.

-spec info_if_imdb_url(binary()) -> ok | false.
info_if_imdb_url(URL) ->
    {ok, Regex} = re:compile(?ImdbUrlPattern, [caseless]),
    case re:run(URL, Regex, [{capture, all_but_first, binary}]) of
        {match, [Id]} -> 
            movie_info_by_id(Id);
        _ -> false
    end.

scrape(Title) ->
    Search = edoc_lib:escape_uri(binary_to_list(Title)),
    {ok, {{"HTTP/1.1",200,"OK"}, _, Data}} =
        httpc:request("http://akas.imdb.com/find?tt=on;nm=on;mx=5&q="++Search),
    mochiweb_html:parse(Data), 
    {<<"a">>, [{<<"href">>,TitleUrl}], _} = hd([ X || X <- mochiweb_xpath:execute("//a[starts-with(@href, '/title')]", mochiweb_html:parse(Data)), 
              not lists:keymember(<<"img">>, 1,
                                  element(3, lists:keyfind(<<"a">>, 1, [X]))) ]),
    <<"tt", ID/binary>> = lists:nth(3, binary:split(TitleUrl, <<"/">>, [global])),
    movie_info_by_id(ID).
    
