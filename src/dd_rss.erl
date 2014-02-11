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
%%% The rss feed generator for dd.
%%% @end
%%% Created : 22 Dec 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_rss).

-export([item_node/3, item_node/4, store/3, create_xml_file/0,
        store_and_publish/3, utcnow/0, is_picture_link/1]).

%% need to create a file with all the rss feed entries as erlang terms.
%% or separate files for each entry?

is_picture_link(URL) ->
    case string:to_lower(filename:extension(URL)) of
        [] -> false;
        ".jpg" -> true;
        ".png" -> true;
        ".gif" -> true;
        ".bmp" -> true;
        _ -> false
    end.

%% test here for urls that are images.
%% create a cdata tag for those in the description tag.
%% <p><img src="url"/></p>   Should be enough to have the image show up
%% in the feed readers.
%% or <media:content url="URL"/> as a subtag of the item tag.
item_node(Title, Desc, URL) ->
    case is_picture_link(URL) of
        false -> item_node(Title, Desc, URL, hd(io_lib:format("~p", [dd_helpers:utc2sec()])));
        true -> picture_node(Title, Desc, URL, hd(io_lib:format("~p", [dd_helpers:utc2sec()])))
    end.


item_node(Title, Desc, URL, Date) ->
    {item, [], [{title, [], [Title]},
                {guid, [{isPermaLink,"false"}], [Date]},
                {description, [], [Desc]},
                {link, [], [URL]}]}.

picture_node(Title, Desc, URL, Date) ->
    {item, [], [{title, [], [Title]},
                {guid, [{isPermaLink,"false"}], [Date]},
                {'media:content', [{url, URL}], []},
                {description, [], [Desc]},
                {link, [], [URL]}]}.

utcnow() ->
    httpd_util:rfc1123_date(erlang:localtime_to_universaltime(erlang:localtime())).

required_channel_items() ->
    [ {title, [], ["You Fancy Languages? link aggregation feed"]},
      {link, [], ["http://yfl.bahmanm.com"]},
      {description, [], ["All the links that are pasted in the channel. The IRC bot named 'dingd1ng' picks them up and creates this feed."]},
      {pubDate, [], [utcnow()]},
      {'atom:link', [{href,"http://users.telenet.be/gertm/yflrss.xml"},{rel,"self"},{type, "application/rss+xml"}], []},
      {generator, [], ["dingding IRC bot: http://fossil.gertm.eu/dingding/"]} ].

rss_term(ItemList) ->
    Items = lists:append(required_channel_items(), ItemList),
    {rss, [{version, "2.0"},{'xmlns:atom',"http://www.w3.org/2005/Atom"},{'xmlns:media',"http://search.yahoo.com/mrss/"}], [{channel, [], Items}]}.

termstorage_file() ->
    code:priv_dir(dd)++"/yflrss.terms".

rss_filename() ->
    code:priv_dir(dd)++"/yflrss.xml".

%% load previous list of items from the local dump
create_xml_file() ->
    io:format("Creating XML file at ~s~n",[rss_filename()]),
    {ok, Terms} = file:consult(termstorage_file()),
    Len = length(Terms),
    case Len > 100 of
        true -> Diff = Len - 100,
                io:format("Truncating terms file~n"),
                NewTerms = lists:sublist(Terms, Diff, Len),
                {ok, Iodev} = file:open(termstorage_file(), [write]),
                [ io:format(Iodev, "~p.~n", [Term]) || Term <- NewTerms ];
        false -> ok
    end,
    TotalRss = rss_term(Terms),
    file:write_file(rss_filename(), xmerl:export_simple([TotalRss], xmerl_xml)).

store_item(Item) ->
    {ok, Iodev} = file:open(termstorage_file(), [append]),
    io:format(Iodev, "~p.~n",[Item]),
    file:close(Iodev).
    
%% unconsult(File, L) ->
%%     {ok, S} = file:open(File, write),
%%     lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L), 
%%     file:close(S).

store(Title, Desc, URL) ->
    Item = item_node(Title, Desc, URL),
    store_item(Item).

store_and_publish(Title, Desc, URL) ->
    store(Title, Desc, URL),
    create_xml_file().
