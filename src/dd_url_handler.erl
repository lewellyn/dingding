                                                % Copyright 2012 Gert Meulyzer

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

%%% @doc
%%% Example module implementation
%%% @end
%%% Created :  2 Jul 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_url_handler).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5]).

%% debug exports for testing
-export([type_and_size/1, get_page_title/1]).
-export([tinyurl/1]).

%% @doc
%% Prefix is the part that contains the nickname and host on most messages
%% Especially PRIVMSG etc.
%%
%% Command is the type of message it is. CTCP messages get converted from PRIVMSG to
%% CTCP and CTCP_REPLY for easy matching
%%
%% Args is the list of arguments that is supplied. For example the channel to which the message
%% has been sent.
%%
%% Tail is the actual message. The things people type in IRC are here.
%% @end
-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(_ReplyPid, <<"yftcl!",_/binary>>, <<"PRIVMSG">>, [<<"#yfl">>], _) ->
    ok;
handle_msg(ReplyPid, Prefix, <<"PRIVMSG">>, Args, Tail) ->
    handle_urls(ReplyPid, Prefix, Args, Tail);
handle_msg(ReplyPid, Prefix, <<"CTCP">>, Args, Tail) ->
    handle_urls(ReplyPid, Prefix, Args, Tail);
handle_msg(_ReplyPid, _Prefix, _Command, _Args, _Tail) ->
    ok.

-spec handle_urls(pid(), binary(), [binary()], binary()) -> ok.
handle_urls(ReplyPid, Prefix, Args, Tail) ->
    [ process_url(ReplyPid, Prefix, Args, URL) || URL <- dd_helpers:get_urls(Tail) ],
    ok.

-spec process_url(pid(), binary(), [binary()], binary()) -> ok.
process_url(P, _Prefix, Args, URL) ->
    %% first of all, get the title, we need it everywhere else anyway.
    Title = get_page_title(URL),
    %% dd_sql:store_url(dd_db, dd_ircmsg:nick_from_prefix(Prefix), URL, Title),
    case byte_size(URL) > 100 of
        true -> dd_connection:reply(P, Args, tinyurl(URL));
        _ -> ok
    end,
    %% spawn(fun() -> export_xml_for_url(URL, dd_ircmsg:nick_from_prefix(Prefix), Title) end),
    spawn(fun() -> do_url_funs(P, Args, URL, Title) end),
    ok.

-spec do_url_funs(pid(), [binary()], binary(), binary()) -> ok.
do_url_funs(P, Args, URL, Title) ->
    %% Functions used here need to take 1 argument and return a string/binary or an atom.
    %% atom means they failed, string/binary is the result that needs to be sent to IRC.
    %% this goes on until the first one fails, or rather, the first one that returns a
    %% reply to IRC. This is to prevent spamming the channel.
    %% (we don't need the title of the page if we already got the imdb/twitter content)
    Funs = [ fun dd_twitter:reply_if_single_tweet/1, fun dd_imdb:info_if_imdb_url/1 ],
    Results = lists:takewhile(
                fun(Fun) ->
                        case Fun(URL) of
                            Result when is_atom(Result) -> true;
                            Result when is_binary(Result) ->
                                dd_connection:reply(P, Args, Result),
                                false;
                            Result ->
                                dd_connection:reply(P, Args, unicode:characters_to_binary(Result)),
                                false
                        end
                end, Funs),
    case length(Funs) == length(Results) of
        true ->
            dd_connection:reply(P, Args, Title);
        false ->
            ok
    end,
    ok.

%% fill up the ets table msgHandlers with for example:
%% ets:insert(myTable, {<<"blabla">>, {module, func}}).
%% what way we can make automatic handlers.
%% load and compile modules on the fly?
-spec tinyurl(Url :: binary()) -> binary().
tinyurl(Url) when is_binary(Url) ->
    %% http://tinyurl.com/api-create.php?url=http://scripting.com/
    case dd_helpers:http_get("http://tinyurl.com/api-create.php?url="++edoc_lib:escape_uri(binary_to_list(Url))) of
        {error, _} -> ok;
        {ok, {_, _, TinyUrl}} -> list_to_binary(TinyUrl)
    end;
tinyurl(Url) ->
    case dd_helpers:http_get("http://tinyurl.com/api-create.php?url="++edoc_lib:escape_uri(Url)) of
        {error, _} -> ok;
        {ok, {_, _, TinyUrl}} -> TinyUrl
    end.

%% export_xml_for_url(URL, NickName, Title) ->
%%     Nick = binary_to_list(NickName),
%%     dd_rss:store_and_publish(case Title of
%%                                  none -> binary_to_list(URL);
%%                                  ok -> binary_to_list(URL);
%%                                  <<"Picture">> -> binary_to_list(URL);
%%                                  <<"Video">> -> binary_to_list(URL);
%%                                  T -> binary_to_list(T)
%%                              end,
%%                              case Title of
%%                                  <<"Picture">> ->
%%                                      AltTitle = "Posted by "++Nick++" in #YFL on "++dd_rss:utcnow(),
%%                                      "<img src=\""++binary_to_list(URL)++
%%                                          "\" alt=\""++AltTitle++"\" title=\""++AltTitle++"\" />";
%%                                  <<"Video">> -> "<video src=\""++binary_to_list(URL)++"\" controls/>";
%%                                  _ -> "Posted by "++Nick++" in #YFL on "++dd_rss:utcnow()
%%                              end,
%%                              binary_to_list(URL)),
%%     ok.

type_and_size(Url) ->
    case string:rstr(Url, ".webm") > 0 of
        true -> {video, 1};
        false ->
            Resp = dd_helpers:http_head(Url),
            case Resp of
                {ok, {_, Headers, _}} -> {proplists:get_value("content-type", Headers),
                                          proplists:get_value("content-length", Headers)};
                _ -> {undefined, undefined}
            end
    end.

-spec get_page_title(string() | binary()) -> binary() | none.
get_page_title(<<"http://192.168.", _/binary>>) ->
    none;
get_page_title(<<"https://192.168.", _/binary>>) ->
    none;
get_page_title(<<"http://localhost", _/binary>>) ->
    none;
get_page_title(<<"https://localhost", _/binary>>) ->
    none;
get_page_title(<<"http://127.0.0.1", _/binary>>) ->
    none;
get_page_title(<<"https://127.0.0.1", _/binary>>) ->
    none;
get_page_title(<<"http://10.", _/binary>>) ->
    none;
get_page_title(<<"https://10.", _/binary>>) ->
    none;
%% I know, lazy. I'll make a better 'is_private_ip' function later.
get_page_title(<<"http://172.", _/binary>>) ->
    none;
get_page_title(<<"https://172.", _/binary>>) ->
    none;
get_page_title(Url) when is_binary(Url) ->
    get_page_title(binary_to_list(Url));
get_page_title(TheUrl) ->
    Url = hd(string:tokens(TheUrl, "#")),
    {T, _} = type_and_size(Url),
    Type = string:tokens(T,"/"),
    case Type of
        ["text", _] ->
            Resp = dd_helpers:http_get(Url),
            case Resp of
                {ok, {_, _, Contents}} ->
                    {<<"html">>,_,Tags} = mochiweb_html:parse(Contents),
                    [{<<"head">>,_,HeadTags}] =
                        lists:filter(
                          fun(X) -> case X of
                                        {<<"head">>,_,_} -> true;
                                        _ -> false
                                    end end, Tags),
                    [{<<"title">>,_,TitleList}] =
                        lists:filter(
                          fun(X) -> case X of
                                        {<<"title">>,_,_} -> true;
                                        _ -> false
                                    end end, HeadTags),
                    io:format("~p~n",[TitleList]),
                    dd_helpers:trim_ws(hd(TitleList));
                _ -> none
            end;
        ["image", _] ->
            none;
        ["video", _] ->
            none;
        ["application", "pdf"] ->
            dd_pdf:get_pdf_info(list_to_binary(Url));
        [] -> none
    end.

