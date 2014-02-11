%% Copyright 2012-2013 Gert Meulyzer

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
%%% Command interface to the bot.
%%% @end
%%% Created : 11 Nov 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_command_handler).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5]).

-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\def ", SearchString/binary>>) ->
    handle_def_command(ReplyPid, Args, SearchString);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\imdb -new ", SearchString/binary>>) ->
    handle_imdb_command(ReplyPid, Args, SearchString, new);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\imdb -old ", SearchString/binary>>) ->
    handle_imdb_command(ReplyPid, Args, SearchString, old);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\imdb ", SearchString/binary>>) ->
    handle_imdb_command(ReplyPid, Args, SearchString, new);
handle_msg(_ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<",bb 15">>) ->
    dd_connection:reply(self(), Args, <<"This command has been replaced by \\bb.">>);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\bb ", BB/binary>>) ->
    handle_bb(ReplyPid, Args, BB);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\@", UserName/binary>>) ->
    handle_at_username(ReplyPid, Args, UserName);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\numbertrivia ", Nr/binary>>) ->
    handle_numbertrivia(ReplyPid, Args, Nr);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\numberinfo ", Nr/binary>>) ->
    handle_numberinfo(ReplyPid, Args, Nr);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\translate ",Source:2/binary,"->",Target:2/binary," ",Phrase/binary>>) ->
    handle_translation(ReplyPid, Args, Source, Target, Phrase);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\translate ",Source:2/binary,"|",Target:2/binary," ",Phrase/binary>>) ->
    handle_translation(ReplyPid, Args, Source, Target, Phrase);
handle_msg(ReplyPid, Prefix, <<"PRIVMSG">>, Args, <<"\\tweet ",Tweet/binary>>) ->
    handle_tweet(ReplyPid, Prefix, Args, Tweet);
handle_msg(ReplyPid, Prefix, <<"PRIVMSG">>, Args, <<"\\retweet ",Tweet/binary>>) ->
    handle_retweet(ReplyPid, Prefix, Args, Tweet);
handle_msg(_ReplyPid, _Prefix, _Command, _Args, _Tail) ->
    ok.

handle_tweet(ReplyPid, Prefix, Args, Tweet) ->
    Nick = dd_ircmsg:nick_from_prefix(Prefix),
    case dd_twitter:tweet(Nick, Tweet) of
        disallowed -> dd_connection:reply(ReplyPid, Args, <<"You are not allowed to use this.">>);
        ID ->
            dd_connection:reply(ReplyPid, Args, iolist_to_binary(["https://twitter.com/dingd1ng/status/",io_lib:format("~p",[ID])]))
    end.

handle_retweet(_ReplyPid, Prefix, _Args, Tweet) ->
    Nick = dd_ircmsg:nick_from_prefix(Prefix),
    dd_twitter:retweet_from_url(Nick, Tweet),    
    ok.

-spec handle_def_command(pid(), [binary()], binary()) -> ok.
handle_def_command(ReplyPid, Args, SearchString) ->
    case dd_ddg:related_topics(SearchString, 2) of
        [] -> dd_connection:reply(ReplyPid, Args, dd_ddg:definition(SearchString));
        Lst -> [ dd_connection:reply(ReplyPid, Args, X) || X <- Lst ]
    end,
    ok.

-spec handle_imdb_command(pid(), [binary()], binary(), atom()) -> ok.
handle_imdb_command(P, Args, SearchString, old) ->
    case dd_imdb:oldest_movie(binary_to_list(SearchString)) of
        none -> ok;
        Info -> dd_connection:reply(P, Args, unicode:characters_to_binary(Info))
    end,
    ok;
handle_imdb_command(P, Args, SearchString, new) ->
    case dd_imdb:newest_movie(binary_to_list(SearchString)) of
        none -> ok;
        Info -> dd_connection:reply(P, Args, unicode:characters_to_binary(Info))
    end,
    ok;
handle_imdb_command(P, Args, SearchString, _) ->
    Info = dd_imdb:scrape(SearchString),
    dd_connection:reply(P, Args, unicode:characters_to_binary(Info)),
    ok.

-spec handle_bb(pid(), [binary()], binary()) -> ok.                       
handle_bb(P, Args, BB) ->
    Nr = list_to_integer(binary_to_list(BB)),
    case dd_bb:get_post_by_number(Nr) of
        none -> ok;
        R -> dd_connection:reply(P, Args, R)
    end,
    ok.

-spec handle_numbertrivia(pid(), [binary()], binary()) -> ok.                       
handle_numbertrivia(P, Args, Nr) ->
    dd_connection:reply(P, Args, ?U(dd_numbers:trivia(Nr))),
    ok.

-spec handle_numberinfo(pid(), [binary()], binary()) -> ok.                       
handle_numberinfo(P, Args, Nr) ->
    dd_connection:reply(P, Args, ?U(dd_numbers:math(Nr))),
    ok.

-spec handle_translation(pid(), [binary()], binary(), binary(), binary()) -> ok.
handle_translation(P, Args, Source, Target, Phrase) ->
    case dd_translation:translate(Phrase, Source, Target) of
        none -> ok;
        Reply -> dd_connection:reply(P, Args, Reply)
    end,
    ok.

handle_at_username(P, Args, Username) ->
    dd_twitter:handle_twitter_usertimeline(P, Args, Username).
                                
