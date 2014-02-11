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
%%% A little module to deal with Freenode specific stuff.
%%% @end
%%% Created : 10 Jun 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_freenode).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5, login_to_nickserv/2, nick_is_logged_in_as/2]).

%%--------------------------------------------------------------------
%% @doc
%% This will crash when the freenode config isn't present.
%% It's not a big deal, the bot just won't be logged in.
%% @end
%%--------------------------------------------------------------------
-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(ReplyPid, _Prefix, <<"001">>, _Args, _Tail) ->
    FreenodeConfig = dd_helpers:consult_priv_dir_file("freenode.cfg"),
    case proplists:get_value(nickservpass, FreenodeConfig, undefined) of
        undefined -> io:format("Couldn't read freenode config!~n");
        Pass -> login_to_nickserv(ReplyPid, Pass)
    end,
    ok;
handle_msg(_ReplyPid, _Prefix, <<"330">>, [_, Nick, Login], <<"is logged in as">>) ->
    ets:insert(logindata, {Nick, freenode, Login}),
    ok;
handle_msg(ReplyPid, Prefix, <<"JOIN">>, _Args, <<_Rest/binary>>) ->
    Nick = dd_ircmsg:nick_from_prefix(Prefix),
    dd_helpers:request_whois(ReplyPid, Nick);
handle_msg(_ReplyPid, _Prefix, _Command, _Args, _Tail) ->
    ok.

login_to_nickserv(ReplyPid, Pwd) ->
    io:format("Sending login information to NickServ."),
    dd_connection:reply(ReplyPid, [<<"NickServ">>], iolist_to_binary(["identify ", Pwd])).

-spec nick_is_logged_in_as(pid(), binary()) -> binary() | atom().
nick_is_logged_in_as(ReplyPid, Nick) ->
    case ets:match(logindata, {Nick, freenode, '$1'}) of
        [] -> dd_helpers:request_whois(ReplyPid, Nick),
              unknown;
        [[Login]] -> Login
    end.


%% REPLY FOR A WHOIS ON FREENODE:
%% {ircmsg,<<"pratchett.freenode.net">>,<<"311">>,
%%         [<<"dingd2ng">>,<<"Gertm">>,<<"~Gertm">>,<<"gertm.eu">>,<<"*">>],
%%         <<"Gert M.">>}
%% {ircmsg,<<"pratchett.freenode.net">>,<<"319">>,
%%         [<<"dingd2ng">>,<<"Gertm">>],
%%         <<"#yfb">>}
%% {ircmsg,<<"pratchett.freenode.net">>,<<"312">>,
%%         [<<"dingd2ng">>,<<"Gertm">>,<<"pratchett.freenode.net">>],
%%         <<"Rennes, France">>}
%% {ircmsg,<<"pratchett.freenode.net">>,<<"671">>,
%%         [<<"dingd2ng">>,<<"Gertm">>],
%%         <<"is using a secure connection">>}
%% {ircmsg,<<"pratchett.freenode.net">>,<<"317">>,
%%         [<<"dingd2ng">>,<<"Gertm">>,<<"13">>,<<"1375405601">>],
%%         <<"seconds idle, signon time">>}
%% {ircmsg,<<"pratchett.freenode.net">>,<<"330">>,
%%         [<<"dingd2ng">>,<<"Gertm">>,<<"Gertm">>],
%%         <<"is logged in as">>}
%% {ircmsg,<<"pratchett.freenode.net">>,<<"318">>,
%%         [<<"dingd2ng">>,<<"Gertm">>],
%%         <<"End of /WHOIS list.">>}
