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
%%% Auto-voice people who join this channel.
%%% @end
%%% Created : 17 Jun 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_voicejoin).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5]).

-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(ReplyPid, Prefix, <<"JOIN">>, Args, _) ->
    Nick = dd_ircmsg:nick_from_prefix(Prefix), 
    case Nick of
        <<"Erlang">> -> ok;
        <<"yftcl">> -> ok;
		<<"clue">> -> ok;
        <<"dingd1ng">> -> ok;
        _ -> dd_connection:send_msg(ReplyPid,<<>>,<<"PRIVMSG">>,[<<"ChanServ">>], iolist_to_binary(["VOICE ",Args," ",Nick]))
    end;
handle_msg(_, _, _, _, _) -> ok.


