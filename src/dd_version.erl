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
-module(dd_version).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5]).

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
handle_msg(ReplyPid, Prefix, <<"CTCP">>, _Args, <<"VERSION">>) ->
    Nick = dd_ircmsg:nick_from_prefix(Prefix),
    dd_connection:send_msg(ReplyPid, <<>>, <<"CTCP_REPLY">>, [Nick], <<"dingding IRC bot http://fossil.gertm.eu/dingding/ ">> ),
    dd_versioncheck:request_version(ReplyPid, Nick),
    ok;
handle_msg(_ReplyPid, Prefix, <<"CTCP_REPLY">>, _Args, <<"VERSION ", Reply/binary>>) ->
    Nick = dd_ircmsg:nick_from_prefix(Prefix),
    io:format("CtcpReply from ~p: ~p~n",[Nick, Reply]),
    %% store it in the db now.
    dd_sql:store_version(dd_helpers:get_db_pid(), Nick, Reply),
    ok;
handle_msg(_,_,_,_,_) ->
    ok.
