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
-module(dd_versioncheck).
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

%% if it hasn't been more than...24? hours till last check, don't check again.
handle_msg(ReplyPid, Prefix, <<"JOIN">>, _Args, <<_Rest/binary>>) ->
    Nick = dd_ircmsg:nick_from_prefix(Prefix),
    %% check whether we have a version for this nick.
    case dd_sql:get_version_for_nick(dd_helpers:get_db_pid(), Nick) of
        undefined ->
            dd_helpers:request_version(ReplyPid, Nick);
        {_, Tstamp} ->
            case dd_helpers:diff2now(Tstamp) of
                {0,_} -> ok;
                {1,_} -> ok;
                {2,_} -> ok;
                {_,_} -> dd_helpers:request_version(ReplyPid, Nick)
            end
    end,
    ok;
handle_msg(_,_,_,_,_) ->
    ok.
