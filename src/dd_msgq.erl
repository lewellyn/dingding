%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2013, Gert Meulyzer
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
%%% Queuing up messages before sending them back out. This is used for
%%% flood control.
%%% @end
%%% Created : 12 Mar 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_msgq).
-include("../include/dd_irc.hrl").

-export([start/1]).

start(Sock) ->
    loop(Sock, queue:new()).

loop(Sock, Q) ->
    receive
        {add, IrcMsg} ->
            Q2 = queue:in(IrcMsg, Q),
            loop(Sock, Q2)
    after 333 ->
            case queue:out(Q) of
                {empty, Q2} -> loop(Sock, Q2);
                {{value, IrcMsg}, Q2} ->
                    dd_connection:send_ircmsg(Sock, IrcMsg),
                    loop(Sock, Q2)
            end
    end.
