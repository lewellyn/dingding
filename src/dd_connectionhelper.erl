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
%%% @author Gert <@G3rtm on Twitter>
%%% @doc
%%% Helper module to check for connection timeouts.
%%% @end
%%% Created : 29 Feb 2012 by Gert <@G3rtm on Twitter>

-module(dd_connectionhelper).

-export([ping_server/2, start/2]).


start(Sock, Pid) ->
    receive
    after 15000 ->
            ping_server(Sock, Pid)
    end.

ping_server(Sock, Pid) ->
    gen_server:cast(Pid, ping_server),
    receive
        pong ->
            receive
            after 56000 ->
                    ping_server(Sock, Pid)
            end
        after 57000 ->
                gen_server:cast(Pid, no_pong)
        end.
            
