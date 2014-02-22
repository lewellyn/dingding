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
%%% Handles the module dispatching.
%%% This gets called solely from inside 
%%% @end
%%% Created :  8 Mar 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_modules).
-include("../include/dd_irc.hrl").

-export([add_module/2, remove_module/2, show_modules/1]).
-export([dispatch/3]).

add_module(ConnectionPid, ModuleName) ->
    gen_server:cast(ConnectionPid, {add_module, ModuleName}).
remove_module(ConnectionPid, ModuleName) ->
    gen_server:cast(ConnectionPid, {remove_module, ModuleName}).
show_modules(ConnectionPid) ->
    gen_server:cast(ConnectionPid, show_modules).


dispatch(#ircmsg{prefix=P, command=C, arguments=A, tail=T}=Msg, Modules, Channels) ->
    %% first we do the server modules.
    ReplyPid = self(),
    [ spawn(fun() ->
                    Module:handle_msg(ReplyPid, P, C, A, T)
            end) 
      || Module <- Modules ],
    [ run_chan_mods_on_msg(ReplyPid, Msg, Channel) || Channel <- Channels ],
    ok.

run_chan_mods_on_msg(ReplyPid, #ircmsg{prefix=P, command=C, arguments=A, tail=T}=_Msg, {Channel, ModList}) ->
    BinChan = list_to_binary(Channel),
    case A of
        [BinChan] ->
            [ spawn(fun() -> Module:handle_msg(ReplyPid, P, C, A, T) end) || Module <- ModList ];
        _ -> ok
    end,
    ok;
run_chan_mods_on_msg(_, _, _Channel) ->
    ok.

