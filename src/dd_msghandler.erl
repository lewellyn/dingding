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
%%% Behaviour definition for msghandlers
%%% @end
%%% Created : 29 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_msghandler).
-include("../include/dd_irc.hrl").

%% @doc
%% The callback function that needs to be implemented.
%% Always returns ok, sends back with dd_connection:send_msg(ReplyPid, Prefix, Command, Args, Tail).
%% @end
-callback handle_msg(ReplyPid :: pid(), Prefix :: binary(), Command :: binary(), Arguments :: [binary()], Tail :: binary()) -> ok.
