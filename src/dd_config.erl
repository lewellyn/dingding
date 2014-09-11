%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%% Copyright 2012, 2013 Gert Meulyzer

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
%%% The configuration module for dd.
%%% @end
%%% Created : 19 Jun 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_config).
-include("../include/dd_irc.hrl").

-export([load_config/0]).


%% config should look like this:
%% {server, "Freenode", "irc.freenode.net", 6667, "dingd2ng", [ "#yfb", "#testerlounge" ], [dd_morning, dd_version, dd_url_handler, dd_le_handler, dd_command_handler] }.
%% we want to convert this to:
%% {server, "Freenode", "irc.freenode.net", 6667, "dingd2ng", [{"#yfb", [dd_morning, dd_version, dd_url_handler, dd_le_handler, dd_command_handler]},
%%                                                             {"#testerlounge", [dd_morning, dd_version, dd_url_handler, dd_le_handler, dd_command_handler]}]}.

load_config() ->
    {ok, ServerConfigs} = application:get_env(dd, serverconfigs),
    [ #serverconfig{name=Name, hostname=HostName, port=Port, nick=Nick, pass=Pass, sasl=Sasl, nickserv=NickServ, ssl=Ssl, channels=Channels, modules=Modules}
                    || {server, Name, HostName, Port, Nick, Pass, Sasl, NickServ, Ssl, Channels, Modules} <- ServerConfigs ].   
