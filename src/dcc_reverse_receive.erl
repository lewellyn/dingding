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

%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @doc
%%% Reverse DCC send module.
%%% @end
%%% Created :  14 Aug 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dcc_reverse_receive).
-export([download/6]).

%% {ok, Listen} = gen_tcp:listen(0, [Options]),
%% Port = inet:port(Listen).

download(ReplyPid, Args, Filename, Size, Token, DestinationDir) ->
    %% construct the reply:
    %% The receiver can accept the file by opening a listening socket and
    %% responding with the CTCP message:
    %% DCC SEND <filename> <ip> <port> <filesize> <token>

    %% This is identical to the original Reverse DCC message, except
    %% the <ip> and <port> identify the socket where the receiver is
    %% listening. <token> is the same as in the original request, letting
    %% the sender know which request is being accepted.

    %% NEED: a free port
    %%       my own IP where I'm listening
    %%
    %% this might not be great when you have multiple IP addresses.
    %% TODO: make this configurable.
    IP = dd_helpers:get_first_public_ip(),
    MyPublicIp = dd_helpers:int_to_bin(dd_helpers:ip_to_int(IP)),
    {ok, Listen} = gen_tcp:listen(0, [{ip, IP}, binary, {packet, raw}, {active, false}]),
    {ok, Port} = inet:port(Listen),
    BinPort = dd_helpers:int_to_bin(Port),
    io:format("Size: ~p~n",[Size]),
    Reply = iolist_to_binary(["DCC SEND ", Filename, " ", MyPublicIp, " ", BinPort, " ", Size, " ", Token]),
    io:format("Reply: ~p~n",[Reply]),
    dd_connection:ctcp_reply(ReplyPid, Args, Reply),
    {ok, Socket} = gen_tcp:accept(Listen),
    Destination = DestinationDir ++ "/" ++ binary_to_list(Filename),
    dcc_receive:get_file(Socket, Size, Destination).
    

