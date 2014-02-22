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
%%% A module for receiving files through DCC.
%%% @end
%%% Created : 11 Aug 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dcc_receive).
-export([download/5]).
-export([get_file/3]).
%% set up the connection
%% spawn a new process that handles the download
%% trap its exits, when it goes down unexpectedly, close the file handle.
%% (check whether Erlang does that for us or not)

-spec download(Filename :: binary(), IpAddress :: tuple(), Port :: integer(), Size :: integer(), DestinationFolder :: any()) -> ok. %% needs work, not sure of everything yet.
download(Filename, IpAddress, Port, Size, DestinationFolder) ->
    %% io:format("Opening socket to ~p:~p~n",[IpAddress, Port]),
    {ok, Sock} = gen_tcp:connect(IpAddress, Port, [binary, {packet, raw}, {active, false}], 1000),
    Destination = DestinationFolder ++ "/" ++ binary_to_list(Filename),
    get_file(Sock, Size, Destination),
    ok.

%% Client B receives the CTCP, parses it, eventually asks the user for confirmation
%% and connects to the specified ip address and port; the transfer then begins.
%% Client A sends blocks of data (usually 1-2 KB) and at every block awaits confirmation
%% from the Client B, that when receiving a block should reply 4 bytes containing 
%% a positive number specifying the total size of the file received up to that moment.
%% The transmission closes when the last acknowledge is received by Client A.

get_file(Socket, Size, Destination) when is_binary(Size) ->
    get_file(Socket, dd_helpers:bin_to_int(Size), Destination);
get_file(Socket, Size, Destination) ->
    %% io:format("Getting content, opening file ~p~n",[Destination]),
    {ok, IoDevice} = file:open(Destination, [write]),
    %% io:format("IoDevice openend, on to getting the parts!~n"),
    {ok, Size} = get_part(Socket, IoDevice, Size, 0),
    file:close(IoDevice),
    %% io:format("Got file ~s~n",[Filename]),
    ok.
    
get_part(Socket, IoDevice, TotalSize, SizeSoFar) ->
    %% io:format("get_part(socket, iodevice, ~p, ~p).~n",[TotalSize, SizeSoFar]),
    {ok, Packet} = gen_tcp:recv(Socket, 0),
    PacketSize = byte_size(Packet),
    file:write(IoDevice, Packet),
    NewSizeSoFar = SizeSoFar + PacketSize,
    StillToGet = TotalSize - NewSizeSoFar,
    case StillToGet > 0 of
        true ->
            %% io:format("Sending reply, ack for ~p bytes~n",[PacketSize]),
            gen_tcp:send(Socket, list_to_binary(integer_to_list(PacketSize))),
            get_part(Socket, IoDevice, TotalSize, NewSizeSoFar);
        false ->
            gen_tcp:send(Socket, list_to_binary(integer_to_list(PacketSize))),
            {ok, TotalSize}
    end.
