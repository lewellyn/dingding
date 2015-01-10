%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
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
%%%-------------------------------------------------------------------
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @doc
%%% attempt at IRC client in a gen_server
%%% @end
%%% Created : 28 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(dd_connection).

-behaviour(gen_server).
-include("../include/dd_irc.hrl").

-define(COLON, 58).
-define(NEWLINE, "\r\n").
-define(VERBOSE, yes).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([send_msg/2, send_msg/5, reply/3, ctcp_reply/3, send_raw/2, handle/2, handle_numeric_reply/3, add_module/2, remove_module/2, show_modules/1]).
-export([add_module_to_channel/3, remove_module_from_channel/3, show_modules_for_channel/2]).
-export([send_ircmsg/2]).

-define(SERVER, ?MODULE).

-record(state, {socket,
                serverconfig=#serverconfig{},
                connectionhelper=undefined,
                msgq=undefined
               }).
-type serverconfig() :: #serverconfig{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec send_msg(Pid :: pid(), Msg :: #ircmsg{}) -> ok.
send_msg(Pid, Msg) ->
    gen_server:cast(Pid, {send_msg, Msg}).
-spec send_msg(Pid :: pid(), Prefix :: binary(), Command :: binary(), Arguments :: [binary()], Tail :: binary()) -> ok.
send_msg(Pid, Prefix, Command, Arguments, Tail) ->
    send_msg(Pid, dd_ircmsg:create(Prefix, Command, Arguments, Tail)).
-spec reply(Pid :: pid(), Arguments :: [binary()], Tail :: binary()) -> ok.
reply(Pid, Arguments, Tail) ->
    send_msg(Pid, <<>>, <<"PRIVMSG">>, Arguments, Tail).
-spec ctcp_reply(Pid :: pid(), Arguments :: [binary()], Tail :: binary()) -> ok.
ctcp_reply(Pid, Arguments, Tail) ->
    send_msg(Pid, <<>>, <<"CTCP">>, Arguments, <<Tail/binary>>).

-spec add_module(pid(), atom()) -> ok.
add_module(Pid, ModuleName) ->
    gen_server:cast(Pid, {add_module, ModuleName}).
-spec remove_module(pid(), atom()) -> ok.
remove_module(Pid, ModuleName) ->
    gen_server:cast(Pid, {remove_module, ModuleName}).
-spec show_modules(pid()) -> ok.
show_modules(Pid) ->
    gen_server:cast(Pid, show_modules).

-spec add_module_to_channel(pid(), atom(), string()) -> ok.
add_module_to_channel(Pid, ModuleName, Channel) ->
    gen_server:cast(Pid, {add_module_to_channel, ModuleName, Channel}).

-spec remove_module_from_channel(pid(), atom(), string()) -> ok.
remove_module_from_channel(Pid, ModuleName, Channel) ->
    gen_server:cast(Pid, {remove_module_from_channel, ModuleName, Channel}).

-spec show_modules_for_channel(pid(), string()) -> ok.
show_modules_for_channel(Pid, Channel) ->
    gen_server:cast(Pid, {show_modules_for_channel, Channel}).

send_raw(Pid, Line) ->
    gen_server:cast(Pid, {send_raw, Line}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(#ircmsg{}) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(#serverconfig{}=Config) ->
    gen_server:start_link({local, list_to_atom(Config#serverconfig.name)}, ?MODULE, [Config], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([#serverconfig{}=Cfg]) ->
    %% construct the botstate here.
    {ok, #state{serverconfig=Cfg}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add_module, ModuleName}, #state{serverconfig=ServerConfig}=State) ->
    case dd_helpers:module_exists(ModuleName) of
        true -> 
            CurrentModules = ServerConfig#serverconfig.modules,
            NewModules = [ModuleName|CurrentModules],
            NewServerConfig = ServerConfig#serverconfig{modules=NewModules},
            NewState = State#state{serverconfig=NewServerConfig},
            io:format("Added module ~s to ~p~n",[ModuleName, ServerConfig#serverconfig.name]),
            {noreply, NewState};
        false -> 
            {noreply, State}
    end;
handle_cast({remove_module, ModuleName}, #state{serverconfig=ServerConfig}=State) ->
    case dd_helpers:module_exists(ModuleName) of
        true -> 
            CurrentModules = ServerConfig#serverconfig.modules,
            NewModules = lists:delete(ModuleName, CurrentModules),
            NewServerConfig = ServerConfig#serverconfig{modules=NewModules},
            NewState = State#state{serverconfig=NewServerConfig},
            io:format("Removed module ~s from ~p~n",[ModuleName, ServerConfig#serverconfig.name]),
            {noreply, NewState};
        false -> 
            {noreply, State}
    end;
handle_cast(show_modules, #state{serverconfig=ServerConfig}=State) ->
    CurrentModules = ServerConfig#serverconfig.modules,
    io:format("Current modules active for: ~p:~n~p~n",[ServerConfig#serverconfig.name, CurrentModules]),
    {noreply, State};
handle_cast({add_module_to_channel, ModuleName, Channel}, #state{serverconfig=#serverconfig{}=ServerConfig}=State) ->
    NewServerConfig = addm2c(ModuleName, Channel, ServerConfig),
    {noreply, State#state{serverconfig=NewServerConfig}};
handle_cast({remove_module_from_channel, ModuleName, Channel}, #state{serverconfig=#serverconfig{}=ServerConfig}=State) ->
    NewServerConfig = remmod4chan(ModuleName, Channel, ServerConfig),
    {noreply, State#state{serverconfig=NewServerConfig}};
handle_cast({show_modules_for_channel, Channel}, #state{serverconfig=#serverconfig{channels=Channels}}=State) ->
    ChanMods = proplists:get_value(Channel, Channels),
    io:format("~p~n",[ChanMods]),
    {noreply, State};
handle_cast({send_raw, Line}, #state{socket=Sock}=State) ->
    send_rawmsg(Sock, Line),
    {noreply, State};
handle_cast({send_msg, Msg}, #state{msgq=Q}=State) ->
    Q ! {add, Msg},
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(ping_server, State) ->
    gen_server:cast(self(), {send_raw, <<"PING :DingBotConnCheck">>}),
    {noreply, State};
handle_cast(got_pong, #state{connectionhelper=C}=State) ->
    C ! pong,
    {noreply, State};
handle_cast(no_pong, State) ->
    io:format("Disconnected apparently... letting the bot crash..~n"),
    {stop, disconnected, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do appropriate pre-registration login.
%%
%% @spec do_server_login(Sock, UserName, Pass, Sasl, NickServ) ->
%%                                   void()
%% @end
%%--------------------------------------------------------------------
do_server_login(Sock, _, true, _) ->
    %% Do SASL Authentication if requested.
    %% https://github.com/atheme/charybdis/blob/master/doc/sasl.txt
    io:format("Starting SASL authentication.~n"),
    gen_tcp:send(Sock, "CAP REQ :sasl"),
    gen_tcp:send(Sock, ?NEWLINE);

do_server_login(Sock, Pass, false, false) ->
    %% Do Password login if no SASL or NickServ login.
    io:format("Doing server password authentication.~n"),
    gen_tcp:send(Sock, "PASS "),
    gen_tcp:send(Sock, Pass),
    gen_tcp:send(Sock, ?NEWLINE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Do appropriate post-registration login.
%%
%% @spec do_final_login(Sock, UserName, Pass, Sasl, NickServ) ->
%%                                   void()
%% @end
%%--------------------------------------------------------------------
do_final_login(Sock, UserName, Pass, true, _) ->
    %% SASL
    io:format("Finishing SASL authentication.~n"),
    gen_tcp:send(Sock, "AUTHENTICATE PLAIN"),
    gen_tcp:send(Sock, ?NEWLINE),
    gen_tcp:send(Sock, "AUTHENTICATE "),
    %% TODO: SASL uses the *account* name, not the nick. Assuming for now that they're the same.
    gen_tcp:send(Sock, base64:encode_to_string(UserName++"\0"++UserName++"\0"++Pass)),
    gen_tcp:send(Sock, ?NEWLINE),
    %% Here, we need to wait until authentication is complete. Timeout is arbitrary.
    %% TODO: Since I've not gotten that far, I'm just sleeping. :P
    timer:sleep(10000),
    gen_tcp:send(Sock, "CAP END"),
    gen_tcp:send(Sock, ?NEWLINE);

do_final_login(Sock, UserName, Pass, false, true) ->
    %% NickServ login
    io:format("Doing NickServ password authentication.~n"),
    gen_tcp:send(Sock, "PRIVMSG NickServ :IDENTIFY "++UserName++" "++Pass++?NEWLINE),
    %% Give NickServ a few (arbitrary) seconds to identify before joining channels.
    timer:sleep(10000);

do_final_login(Sock, _, _, _, _) ->
    gen_tcp:send(Sock, ?NEWLINE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{serverconfig=#serverconfig{name=Network, hostname=Host, port=Port, nick=UserName, pass=Pass, sasl=Sasl, nickserv=NickServ, ssl=Ssl}=ServerCfg}) ->    
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, once}]),

    %% Do the IRC login
    io:format("Logging into ~s (~s:~B). SSL: ~s~n", [Network, Host, Port, Ssl]),
    if
        Pass == false ->
            io:format("No server authentication requested.~n");
        true ->
            do_server_login(Sock, Pass, Sasl, NickServ)
    end,
    gen_tcp:send(Sock, "NICK "++UserName),
    gen_tcp:send(Sock, "\r\n"),
    gen_tcp:send(Sock, "USER "++UserName++" "++UserName++" "++UserName++" "++UserName),
    gen_tcp:send(Sock, "\r\n"),
    if
        Pass == false ->
            io:format("No final server authentication required.~n");
        true ->
            do_final_login(Sock, UserName, Pass, Sasl, NickServ)
    end,
    io:format("Connected to ~s as ~s.~n", [Network, UserName]),

    %% spawn the helper process to keep pinging the server.
    %% needs to be its own module probably.
    ConHelpPid = spawn_link(dd_connectionhelper, start, [Sock, self()]),
    MsgBuff = spawn_link(dd_msgq, start, [Sock]),
    inet:setopts(Sock, [{active, once}]),
    %% initialize the db registered name if it hasn't been already.
    %% dd_helpers:get_db_pid(),
    {noreply, #state{socket=Sock, serverconfig=ServerCfg, connectionhelper=ConHelpPid, msgq=MsgBuff}};
handle_info({tcp, _S, Data}, #state{socket=Sock, msgq=Q}=State) ->
    Msg = dd_ircmsg:parse_line(Data),
    {Response, NewState} = ?MODULE:handle(Msg, State),
    case Response of
        ok -> ok;
        _ -> Q ! {add, Response}
    end,
    inet:setopts(Sock, [{active, once}]),
    {noreply, NewState};
handle_info({tcp_closed, _Port}, State) ->
    io:format("DISCONNECTED!!!!"),
    {stop, disconnected, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    io:format("TCP ERROR! ~p~n", [Reason]),
    {stop, error, State};
handle_info(Info, State) ->
    io:format("UNKNOWN: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_ircmsg(_Sock, ok) ->
    ok;
send_ircmsg(Sock, Msg) ->
    Line = dd_ircmsg:to_line(Msg),
    gen_tcp:send(Sock, iolist_to_binary([Line, <<"\r\n">>])).

send_rawmsg(Sock, Line) ->
    gen_tcp:send(Sock, [Line, "\r\n"]).

-spec addm2c(atom(), string(), serverconfig()) -> serverconfig().
addm2c(ModuleName, Channel, ServerConfig) ->
    case dd_helpers:module_exists(ModuleName) of
        true -> 
            Channels = ServerConfig#serverconfig.channels,
            NewChannels = dd_helpers:add_item_to_proplist_value(ModuleName, Channels, Channel),
            ServerConfig#serverconfig{channels=NewChannels};
        false ->
            ServerConfig
    end.

-spec remmod4chan(atom(), string(), serverconfig()) -> serverconfig().
remmod4chan(ModuleName, Channel, ServerConfig) ->
    Channels = ServerConfig#serverconfig.channels,
    NewChannels = dd_helpers:remove_item_from_proplist_value(ModuleName, Channels, Channel),
    ServerConfig#serverconfig{channels=NewChannels}.

%%%===================================================================
%%% Main dispatcher
%%%===================================================================

-spec handle(#ircmsg{}, #state{}) -> {#ircmsg{}, #state{}} | {ok, #state{}}.
handle(#ircmsg{command= <<"PING">>, tail=T}=_Msg, #state{}=State) ->
    {dd_ircmsg:create(<<>>, <<"PONG">>, [], T), State};
handle(#ircmsg{prefix=_P, command= <<"PONG">>, arguments=_A, tail=_T}=_Msg, #state{}=State) ->
    gen_server:cast(self(), got_pong),
    {ok, State};
handle(#ircmsg{prefix=__P, command = <<"001">>, arguments=__A, tail=__T}=Msg, #state{serverconfig=#serverconfig{modules=Modules, channels=Channels}}=State) ->
    join_channels(State),
    dd_modules:dispatch(Msg, Modules, Channels),
    {ok, State};
handle(#ircmsg{prefix=_P, command=_C, arguments=_A, tail=_T}=Msg, #state{serverconfig=#serverconfig{modules=Modules, channels=Channels}}=State) ->
    %% io:format("~p~n",[Msg]),
    dd_modules:dispatch(Msg, Modules, Channels),
    {ok, State}.

join_channels(#state{serverconfig=#serverconfig{channels=Channels}}=State) ->
    io:format("Joining channels!~n"),
    [ case Channel of
            {X, _} -> 
                gen_server:cast(self(), {send_raw, iolist_to_binary([<<"JOIN ">>, X])});
            X ->  
                gen_server:cast(self(), {send_raw, iolist_to_binary([<<"JOIN ">>, X])}) 
        end || Channel <- Channels ],
    spawn(dd_twitter, periodic_get_mentions, []),
    {ok, State}.

%% @doc
%% These handlers are still here for my personal documentation.
%% They will be removed in the future. No use in having them in one single
%% place if multiple modules would need them.
%%%===================================================================
%%% Numeric handlers. See: http://www.irchelp.org/irchelp/rfc/rfc2812.txt
%%%===================================================================
%%% Numerics in the range from 001 to 099 are used for client-server
%%% connections only and should never travel between servers.  Replies
%%% generated in the response to commands are found in the range from 200
%%% to 399.
%%%===================================================================
%% @end
-spec handle_numeric_reply(Nr :: integer(), Msg :: #ircmsg{}, #state{}) -> {#ircmsg{}, #state{}} | {ok, #state{}}.

%% @doc
%%% The server sends Replies 001 to 004 to a user upon
%%% successful registration.
%% RPL_WELCOME
%% @end
handle_numeric_reply(001, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_WELCOME not implemented.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_
%% @end
handle_numeric_reply(002, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ not implemented.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_CREATED
%% @end
handle_numeric_reply(003, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_CREATED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_MYINFO
%% @end
handle_numeric_reply(004, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_MYINFO no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% Sent by the server to a user to suggest an alternative
%% server.  This is often used when the connection is
%% refused because the server is already full.

%% RPL_BOUNCE
%% @end
handle_numeric_reply(005, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_BOUNCE not implemented.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - Reply format used by USERHOST to list replies to
%%   the query list.  The reply string is composed as
%%   follows:

%%   reply = nickname [ "*" ] "=" ( "+" / "-" ) hostname

%%   The '*' indicates whether the client has registered
%%   as an Operator.  The '-' or '+' characters represent
%%   whether the client has set an AWAY message or not
%%   respectively.

%% RPL_USERHOST
%% @end
handle_numeric_reply(302, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_USERHOST not implemented.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - Reply format used by ISON to list replies to the
%%   query list.

%% RPL_ISON
%% @end
handle_numeric_reply(303, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ISON not implemented.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - These replies are used with the AWAY command (if
%% allowed).  RPL_AWAY is sent to any client sending a
%% PRIVMSG to a client which is away.  RPL_AWAY is only
%% sent by the server to which the client is connected.
%% Replies RPL_UNAWAY and RPL_NOWAWAY are sent when the
%% client removes and sets an AWAY message.

%% RPL_AWAY
%% @end
handle_numeric_reply(301, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_AWAY not implemented.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_UNAWAY
%% @end
handle_numeric_reply(305, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_UNAWAY no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_NOWAWAY
%% @end
handle_numeric_reply(306, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_NOWAWAY no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - Replies 311 - 313, 317 - 319 are all replies
%% generated in response to a WHOIS message.  Given that
%% there are enough parameters present, the answering
%% server MUST either formulate a reply out of the above
%% numerics (if the query nick is found) or return an
%% error reply.  The '*' in RPL_WHOISUSER is there as
%% the literal character and not as a wild card.  For
%% each reply set, only RPL_WHOISCHANNELS may appear
%% more than once (for long lists of channel names).
%% The '@' and '+' characters next to the channel name
%% indicate whether a client is a channel operator or
%% has been granted permission to speak on a moderated
%% channel.  The RPL_ENDOFWHOIS reply is used to mark
%% the end of processing a WHOIS message.

%% RPL_WHOISUSER
%% @end
handle_numeric_reply(311, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_WHOISUSER no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_WHOISSERVER
%% @end
handle_numeric_reply(312, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_WHOISSERVER no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_WHOISOPERATOR
%% @end
handle_numeric_reply(313, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_WHOISOPERATOR no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_WHOISIDLE
%% @end
handle_numeric_reply(317, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_WHOISIDLE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFWHOIS
%% @end
handle_numeric_reply(318, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFWHOIS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_WHOISCHANNELS
%% @end
handle_numeric_reply(319, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_WHOISCHANNELS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% @doc
%% Logged in as...
%% WHOIS nox :nox 
%% :morgan.freenode.net 311 gerttest nox uid3872 gateway/web/irccloud.com/x-xedmtsocvxwmvmus * :Anthony Ramine
%% :morgan.freenode.net 312 gerttest nox morgan.freenode.net :Chicago, IL, USA
%% :morgan.freenode.net 317 gerttest nox 177 1367173582 :seconds idle, signon time
%% :morgan.freenode.net 330 gerttest nox nox :is logged in as
%% :morgan.freenode.net 318 gerttest nox :End of /WHOIS list.
%% @end
handle_numeric_reply(330, _Msg, #state{}=State) ->
    ?PRINT("Reply for loggedinas no implemented yet.~n~p~n", [_Msg]),
    {ok, State};

%% - When replying to a WHOWAS message, a server MUST use
%% the replies RPL_WHOWASUSER, RPL_WHOISSERVER or
%% ERR_WASNOSUCHNICK for each nickname in the presented
%% list.  At the end of all reply batches, there MUST
%% be RPL_ENDOFWHOWAS (even if there was only one reply
%% and it was an error).

%% RPL_WHOWASUSER
%% @end
handle_numeric_reply(314, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_WHOWASUSER no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFWHOWAS
%% @end
handle_numeric_reply(369, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFWHOWAS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - Replies RPL_LIST, RPL_LISTEND mark the actual replies
%% with data and end of the server's response to a LIST
%% command.  If there are no channels available to return,
%% only the end reply MUST be sent.

%% RPL_LIST
%% @end
handle_numeric_reply(322, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_LIST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_LISTEND
%% @end
handle_numeric_reply(323, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_LISTEND no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_UNIQOPIS
%% @end
handle_numeric_reply(325, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_UNIQOPIS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_CHANNELMODEIS
%% @end
handle_numeric_reply(324, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_CHANNELMODEIS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - When sending a TOPIC message to determine the
%% channel topic, one of two replies is sent.  If
%% the topic is set, RPL_TOPIC is sent back else
%% RPL_NOTOPIC.

%% RPL_NOTOPIC
%% @end
handle_numeric_reply(331, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_NOTOPIC no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_TOPIC
%% @end
handle_numeric_reply(332, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TOPIC no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - Returned by the server to indicate that the
%% attempted INVITE message was successful and is
%% being passed onto the end client.

%% RPL_INVITING
%% @end
handle_numeric_reply(341, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_INVITING no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - Returned by a server answering a SUMMON message to
%% indicate that it is summoning that user.

%% RPL_SUMMONING
%% @end
handle_numeric_reply(342, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_SUMMONING no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - When listing the 'invitations masks' for a given channel,
%% a server is required to send the list back using the
%% RPL_INVITELIST and RPL_ENDOFINVITELIST messages.  A
%% separate RPL_INVITELIST is sent for each active mask.
%% After the masks have been listed (or if none present) a
%% RPL_ENDOFINVITELIST MUST be sent.

%% RPL_INVITELIST
%% @end
handle_numeric_reply(346, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_INVITELIST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFINVITELIST
%% @end
handle_numeric_reply(347, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFINVITELIST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - When listing the 'exception masks' for a given channel,
%% a server is required to send the list back using the
%% RPL_EXCEPTLIST and RPL_ENDOFEXCEPTLIST messages.  A
%% separate RPL_EXCEPTLIST is sent for each active mask.
%% After the masks have been listed (or if none present)
%% a RPL_ENDOFEXCEPTLIST MUST be sent.

%% RPL_EXCEPTLIST
%% @end
handle_numeric_reply(348, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_EXCEPTLIST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFEXCEPTLIST
%% @end
handle_numeric_reply(349, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFEXCEPTLIST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - Reply by the server showing its version details.
%% The <version> is the version of the software being
%% used (including any patchlevel revisions) and the
%% <debuglevel> is used to indicate if the server is
%% running in "debug mode".

%% The "comments" field may contain any comments about
%% the version or further version details.

%% RPL_VERSION
%% @end
handle_numeric_reply(351, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_VERSION no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - The RPL_WHOREPLY and RPL_ENDOFWHO pair are used
%% to answer a WHO message.  The RPL_WHOREPLY is only
%% sent if there is an appropriate match to the WHO
%% query.  If there is a list of parameters supplied
%% with a WHO message, a RPL_ENDOFWHO MUST be sent
%% after processing each list item with <name> being
%% the item.

%% RPL_WHOREPLY
%% @end
handle_numeric_reply(352, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_WHOREPLY no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFWHO
%% @end
handle_numeric_reply(315, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFWHO no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - To reply to a NAMES message, a reply pair consisting
%% of RPL_NAMREPLY and RPL_ENDOFNAMES is sent by the
%% server back to the client.  If there is no channel
%% found as in the query, then only RPL_ENDOFNAMES is
%% returned.  The exception to this is when a NAMES
%% message is sent with no parameters and all visible
%% channels and contents are sent back in a series of
%% RPL_NAMEREPLY messages with a RPL_ENDOFNAMES to mark
%% the end.

%% RPL_NAMREPLY
%% @end
handle_numeric_reply(353, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_NAMREPLY no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFNAMES
%% @end
handle_numeric_reply(366, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFNAMES no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - In replying to the LINKS message, a server MUST send
%% replies back using the RPL_LINKS numeric and mark the
%% end of the list using an RPL_ENDOFLINKS reply.

%% RPL_LINKS
%% @end
handle_numeric_reply(364, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_LINKS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFLINKS
%% @end
handle_numeric_reply(365, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFLINKS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - When listing the active 'bans' for a given channel,
%% a server is required to send the list back using the
%% RPL_BANLIST and RPL_ENDOFBANLIST messages.  A separate
%% RPL_BANLIST is sent for each active banmask.  After the
%% banmasks have been listed (or if none present) a
%% RPL_ENDOFBANLIST MUST be sent.

%% RPL_BANLIST
%% @end
handle_numeric_reply(367, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_BANLIST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFBANLIST
%% @end
handle_numeric_reply(368, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFBANLIST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - A server responding to an INFO message is required to
%% send all its 'info' in a series of RPL_INFO messages
%% with a RPL_ENDOFINFO reply to indicate the end of the
%% replies.

%% RPL_INFO
%% @end
handle_numeric_reply(371, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_INFO no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFINFO
%% @end
handle_numeric_reply(374, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFINFO no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - When responding to the MOTD message and the MOTD file
%% is found, the file is displayed line by line, with
%% each line no longer than 80 characters, using
%% RPL_MOTD format replies.  These MUST be surrounded
%% by a RPL_MOTDSTART (before the RPL_MOTDs) and an
%% RPL_ENDOFMOTD (after).

%% RPL_MOTDSTART
%% @end
handle_numeric_reply(375, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_MOTDSTART no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_MOTD
%% @end
handle_numeric_reply(372, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_MOTD no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFMOTD
%% we're using this to start the joining of channels.
%% Should get passed around as state, so we know what to join here.
%%
%% @end
handle_numeric_reply(376, _Msg, #state{serverconfig=#serverconfig{channels=_Channels}}=State) ->
    join_channels(State);
%% @doc

%% - RPL_YOUREOPER is sent back to a client which has
%% just successfully issued an OPER message and gained
%% operator status.

%% RPL_YOUREOPER
%% @end
handle_numeric_reply(381, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_YOUREOPER no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - If the REHASH option is used and an operator sends
%% a REHASH message, an RPL_REHASHING is sent back to
%% the operator.

%% RPL_REHASHING
%% @end
handle_numeric_reply(382, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_REHASHING no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - Sent by the server to a service upon successful
%% registration.

%% RPL_YOURESERVICE
%% @end
handle_numeric_reply(383, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_YOURESERVICE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - When replying to the TIME message, a server MUST send
%% the reply using the RPL_TIME format below.  The string
%% showing the time need only contain the correct day and
%% time there.  There is no further requirement for the
%% time string.

%% RPL_TIME
%% @end
handle_numeric_reply(391, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TIME no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - If the USERS message is handled by a server, the
%% replies RPL_USERSTART, RPL_USERS, RPL_ENDOFUSERS and
%% RPL_NOUSERS are used.  RPL_USERSSTART MUST be sent
%% first, following by either a sequence of RPL_USERS
%% or a single RPL_NOUSER.  Following this is
%% RPL_ENDOFUSERS.

%% RPL_USERSSTART
%% @end
handle_numeric_reply(392, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_USERSSTART no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_USERS
%% @end
handle_numeric_reply(393, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_USERS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ENDOFUSERS
%% @end
handle_numeric_reply(394, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFUSERS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_NOUSERS
%% @end
handle_numeric_reply(395, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_NOUSERS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - The RPL_TRACE* are all returned by the server in
%% response to the TRACE message.  How many are
%% returned is dependent on the TRACE message and
%% whether it was sent by an operator or not.  There
%% is no predefined order for which occurs first.
%% Replies RPL_TRACEUNKNOWN, RPL_TRACECONNECTING and
%% RPL_TRACEHANDSHAKE are all used for connections
%% which have not been fully established and are either
%% unknown, still attempting to connect or in the
%% process of completing the 'server handshake'.
%% RPL_TRACELINK is sent by any server which handles
%% a TRACE message and has to pass it on to another
%% server.  The list of RPL_TRACELINKs sent in
%% response to a TRACE command traversing the IRC
%% network should reflect the actual connectivity of
%% the servers themselves along that path.
%% RPL_TRACENEWTYPE is to be used for any connection
%% which does not fit in the other categories but is
%% being displayed anyway.
%% RPL_TRACEEND is sent to indicate the end of the list.

%% RPL_TRACELINK
%% @end
handle_numeric_reply(200, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACELINK no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_TRACECONNECTING
%% @end
handle_numeric_reply(201, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACECONNECTING no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACEHANDSHAKE
%% @end
handle_numeric_reply(202, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACEHANDSHAKE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACEUNKNOWN
%% @end
handle_numeric_reply(203, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACEUNKNOWN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACEOPERATOR
%% @end
handle_numeric_reply(204, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACEOPERATOR no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACEUSER
%% @end
handle_numeric_reply(205, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACEUSER no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACESERVER
%% @end
handle_numeric_reply(206, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACESERVER no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACESERVICE
%% @end
handle_numeric_reply(207, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACESERVICE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACENEWTYPE
%% @end
handle_numeric_reply(208, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACENEWTYPE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACECLASS
%% @end
handle_numeric_reply(209, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACECLASS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACERECONNECT
%% @end
handle_numeric_reply(210, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACERECONNECT no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACELOG
%% @end
handle_numeric_reply(261, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACELOG no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TRACEEND
%% @end
handle_numeric_reply(262, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRACEEND no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% - reports statistics on a connection.  <linkname> identifies the particular connection,
%% <sendq> is the amount of data that is queued and waiting to be
%% sent <sent messages> the number of messages sent, and <sent Kbytes> the amount of data sent,
%% in Kbytes. <received messages> and <received Kbytes> are the equivalent of
%% <sent messages> and <sent Kbytes> for received data,
%% respectively.  <time open> indicates how long ago the connection was opened, in seconds.

%% RPL_STATSLINKINFO
%% "<linkname> <sendq> <sent messages> <sent Kbytes> <received messages> <received Kbytes> <time open>"
%% @end
handle_numeric_reply(211, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_STATSLINKINFO no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_STATSCOMMANDS
%% "<command> <count> <byte count> <remote count>"
%% reports statistics on commands usage.
%% @end
handle_numeric_reply(212, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_STATSCOMMANDS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_ENDOFSTATS
%% "<stats letter> :End of STATS report"
%% @end
handle_numeric_reply(219, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ENDOFSTATS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_STATSUPTIME
%% ":Server Up %d days %d:%02d:%02d"
%% Reports the server uptime.
%% @end
handle_numeric_reply(242, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_STATSUPTIME no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_STATSOLINE
%% "O <hostmask> * <name>"
%% reports the allowed hosts from where user may become IRC operators.
%% @end
handle_numeric_reply(243, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_STATSOLINE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_UMODEIS
%% "<user mode string>"
%% To answer a query about a client's own mode, RPL_UMODEIS is sent back.
%% @end
handle_numeric_reply(221, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_UMODEIS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_SERVLIST
%% "<name> <server> <mask> <type> <hopcount> <info>"
%% @end
handle_numeric_reply(234, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_SERVLIST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_SERVLISTEND
%% "<mask> <type> :End of service listing"
%% When listing services in reply to a SERVLIST message,
%% a server is required to send the list back using the
%% RPL_SERVLIST and RPL_SERVLISTEND messages.  A separate
%% RPL_SERVLIST is sent for each service.  After the
%% services have been listed (or if none present) a
%% RPL_SERVLISTEND MUST be sent.
%% @end
handle_numeric_reply(235, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_SERVLISTEND no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% In processing an LUSERS message, the server
%% sends a set of replies from RPL_LUSERCLIENT,
%% RPL_LUSEROP, RPL_USERUNKNOWN,
%% RPL_LUSERCHANNELS and RPL_LUSERME.  When
%% replying, a server MUST send back
%% RPL_LUSERCLIENT and RPL_LUSERME.  The other
%% replies are only sent back if a non-zero count
%% is found for them.

%% RPL_LUSERCLIENT
%% ":There are <integer> users and <integer> services on <integer> servers"
%% @end
handle_numeric_reply(251, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_LUSERCLIENT no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_LUSEROP
%% "<integer> :operator(s) online"
%% @end
handle_numeric_reply(252, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_LUSEROP no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_LUSERUNKNOWN
%% "<integer> :unknown connection(s)"
%% @end
handle_numeric_reply(253, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_LUSERUNKNOWN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_LUSERCHANNELS
%% "<integer> :channels formed"
%% @end
handle_numeric_reply(254, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_LUSERCHANNELS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_LUSERME
%% ":I have <integer> clients and <integer> servers"
%% @end
handle_numeric_reply(255, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_LUSERME no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% When replying to an ADMIN message,  a server
%% is expected to use replies RPL_ADMINME
%% through to RPL_ADMINEMAIL and provide a text
%% message with each.  For RPL_ADMINLOC1 a
%% description of what city, state and country
%% the server is in is expected, followed by
%% details of the institution (RPL_ADMINLOC2)
%% and finally the administrative contact for the
%% server (an email address here is REQUIRED)
%% in RPL_ADMINEMAIL.

%% RPL_ADMINME
%% "<server> :Administrative info"
%% @end
handle_numeric_reply(256, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ADMINME no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_ADMINLOC1
%% ":<admin info>"
%% @end
handle_numeric_reply(257, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ADMINLOC1 no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_ADMINLOC2
%% ":<admin info>"
%% @end
handle_numeric_reply(258, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ADMINLOC2 no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% RPL_ADMINEMAIL
%% ":<admin info>
%% @end
handle_numeric_reply(259, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_ADMINEMAIL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% When a server drops a command without processing it,
%% it MUST use the reply RPL_TRYAGAIN to inform the
%% originating client.

%% RPL_TRYAGAIN
%% "<command> :Please wait a while and try again."
%% @end
handle_numeric_reply(263, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TRYAGAIN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% RPL_TOPICWHOTIME
%% @end
handle_numeric_reply(333, _Msg, #state{}=State) ->
    ?PRINT("Reply for RPL_TOPICWHOTIME no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc


%%%%%%%%%%%%% ERROR REPLIES %%%%%%%%%%%%%%%%%%%%%%%%

%% Used to indicate the nickname parameter supplied to a
%% command is currently unused.
%% ERR_NOSUCHNICK
%% "<nickname> :No such nick/channel"
%% @end
handle_numeric_reply(401, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOSUCHNICK no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% Used to indicate the server name given currently
%% does not exist.
%% ERR_NOSUCHSERVER
%% "<server name> :No such server"
%% @end
handle_numeric_reply(402, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOSUCHSERVER no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOSUCHCHANNEL
%% "<channel name> :No such channel"
%% Used to indicate the given channel name is invalid.
%% @end
handle_numeric_reply(403, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOSUCHCHANNEL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_CANNOTSENDTOCHAN
%% "<channel name> :Cannot send to channel"
%% Sent to a user who is either (a) not on a channel
%% which is mode +n or (b) not a chanop (or mode +v) on
%% a channel which has mode +m set or where the user is
%% banned and is trying to send a PRIVMSG message to
%% that channel.
%% @end
handle_numeric_reply(404, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_CANNOTSENDTOCHAN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_TOOMANYCHANNELS
%% "<channel name> :You have joined too many channels"
%% Sent to a user when they have joined the maximum
%% number of allowed channels and they try to join
%% another channel.
%% @end
handle_numeric_reply(405, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_TOOMANYCHANNELS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc


%% ERR_WASNOSUCHNICK
%% "<nickname> :There was no such nickname"
%% Returned by WHOWAS to indicate there is no history
%% information for that nickname.
%% @end
handle_numeric_reply(406, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_WASNOSUCHNICK no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_TOOMANYTARGETS
%% "<target> :<error code> recipients. <abort message>"
%% Returned to a client which is attempting to send a
%% PRIVMSG/NOTICE using the user@host destination format
%% and for a user@host which has several occurrences.
%% Returned to a client which trying to send a
%% PRIVMSG/NOTICE to too many recipients.
%% Returned to a client which is attempting to JOIN a safe
%% channel using the shortname when there are more than one
%% such channel.
%% @end
handle_numeric_reply(407, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_TOOMANYTARGETS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOSUCHSERVICE
%% "<service name> :No such service"
%% Returned to a client which is attempting to send a SQUERY
%% to a service which does not exist.
%% @end
handle_numeric_reply(408, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOSUCHSERVICE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOORIGIN
%% ":No origin specified"
%% PING or PONG message missing the originator parameter.
%% @end
handle_numeric_reply(409, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOORIGIN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NORECIPIENT
%% ":No recipient given (<command>)"
%% @end
handle_numeric_reply(411, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NORECIPIENT no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc


%% 412 - 415 are returned by PRIVMSG to indicate that
%% the message wasn't delivered for some reason.
%% ERR_NOTOPLEVEL and ERR_WILDTOPLEVEL are errors that
%% are returned when an invalid use of
%% "PRIVMSG $<server>" or "PRIVMSG #<host>" is attempted.

%% ERR_NOTEXTTOSEND
%% ":No text to send"
%% @end
handle_numeric_reply(412, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOTEXTTOSEND no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOTOPLEVEL
%% "<mask> :No toplevel domain specified"
%% @end
handle_numeric_reply(413, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOTOPLEVEL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_WILDTOPLEVEL
%% "<mask> :Wildcard in toplevel domain"
%% @end
handle_numeric_reply(414, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_WILDTOPLEVEL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_BADMASK
%% "<mask> :Bad Server/host mask"
%% @end
handle_numeric_reply(415, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_BADMASK no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_UNKNOWNCOMMAND
%% "<command> :Unknown command"
%% @end
handle_numeric_reply(421, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_UNKNOWNCOMMAND no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOMOTD
%% ":MOTD File is missing"
%% @end
handle_numeric_reply(422, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOMOTD no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOADMININFO
%% "<server> :No administrative info available"
%% Returned by a server in response to an ADMIN message
%% when there is an error in finding the appropriate
%% information.
%% @end
handle_numeric_reply(423, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOADMININFO no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_FILEERROR
%% ":File error doing <file op> on <file>"
%% Generic error message used to report a failed file
%% operation during the processing of a message.
%% @end
handle_numeric_reply(424, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_FILEERROR no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NONICKNAMEGIVEN
%% ":No nickname given"
%% Returned when a nickname parameter expected for a
%% command and isn't found.
%% @end
handle_numeric_reply(431, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NONICKNAMEGIVEN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_ERRONEUSNICKNAME
%% "<nick> :Erroneous nickname"
%% Returned after receiving a NICK message which contains
%% characters which do not fall in the defined set.  See
%% section 2.3.1 for details on valid nicknames.
%% @end
handle_numeric_reply(432, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_ERRONEUSNICKNAME no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc


%% ERR_NICKNAMEINUSE
%% "<nick> :Nickname is already in use"
%% Returned when a NICK message is processed that results
%% in an attempt to change to a currently existing
%% nickname.
%% @end
handle_numeric_reply(433, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NICKNAMEINUSE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NICKCOLLISION
%% "<nick> :Nickname collision KILL from <user>@<host>"
%% Returned by a server to a client when it detects a
%% nickname collision (registered of a NICK that
%% already exists by another server).
%% @end
handle_numeric_reply(436, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NICKCOLLISION no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_UNAVAILRESOURCE
%% "<nick/channel> :Nick/channel is temporarily unavailable"
%% Returned by a server to a user trying to join a channel
%% currently blocked by the channel delay mechanism.
%% Returned by a server to a user trying to change nickname
%% when the desired nickname is blocked by the nick delay
%% mechanism.
%% @end
handle_numeric_reply(437, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_UNAVAILRESOURCE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_USERNOTINCHANNEL
%% "<nick> <channel> :They aren't on that channel"
%% Returned by the server to indicate that the target
%% user of the command is not on the given channel.
%% @end
handle_numeric_reply(441, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_USERNOTINCHANNEL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOTONCHANNEL
%% "<channel> :You're not on that channel"
%% Returned by the server whenever a client tries to
%% perform a channel affecting command for which the
%% client isn't a member.
%% @end
handle_numeric_reply(442, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOTONCHANNEL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_USERONCHANNEL
%% "<user> <channel> :is already on channel"
%% Returned when a client tries to invite a user to a
%% channel they are already on.
%% @end
handle_numeric_reply(443, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_USERONCHANNEL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOLOGIN
%% "<user> :User not logged in"
%% Returned by the summon after a SUMMON command for a
%% user was unable to be performed since they were not
%% logged in.
%% @end
handle_numeric_reply(444, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOLOGIN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_SUMMONDISABLED
%% ":SUMMON has been disabled"
%% Returned as a response to the SUMMON command.  MUST be
%% returned by any server which doesn't implement it.
%% @end
handle_numeric_reply(445, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_SUMMONDISABLED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_USERSDISABLED
%% ":USERS has been disabled"
%% Returned as a response to the USERS command.  MUST be
%% returned by any server which does not implement it.
%% @end
handle_numeric_reply(446, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_USERSDISABLED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOTREGISTERED
%% ":You have not registered"
%% Returned by the server to indicate that the client
%% MUST be registered before the server will allow it
%% to be parsed in detail.
%% @end
handle_numeric_reply(451, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOTREGISTERED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NEEDMOREPARAMS
%% "<command> :Not enough parameters"
%% Returned by the server by numerous commands to
%% indicate to the client that it didn't supply enough
%% parameters.
%% @end
handle_numeric_reply(461, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NEEDMOREPARAMS no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_ALREADYREGISTRED
%% ":Unauthorized command (already registered)"
%% Returned by the server to any link which tries to
%% change part of the registered details (such as
%% password or user details from second USER message).
%% @end
handle_numeric_reply(462, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_ALREADYREGISTRED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOPERMFORHOST
%% ":Your host isn't among the privileged"
%% Returned to a client which attempts to register with
%% a server which does not been setup to allow
%% connections from the host the attempted connection
%% is tried.
%% @end
handle_numeric_reply(463, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOPERMFORHOST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_PASSWDMISMATCH
%% ":Password incorrect"
%% Returned to indicate a failed attempt at registering
%% a connection for which a password was required and
%% was either not given or incorrect.
%% @end
handle_numeric_reply(464, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_PASSWDMISMATCH no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_YOUREBANNEDCREEP
%% ":You are banned from this server"
%% Returned after an attempt to connect and register
%% yourself with a server which has been setup to
%% explicitly deny connections to you.
%% @end
handle_numeric_reply(465, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_YOUREBANNEDCREEP no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_YOUWILLBEBANNED
%% Sent by a server to a user to inform that access to the
%% server will soon be denied.
%% @end
handle_numeric_reply(466, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_YOUWILLBEBANNED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_KEYSET
%% "<channel> :Channel key already set"
%% @end
handle_numeric_reply(467, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_KEYSET no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_CHANNELISFULL
%% "<channel> :Cannot join channel (+l)"
%% @end
handle_numeric_reply(471, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_CHANNELISFULL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_UNKNOWNMODE
%% "<char> :is unknown mode char to me for <channel>"
%% @end
handle_numeric_reply(472, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_UNKNOWNMODE no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_INVITEONLYCHAN
%% "<channel> :Cannot join channel (+i)"
%% @end
handle_numeric_reply(473, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_INVITEONLYCHAN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_BANNEDFROMCHAN
%% "<channel> :Cannot join channel (+b)"
%% @end
handle_numeric_reply(474, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_BANNEDFROMCHAN no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_BADCHANNELKEY
%% "<channel> :Cannot join channel (+k)"
%% @end
handle_numeric_reply(475, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_BADCHANNELKEY no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_BADCHANMASK
%% "<channel> :Bad Channel Mask"
%% @end
handle_numeric_reply(476, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_BADCHANMASK no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOCHANMODES
%% "<channel> :Channel doesn't support modes"
%% @end
handle_numeric_reply(477, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOCHANMODES no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_BANLISTFULL
%% "<channel> <char> :Channel list is full"
%% @end
handle_numeric_reply(478, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_BANLISTFULL no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOPRIVILEGES
%% ":Permission Denied- You're not an IRC operator"
%% Any command requiring operator privileges to operate
%% MUST return this error to indicate the attempt was
%% unsuccessful.
%% @end
handle_numeric_reply(481, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOPRIVILEGES no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_CHANOPRIVSNEEDED
%% "<channel> :You're not channel operator"
%% Any command requiring 'chanop' privileges (such as
%% MODE messages) MUST return this error if the client
%% making the attempt is not a chanop on the specified
%% channel.
%% @end
handle_numeric_reply(482, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_CHANOPRIVSNEEDED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_CANTKILLSERVER
%% ":You can't kill a server!"
%% Any attempts to use the KILL command on a server
%% are to be refused and this error returned directly
%% to the client.
%% @end
handle_numeric_reply(483, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_CANTKILLSERVER no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_RESTRICTED
%% ":Your connection is restricted!"
%% Sent by the server to a user upon connection to indicate
%% the restricted nature of the connection (user mode "+r").
%% @end
handle_numeric_reply(484, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_RESTRICTED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_UNIQOPPRIVSNEEDED
%% ":You're not the original channel operator"
%% Any MODE requiring "channel creator" privileges MUST
%% return this error if the client making the attempt is not
%% a chanop on the specified channel.
%% @end
handle_numeric_reply(485, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_UNIQOPPRIVSNEEDED no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_NOOPERHOST
%% ":No O-lines for your host"
%% If a client sends an OPER message and the server has
%% not been configured to allow connections from the
%% client's host as an operator, this error MUST be
%% returned.
%% @end
handle_numeric_reply(491, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_NOOPERHOST no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_UMODEUNKNOWNFLAG
%% ":Unknown MODE flag"
%% Returned by the server to indicate that a MODE
%% message was sent with a nickname parameter and that
%% the a mode flag sent was not recognized.
%% @end
handle_numeric_reply(501, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_UMODEUNKNOWNFLAG no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc

%% ERR_USERSDONTMATCH
%% ":Cannot change mode for other users"
%% Error sent to any user trying to view or change the
%% user mode for a user other than themselves.
%% @end
handle_numeric_reply(502, _Msg, #state{}=State) ->
    ?PRINT("Reply for ERR_USERSDONTMATCH no implemented yet.~n~p~n", [_Msg]),
    {ok, State};
%% @doc
%% Unknown numeric reply.
%% @end
handle_numeric_reply(_Nr, _Msg, #state{}=State) ->
    ?PRINT("Unknown numeric reply: ~p: ~p ~n",[_Nr, _Msg]),
    {ok, State}.

