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
%%% DCC module. First and foremost a client module, server functionality
%%% may be added later.
%%% WARNING: Enabling this module (for a server) will result in the bot
%%% autoaccepting and downloading all files sent to it!
%%% You have been warned!
%%% @end
%%% Created :  4 Feb 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_dcc).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5]).
-compile(export_all).

%% need to be able to recognize DCC requests and accept incoming 'sends'.
%% brush up on DCC: RFC 2812 http://tools.ietf.org/html/rfc2812
%% and this: http://www.irchelp.org/irchelp/rfc/ctcpspec.html

-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(ReplyPid, Prefix, <<"CTCP">>, _Args, <<"DCC SEND ", SendSpec/binary>>) ->
    Parts = binary:split(SendSpec, <<" ">>, [global]),
    case length(Parts) of
        4 -> 
            [Filename, IP, P, S] = Parts,
            IpAddress = dd_helpers:int_to_ip(list_to_integer(binary_to_list(IP))),
            Port = dd_helpers:bin_to_int(P),
            Size = dd_helpers:bin_to_int(S),
            io:format("Going to get '~s' at: ~p:~p for a total size of ~p~n",
                      [Filename, IpAddress, Port, Size]),
            dcc_receive:download(Filename, IpAddress, Port, Size, code:priv_dir(dd));
        5 ->
            Nick = dd_ircmsg:nick_from_prefix(Prefix),
            [Filename, _, _Port, Size, Token] = Parts,
            dcc_reverse_receive:download(ReplyPid, [Nick], Filename, Size, Token, code:priv_dir(dd))
    end,
    ok;
handle_msg(_ReplyPid, _Prefix, _Command, _Args, _Tail) ->
    ok.
 

