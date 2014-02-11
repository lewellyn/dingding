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

%%% @doc
%%% Module to get all the data from yftcl
%%% @end
%%% Created :  19 Jan 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_yftcl).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5]).

-compile(export_all). %% remove later

-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(_P, _, <<"PRIVMSG">>, [<<"dingd1ng">>], <<"Total number of posts: ", Count/binary>>) ->
    NrOfPostsStr = string:strip(binary_to_list(Count),right,$.),
    {NrOfPosts,_} = string:to_integer(NrOfPostsStr),
    ?PRINT("Got total post count from yftcl, starting loop!",[]),
    spawn(fun() -> loop(0, NrOfPosts) end);
handle_msg(_P, <<"yftcl",_/binary>>, <<"PRIVMSG">>, [<<"dingd1ng">>], <<"{", _/binary>>=Full) ->
    {ok,Tokens,_} = erl_scan:string(binary_to_list(Full)),
    {ok,Term} = erl_parse:parse_term(Tokens),
    write_to_disk(Term);
handle_msg(_P, _Prefix, _Command, _Args, _Tail) ->
    ok.

start() ->
    dd_connection:reply('Freenode', [<<"yftcl">>], <<".count">>).

loop(Nr, Max) ->
    Next = Nr + 1,
    NextStr = hd(io_lib:format("~p", [Next])),
    if
        Next > Max -> ok;
        true ->
            receive
            after 3000 ->
                    io:format("Requesting post: ~p~n",[Next]),
                    dd_connection:reply('Freenode', [<<"yftcl">>], list_to_binary(".bb_erl "++ NextStr))
            end,
            loop(Next, Max)
    end.

storage() ->
    code:priv_dir(dd)++"/yftcl.terms".

write_to_disk(Msg) ->
    {ok, Iodev} = file:open(storage(), [append]),
    io:format(Iodev, "~p.~n", [Msg]),
    file:close(Iodev),
    ok.

