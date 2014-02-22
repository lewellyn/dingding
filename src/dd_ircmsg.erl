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
%%% Parser module for IRC messages.
%%% Pulled this from my 'ding' project.
%%% @end
%%% Created : 29 Jan 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_ircmsg).
-include("../include/dd_irc.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(COLON, 58).

%%& API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([parse_line/1, parse_packet/1]).
-export([to_line/1]).
-export([create/4]).
%% helpers
-export([nick/1, show/1]).
-export([nick_from_prefix/1, is_numeric/1]).
%% accessors
-export([prefix/1, command/1, arguments/1, tail/1]).

-spec prefix(#ircmsg{}) -> binary().
prefix(#ircmsg{prefix=P}) -> P.

-spec command(#ircmsg{}) -> binary().
command(#ircmsg{command=C}) -> C.

-spec arguments(#ircmsg{}) -> [binary()].
arguments(#ircmsg{arguments=A}) -> A.

-spec tail(#ircmsg{}) -> binary().
tail(#ircmsg{tail=T}) -> T.

%% @doc
%% Retrieves the nickname from an ircmsg{} record.
%% Returns an empty binary when there is no nick to be retrieved.
%% @end
-spec nick(#ircmsg{}) -> binary().
nick(Msg) ->
    nick_from_prefix(prefix(Msg)).

-spec nick_from_prefix(binary()) -> binary().
nick_from_prefix(<<>>) ->
    <<>>;
nick_from_prefix(P) ->
    hd(binary:split(P, <<"!">>)).

%% @doc
%% Displays the ircmsg{} on screen. Useful for debugging.
%% @end
-spec show(#ircmsg{}) -> ok.
%% show(#ircmsg{command = <<"PRIVMSG">>, arguments=A, tail=_T}=Msg) ->
%%     _Channel = binary_to_list(hd(A)), 
%%     _Nick = binary_to_list(nick(Msg)), 
%%     _Content = binary_to_list(_T), 
%%     ?PRINT("~s <~s> ~s~n", [_Channel, _Nick, _Content]);
show(#ircmsg{}=_Msg) ->
    ?PRINT("~p~n",[_Msg]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Helper Functions
-spec create(Prefix :: binary(), Command :: binary(), Arguments :: [binary()], Tail :: binary()) -> #ircmsg{}.
create(Prefix, Command, Arguments, Tail) ->
    #ircmsg{prefix=Prefix, command=Command, arguments=Arguments, tail=Tail}.

%%% server to ircmsg %%%

-spec lines(Packet :: binary()) -> [binary()].
lines(Packet) ->
    binary:split(Packet, <<"\r\n">>, [global, trim]).

-spec starts_with_colon(B :: binary()) -> boolean().
starts_with_colon(<<>>) ->
    false;
starts_with_colon(Bin) ->
    binary:first(Bin) == ?COLON.

-spec rest(B :: binary()) -> binary().
rest(<<>>) ->
    <<>>;
rest(B) when is_binary(B) ->
    {_, B2} = split_binary(B, 1), 
    B2.

-spec words_in_line(Line :: binary()) -> [binary()].
words_in_line(Line) ->
    binary:split(Line, <<" ">>, [global, trim]).

-spec next_word(list()) -> tuple().
next_word([]) ->
    {the_end, []};
next_word([H]) ->
    {H, []};
next_word([H|T]) ->
    {H, T}.

-spec get_prefix(Words :: [binary()]) -> {binary(), [binary()]}.
get_prefix([H|T]=L) ->
    case starts_with_colon(H) of
        true -> {rest(H), T};
        _ -> {<<>>, L}
    end;
get_prefix(A) ->
    {<<>>, A}.

-spec assemble_tail([binary()]) -> binary().
assemble_tail(T) ->
    FirstStep = iolist_to_binary([ [ <<" ">>, X ] || X <- T ]), 
    rest(rest(FirstStep)).

-spec get_arguments_and_tail(AfterCmd :: [binary()]) -> {[binary()], binary()}.
get_arguments_and_tail(AfterCmd) ->
    {Args, T} = lists:splitwith(fun(X) -> not(starts_with_colon(X)) end, AfterCmd), 
    {Args, assemble_tail(T)}.

%% @doc
%% Converts the binary line that comes from the server to an ircmsg{} record.
%% @end
-spec parse_line(RawLine :: binary()) -> #ircmsg{}.
parse_line(RawLine) ->
    Line = binary:replace(RawLine, <<"\r\n">>, <<>> ),
    Words = words_in_line(Line), 
    {P, Rest1} = get_prefix(Words), 
    {C, Rest2} = next_word(Rest1), 
    {A, T} = get_arguments_and_tail(Rest2), 
    Msg = #ircmsg{prefix=P, command=C, arguments=A, tail=T}, 
    case is_ctcp(Msg) of
        true -> convert_ctcp(Msg);
        _ -> Msg
    end.
%% @doc
%% Checks whether the ircmsg{} is a CTCP message.
%% @end
-spec is_ctcp(#ircmsg{}) -> boolean().
is_ctcp(#ircmsg{tail = <<>>}) ->
    false;
is_ctcp(#ircmsg{tail=T}) ->
    binary:first(T) == 1 andalso binary:last(T) == 1.

-spec strip_tailwraps(T :: binary()) -> binary().
strip_tailwraps(<<>>) -> <<>>;
strip_tailwraps(B) ->
    TailSize = byte_size(B), 
    binary_part(B, 1, TailSize-2).

%% @doc
%% extremely basic CTCP parsing
%% @end
-spec convert_ctcp(#ircmsg{}) -> #ircmsg{}.
convert_ctcp(#ircmsg{command = <<"NOTICE">>, tail=T}=Msg) ->
    Msg#ircmsg{command = <<"CTCP_REPLY">>, tail = strip_tailwraps(T)};
convert_ctcp(#ircmsg{command = <<"PRIVMSG">>, tail=T}=Msg) ->
    Msg#ircmsg{command = <<"CTCP">>, tail = strip_tailwraps(T)}.

%% @doc
%% A 'packet' is just a list of lines. Run parse_line/1 on all of them.
%% @end
-spec parse_packet(Packet :: binary()) -> [ [tuple()] ].
parse_packet(Packet) ->
    [ parse_line(X) || X <- lines(Packet) ].

%%% ircmsg to server %%%

%% @doc
%% Converts an ircmsg{} back to a binary line so it can be sent to the server.
%% @end
-spec to_line(Msg :: #ircmsg{}) -> binary().
to_line(#ircmsg{prefix=P, command=C, arguments=A, tail=T}=_Msg) ->
    Prefix =
        case P of
            <<>> -> <<>>;
            _ -> iolist_to_binary([<<":">>, P, <<" ">>])
        end, 
    {Cmd, T2} = 
        case C of
            <<"CTCP">> -> {<<"PRIVMSG">>, [<<1>>, T, <<1>>]};
            <<"CTCP_REPLY">> -> {<<"NOTICE">>, [<<1>>, T, <<1>>]};
            _ -> {C, T}
        end, 
    Tail = 
        case T2 of
            <<>> -> <<>>;
            none -> <<>>;
            _ -> iolist_to_binary([<<" :">>, T2])
        end, 
    iolist_to_binary([Prefix, Cmd, [ [<<" ">>, X] || X <- A ], Tail]).

%% @doc
%% Returns {true, integer()} when the ircmsg{} is a numeric reply from the server.
%% @end
-spec is_numeric(Msg :: #ircmsg{}) -> { true | false, integer() }.
is_numeric(#ircmsg{command=C}) ->
    Cmd = binary:bin_to_list(C), 
    case string:to_integer(Cmd) of
        {error, _} -> {false, 0};
        {I, _} -> {true, I}
    end.


-ifdef(TEST).

parse_line_test() ->
    ?assertEqual(#ircmsg{prefix = <<"prefix">>, command = <<"command">>, arguments=[<<"arg1">>, <<"arg2">>], tail = <<"tail of the line">>}, 
                 parse_line(<<":prefix command arg1 arg2 :tail of the line">>)), 
    ?assertEqual(#ircmsg{prefix = <<>>, command = <<"command">>, arguments=[], tail = <<"tail of the line">>}, 
                 parse_line(<<"command :tail of the line">>)), 
    ?assertEqual(#ircmsg{prefix = <<>>, command = <<"NICK">>, arguments=[ <<"mynick">> ], tail = <<>>}, 
                 parse_line(<<"NICK mynick">>)), 
    ?assertEqual(parse_line(iolist_to_binary([<<>>, <<"PRIVMSG ">>, <<"nickname ">>, <<":">>, 
                                                      <<1>>, <<"VERSION">>, <<1>>])), 
                 #ircmsg{prefix = <<>>, command= <<"CTCP">>, arguments=[<<"nickname">>], 
                        tail= <<"VERSION">>}), 
    ?assertEqual(#ircmsg{prefix = <<>>, command = <<"CTCP">>, arguments=[ <<"#channel">> ], 
                         tail = <<"ACTION does a barrel roll.">>}, 
                 parse_line(iolist_to_binary([<<>>, <<"PRIVMSG ">>, <<"#channel ">>, <<":">>, 
                                              <<1>>, <<"ACTION does a barrel roll.">>, <<1>>]))), 
    ?assertEqual(#ircmsg{prefix = <<>>, command = <<"CTCP">>, arguments=[ <<"#channel">> ], 
                         tail = <<"ACTION does a barrel roll. :)">>}, 
                 parse_line(iolist_to_binary([<<>>, <<"PRIVMSG ">>, <<"#channel ">>, <<":">>, 
                                              <<1>>, <<"ACTION does a barrel roll. :)">>, <<1>>]))).



to_line_test() ->
    ?assertEqual(<<":prefix command arg1 arg2 :tail of the line">>, 
                 to_line(#ircmsg{prefix = <<"prefix">>, command = <<"command">>, arguments = [<<"arg1">>, <<"arg2">>], tail = <<"tail of the line">>})), 
    ?assertEqual(<<":prefix command">>, 
                 to_line(#ircmsg{prefix = <<"prefix">>, command = <<"command">>})), 
    ?assertEqual(<<"command arg1 arg2">>, 
                 to_line(#ircmsg{command = <<"command">>, arguments = [<<"arg1">>, <<"arg2">>]})), 
    ?assertEqual(<<"command :this is the tail">>, 
                 to_line(#ircmsg{command = <<"command">>, tail="this is the tail"})).
-endif.
