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
%%% Handler module for the 'le ..' commands of the bot.
%%% @end
%%% Created : 26 Sep 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_le_handler).
-behaviour(dd_msghandler).
-include("../include/dd_irc.hrl").
-export([handle_msg/5]).

-spec handle_msg(pid(), binary(), binary(), [binary()], binary()) -> ok.
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"Le doc for ", Doc/binary>>) ->
    handle_search(ReplyPid, Args, Doc);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"le doc for ", Doc/binary>>) ->   
    handle_search(ReplyPid, Args, Doc);
handle_msg(ReplyPid, _Prefix, <<"PRIVMSG">>, Args, <<"\\erldoc ", Doc/binary>>) ->   
    handle_search(ReplyPid, Args, Doc);
handle_msg(ReplyPid, _Prefix, <<"CTCP">>, Args, <<"ACTION searches for ", Doc/binary>>) ->
    handle_search(ReplyPid, Args, Doc);
handle_msg(_ReplyPid, _Prefix, _Command, _Args, _Tail) ->
    ok.

-spec handle_search(pid(), [binary()], binary()) -> ok.
handle_search(ReplyPid, Args, Doc) ->
    case erlang_doc_url(Doc) of
        none -> ok;
        Url -> dd_connection:reply(ReplyPid, Args, Url)
    end,
    ok.

-spec erlang_doc_url(binary()) -> binary().
erlang_doc_url(Doc) ->
    case binary:split(Doc, <<":">>) of
        [H|_T] -> 
            case url_exists("http://www.erlang.org/doc/man/"++binary_to_list(H)++".html") of
                false -> none;
                true ->
                    case reformat(Doc) of
                        none -> iolist_to_binary("http://www.erlang.org/doc/man/"++binary_to_list(H)++".html");
                        Url -> Url
                    end
            end;
        _ -> none
    end.
    
-spec url_exists(string()) -> boolean().
url_exists(Url) ->
    case dd_helpers:http_get(Url) of
        {error, _} -> false;
        {ok, {{_, 404, _}, _, _}} -> false;
        _ -> true
    end.

-spec reformat(binary()) -> 'none' | binary().
reformat(Doc) ->
    Pattern="^(\\w+):(\\w+)/(\\d+)$",
    {ok,Regex} = re:compile(Pattern, []),
    case re:run(Doc, Regex, [{capture, all_but_first, binary}]) of
        {match, [Module, Fn, Arity]} ->
            iolist_to_binary([<<"http://www.erlang.org/doc/man/">>, Module, <<".html#">>, Fn, <<"-">>, Arity]);
        _ -> none
    end.
