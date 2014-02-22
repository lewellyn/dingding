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
%%% Module for the bulletin board system.
%%% @end
%%% Created : 23 Dec 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_bb).
-export([get_post_by_number/1]).
-compile(export_all).


load(Filename) ->
    {ok, L} = file:consult(Filename),
    L.

load() ->
    load(code:priv_dir(dd)++"/yfml.terms").

init_dets() ->
    dets:open_file(bbdb, [{file, code:priv_dir(dd)++"/bbdb.dets"}, {repair, true}, {type, set}]).

terms_to_dets() ->
    Terms = load(),
    [ dets:insert(bbdb, Term) || Term <- Terms ].

get_post_by_number(Nr) ->
    case dets:lookup(bbdb, Nr) of
        [] -> none;
        [{Nr, From, Title, Text}] -> io_lib:format("~s (from: ~s): ~s", [Title, From, Text]);
        _ -> none
    end.

