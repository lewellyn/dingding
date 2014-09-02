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
%%% SQL stuff for working with the database.
%%% @end
%%% Created : 25 Feb 2013 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_sql).
-include("../include/dd_irc.hrl").

-compile(export_all).

db_location() ->
    code:priv_dir(dd)++"/dd.db".

open_db() ->
    {ok, DB} = sqlite3:open(db, [{file, db_location()}]),
    DB.

restart() ->
    %% the supervisor will restart this immediately.
    sqlite3:close(dd_helpers:get_db_pid()).

%%===========
%% Activity
%%===========

store_activity(Pid, Nick, Channel, Msg) when is_binary(Nick) ->
    store_activity(Pid, binary_to_list(Nick), Channel, Msg);
store_activity(Pid, Nick, Channel, Msg) ->
    Ts = dd_helpers:utc2sec(),
    sqlite3:sql_exec(Pid, io_lib:format("INSERT OR REPLACE INTO activity (nick, tstamp, channel, msg) VALUES (\"~s\",~p,\"~s\",\"~s\");", [Nick, Ts, binary_to_list(Channel), Msg])),
    ok.

last_activity_for_nick(Pid, Nick) when is_binary(Nick) ->
    last_activity_for_nick(Pid, binary_to_list(Nick));
last_activity_for_nick(Pid, Nick) ->
    Resp = sqlite3:sql_exec(Pid, "SELECT * FROM activity WHERE nick='"++Nick++"'"),
    Resp.

%%===========
%% URL stuff
%%===========

store_url(Pid, Nick, Url, Title) ->
    case sqlite3:write(Pid, urls, [{tstamp, dd_helpers:utc2sec()}, {nick, Nick},{url, Url},{title, Title}]) of
        {error, Nr, Msg} ->
            io:format("Error: ~p -> ~s~n",[Nr, Msg]);
        _ ->
            ok
    end.

-spec search_for_url(pid(), string()) -> [string()].
search_for_url(Pid, SearchSpec) when is_binary(SearchSpec) ->
    search_for_url(Pid, binary_to_list(SearchSpec), 0);
search_for_url(Pid, SearchSpec) ->
    search_for_url(Pid, SearchSpec, 0).

-spec search_for_url(pid(), string(), integer()) -> [string()].
search_for_url(Pid, SearchSpec, Offset) when is_binary(SearchSpec) ->
    search_for_url(Pid, binary_to_list(SearchSpec), Offset);
search_for_url(Pid, SearchSpec, Offset) ->
    [{columns,_},{rows,Rows}] = sqlite3:sql_exec(Pid, "SELECT * FROM urls WHERE url LIKE '%"++SearchSpec++"%' LIMIT 3 OFFSET "++integer_to_list(Offset)++";"),
    [ io_lib:format("~p (posted by ~p on ~p): ~p~n",[U, N, dd_helpers:format_time(dd_helpers:sec2utc(T)), T]) || {T, N, U, T} <- Rows ].

is_database_empty(Pid) ->
    [{columns, ["name"]}, {rows, Lst}] = sqlite3:sql_exec(Pid, "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"),
    case Lst of
        [] -> true;
        _ -> false
    end.

%%============
%% DB helpers
%%============

prepare_database(Db) ->
    %%    @type table_info() = [{atom(), sql_type()} | {atom(), sql_type(), column_constraints()}].
    %%    @type sql_type() = integer | text | double | blob | atom() | string().
    %%    @type table_id() = atom() | binary() | string()
    %% @type table_constraint() = {primary_key, [atom()]} | {unique, [atom()]}.
    %% @type table_constraints() = table_constraint() | [table_constraint()].
    %%
    %% Currently supported constraints for {@link table_info()} and {@link sqlite3:create_table/4}.
    %% @end
    %% what do I want to track?
    %% - join/part times
    %% - talk times .. lots of TIMES :)
    %% Using integer type for date/time format.
    %% dd_helpers:utc2sec/0 and dd_helpers:sec2utc/1
    %% need to check whether we actually already have initialized the database.
    case is_database_empty(Db) of
        true ->
            create_versions_table_if_needed(Db),
            sqlite3:create_table(Db, urls, [{tstamp, integer}, {nick, text}, {url, text}, {title, text}], {unique, [tstamp]}),
            sqlite3:create_table(Db, activity, [{nick, text}, {tstamp, integer}, {channel, text}, {msg, text}], {unique, [nick]}),
            create_versions_table_if_needed(Db);
        false ->
            ok
    end.

table_exists(Db, Table) ->
    lists:member(Table, sqlite3:list_tables(Db)).

%%=============================
%% versions stuff
%%=============================
create_versions_table_if_needed(Db) ->
    case table_exists(Db, versions) of
        true -> ok;
        false ->
            sqlite3:create_table(Db, versions, [{nick, text}, {versionreply, text}, {tstamp, integer}], {unique, [nick]})
    end.

store_version(Db, Nick, Version) when is_binary(Nick) ->
    store_version(Db, binary_to_list(Nick), Version);
store_version(Db, Nick, Version) when is_binary(Version) ->
    store_version(Db, Nick, binary_to_list(Version));
store_version(Db, Nick, Version) ->
    sqlite3:sql_exec(Db, io_lib:format("INSERT OR REPLACE INTO versions (nick, tstamp, versionreply) VALUES (\"~s\",~p,\"~s\");", [Nick, dd_helpers:utc2sec(), Version])).

get_version_for_nick(Db, Nick) when is_binary(Nick)->
    get_version_for_nick(Db, binary_to_list(Nick));
get_version_for_nick(Db, Nick) ->
    case sqlite3:sql_exec(Db, "SELECT * FROM versions WHERE nick='"++Nick++"'") of
        [{columns,_},{rows,[{_, Version, Tstamp}]}] ->
            {Version, Tstamp};
        [{columns,_},{rows,[]}] ->
            undefined
    end.

list_versions_table() ->
    list_versions_table(dd_helpers:get_db_pid()).
list_versions_table(Db) ->
    Res = sqlite3:sql_exec(Db, "SELECT * FROM versions"),
    io:format("~p~n",[Res]).
