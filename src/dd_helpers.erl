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
%%% General helpers.
%%% @end
%%% Created :  2 Dec 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_helpers).
-export([get_urls/1, http_get/1, http_head/1, module_exists/1, utc2sec/0, sec2utc/1, get_db_pid/0, consult_priv_dir_file/1]).
-export([format_time/0, format_time/1, iso_8601_fmt/1, ppsec2utc/1]).
-export([add_item_to_proplist_value/3, remove_item_from_proplist_value/3]).
-export([diff2now/1, pptimediff/1]).
-export([request_version/2, request_whois/2]).
-export([ip_to_int/1, int_to_ip/1]).
-export([bin_to_int/1, int_to_bin/1]).
-export([get_first_public_ip/0, remove_crnl/1, cleanup_html_entities/1, trim_ws/1]).

-spec consult_priv_dir_file(string()) -> any().
consult_priv_dir_file(Filename) ->
    PrivDir = code:lib_dir(dd, priv),    
    {ok, Terms, _} = file:path_consult([PrivDir], Filename),
    Terms.


-spec get_urls(binary()) -> [binary()].
get_urls(Bin) ->
    Parts = binary:split(Bin, <<" ">>, [global, trim]),
    lists:flatten([ case Part of
                        <<"http://", _/binary>> -> Part;
                        <<"https://", _/binary>> -> Part;
                        <<"<http://", _/binary>> -> binary:part(Part, 1, byte_size(Part)-2);
                        <<"<https://", _/binary>> -> binary:part(Part, 1, byte_size(Part)-2);
                       _ -> []
                   end || Part <- Parts ]).

-spec http_get(string()) -> any().
http_get(Url) ->
    httpc:request(get, {Url, [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0"}]}, [{autoredirect, true}, {relaxed, true}], []).

-spec http_head(string()) -> any().
http_head(Url) ->
    httpc:request(head, {Url, [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0"}]}, [{autoredirect, true}, {relaxed, true}], []).

-spec module_exists(module()) -> boolean().
module_exists(Module) ->
    case is_atom(Module) of
        true ->
            try Module:module_info() of
                _InfoList ->
                    true
            catch
                _:_ ->
                    false
            end;
        false ->
            false
    end.

utc2sec() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

sec2utc(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds).

diff2now(Seconds) ->
    Now = dd_helpers:utc2sec(),
    R = Now - Seconds,
    calendar:seconds_to_daystime(R).

%% io_lib:format("~2..0B", [42]),
pptimediff({Days, {Hours, Minutes, Seconds}}) ->
    io_lib:format("~s~s~s~s",[timestr(days, Days), timestr(hours, Hours), timestr(minutes, Minutes), timestr(seconds, Seconds)]).

timestr(seconds, 0) ->
    "exactly";
timestr(_, 0) ->
    "";
timestr(minutes, 1) ->
    "1 minute, ";
timestr(minutes, X) ->
    io_lib:format("~p minutes, ",[X]);
timestr(seconds, 1) ->
    "1 second, ";
timestr(seconds, X) ->
    io_lib:format("~p seconds ",[X]);
timestr(hours, 1) ->
    "1 hour, ";
timestr(hours, X) ->
    io_lib:format("~p hours, ",[X]);
timestr(days, 1) ->
    "a day, ";
timestr(days, X) ->
    io_lib:format("~p days, ",[X]).

ppsec2utc(Seconds) ->
    iso_8601_fmt(sec2utc(Seconds)).

get_db_pid() ->
    case whereis(dd_db) of
        undefined ->
            {dd_db,Pid,_,_} = hd(lists:filter(fun(X) -> is_db_process(X) end, supervisor:which_children(dd_sup))),
            register(dd_db, Pid),
            Pid;
        Pid -> Pid
    end.

is_db_process({dd_db,_,_,_}) -> true;
is_db_process(_) -> false.

localtime_ms() ->
    {_, _, Micro} = Now = os:timestamp(),
    {Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    {Date, {Hours, Minutes, Seconds, Micro div 1000 rem 1000}}.

format_time() ->
    format_time(localtime_ms()).

format_time({{Y, M, D}, {H, Mi, S, Ms}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
     io_lib:format("~2..0b:~2..0b:~2..0b.~3..0b", [H, Mi, S, Ms])};
format_time({{Y, M, D}, {H, Mi, S}}) ->
    {io_lib:format("~b-~2..0b-~2..0b", [Y, M, D]),
     io_lib:format("~2..0b:~2..0b:~2..0b", [H, Mi, S])}.

iso_8601_fmt(DateTime) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]).

%% @doc
%% When you have a proplist that contains {string(), list(any())} items, you can use this function
%% to add items to the list(any()).
%% This uses lists:append to make sure it gets appended to the end of the list.
%% Since we will use this to add modules to a specific channel configuration, I want that module to be run
%% after the original modules.
%% @end
add_item_to_proplist_value(Item, PropList, Key) ->
    case proplists:get_value(Key, PropList) of
        undefined -> PropList;
        Value ->
            [ {Key, lists:append(Value,[Item])}  |proplists:delete(Key, PropList)]
    end.

%% @doc
%% This does the reverse of the adding function.
%% @end 
remove_item_from_proplist_value(Item, PropList, Key) ->
    case proplists:get_value(Key, PropList) of
        undefined -> PropList;
        Value ->
            [ {Key, lists:delete(Item, Value)} | proplists:delete(Key, PropList)]
    end.

ip_to_int({A,B,C,D}) -> (A*16777216)+(B*65536)+(C*256)+(D).
int_to_ip(Ip)->	{Ip bsr 24, (Ip band 16711680) bsr 16, (Ip band 65280) bsr 8, Ip band 255}.


%% @doc
%% Function to go from <<"5">> to 5
%% @end
-spec bin_to_int(X :: binary()) -> integer().
bin_to_int(X) ->
    list_to_integer(binary_to_list(X)).
%% @doc
%% Function to go from 5 to <<"5">>.
%% Perhaps not the best name, but in the context of this bot, it'll work.
%% @end
-spec int_to_bin(X :: binary()) -> integer().
int_to_bin(X) ->
    list_to_binary(integer_to_list(X)).

-spec get_first_public_ip() -> {integer(), integer(), integer(), integer()}.
get_first_public_ip() ->
    {ok, IPs} = inet:getif(),
    {IP, _, _} = hd(lists:filter(fun is_not_local_ip/1, IPs)),
    IP.

is_not_local_ip({{127,0,0,1},_,_}) ->
    false;
is_not_local_ip(_) ->
    true.

-spec remove_crnl(binary()) -> binary().
remove_crnl(Bin) ->
    [ X || <<X>> <= Bin,
           X =/= 13,
           X =/= 10 ].

cleanup_html_entities(Line) ->
    {ok, LF} = re:compile("\r", [caseless]),
    {ok, NL} = re:compile("\n", [caseless]),
    {ok, Gt} = re:compile("&gt;", [caseless]),
    {ok, Lt} = re:compile("&lt;", [caseless]),
    {ok, Amp} = re:compile("&amp;", [caseless]),
    {ok, Quot} = re:compile("&quot;", [caseless]),
    {ok, Apos} = re:compile("&apos;", [caseless]),
    lists:foldl(fun({Rgx,Repl}, Acc) -> 
                        re:replace(Acc, Rgx, Repl, [{return, list}, global]) 
                end, 
                Line, 
                [{Amp, "\\&"},
                 {Gt, ">"},
                 {Lt, "<"},
                 {Quot, "\""},
                 {Apos, "'"},
                 {LF, " "},
                 {NL, " "}
                 ]).

trim_ws(Bin) ->
    re:replace(Bin, "^\\s+|\\s+$", "", [{return, binary}, global]).

%% ================================
%% IRC PROTOCOL HELPERS
%% ================================

-spec request_version(ReplyPid :: pid(), Nick :: binary()) -> ok.
request_version(ReplyPid, Nick) ->
    %% needs to be some kind of caching here, so it only requests this once every 24 hours or so.
    dd_connection:send_msg(ReplyPid, <<>>, <<"CTCP">>, [Nick], <<"VERSION">>),
    ok.
-spec request_whois(ReplyPid :: pid(), Nick :: binary()) -> ok.
request_whois(ReplyPid, Nick) ->
    %% the response to this will be caught in the numeric reply handlers.
    dd_connection:send_msg(ReplyPid, <<>>, <<"WHOIS">>, [Nick], Nick),
    ok.
