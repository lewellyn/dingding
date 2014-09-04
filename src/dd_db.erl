%% Copyright 2014 Gert Meulyzer
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at

%%     http://www.apache.org/licenses/LICENSE-2.0

%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @doc
%%% Database access
%%% @end
%%% Created :  4 Sep 2014 by Gert Meulyzer <@G3rtm on Twitter>

-module(dd_db).
-compile(export_all).

-record(dd_versions, {nick, versionreply, timestamp}).
-record(dd_urls, {url, nick, timestamp, title}).
-record(dd_activity, {nick, timestamp, channel, msg}).

init() ->
	application:set_env(mnesia, dir, code:priv_dir(dd)),
	mnesia:start(),
	Tables = mnesia:system_info(tables),
	case {lists:member(dd_versions, Tables),
		  lists:member(dd_urls, Tables),
		  lists:member(dd_activity, Tables)} of
		{true,true,true} -> ok;
		_ -> create_tables()
	end,
	application:start(mnesia),
	mnesia:wait_for_tables([dd_versions, dd_urls, dd_activity], 10000).

create_tables() ->
	application:stop(mnesia),
	mnesia:delete_schema([node()]),
	mnesia:create_schema([node()]),
	application:start(mnesia),
	mnesia:create_table([node()]),
	Versions = mnesia:create_table(dd_versions, 
								   [{attributes, record_info(fields, dd_versions)},
									{disc_only_copies, [node()]}]),
	Urls = mnesia:create_table(dd_urls, 
							   [{attributes, record_info(fields, dd_urls)},
								{disc_only_copies, [node()]}]),
	Activity = mnesia:create_table(dd_activity, 
								   [{attributes, record_info(fields, dd_activity)},
									{disc_only_copies, [node()]}]),
	application:stop(mnesia),
	{Versions, Urls, Activity}.
	
