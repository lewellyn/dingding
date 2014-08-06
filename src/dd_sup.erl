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

-module(dd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% for later.
 	%% yaws:start_embedded(code:priv_dir(dd),	
    %%                     [{servername, "sej"}, {auth_log, false},
    %%                      {listen, {0,0,0,0}},{port, 8888}, {appmods, [{"/", dd_web}]}],
    %%                     [{copy_errlog, false}]),
    DB = {dd_db, {sqlite3, start_link, [dd_db, [{file, dd_sql:db_location()}]]}, permanent, 2000, worker, [sqlite3]},
	Twt = {dd_twitter, {dd_twitter, periodic_get_mentions, []}, permanent, 2000, worker, [dd_twitter]},
    {ok, { {one_for_one, 5, 10}, [DB, Twt, ?CHILD(dd_connection_sup, supervisor)]} }.

    
