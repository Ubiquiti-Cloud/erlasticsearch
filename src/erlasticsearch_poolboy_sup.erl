%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
-module(erlasticsearch_poolboy_sup).

-behaviour(supervisor).

-include("erlasticsearch.hrl").

-export([start_link/0]).
-export([start_pool/3]).
-export([stop_pool/1]).

-export([init/1]).

-define(WORKER(Restart, Module, Args), {Module, {Module, start_link, Args}, Restart, 5000, worker, [Module]}).
-define(SUPERVISOR(Restart, Module, Args), {Module, {Module, start_link, Args}, Restart, 5000, supervisor, [Module]}).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_pool(pool_name(), params(), params()) -> supervisor:startchild_ret().
start_pool(PoolName, PoolOptions, ConnectionOptions) when is_list(PoolOptions),
                                                          is_list(ConnectionOptions) ->
    PoolSpec = pool_spec(PoolName, PoolOptions, ConnectionOptions),
    supervisor:start_child(?MODULE, PoolSpec).


-spec stop_pool(pool_name()) -> ok | error().
stop_pool(PoolName) ->
    supervisor:terminate_child(?MODULE, PoolName),
    supervisor:delete_child(?MODULE, PoolName).


-spec init(Args :: term()) -> {ok, {{RestartStrategy :: supervisor:strategy(), MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
                                    [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

-spec pool_spec(pool_name(), params(), params()) -> supervisor:child_spec().
pool_spec(PoolName, PoolOptions, ConnectionOptions) ->
    PoolArgs = [{name, {local, PoolName}},
                {worker_module, erlasticsearch_worker}] ++ PoolOptions,
    poolboy:child_spec(PoolName, PoolArgs, ConnectionOptions).
