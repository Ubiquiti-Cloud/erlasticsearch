%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc type definitions and records.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erlasticsearch).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-include("erlasticsearch.hrl").

-export([start/0, start/1]).
-export([stop/0, stop/1]).
-export([stop_pool/1]).
-export([start_pool/1, start_pool/2, start_pool/3]).
-export([get_env/2, set_env/2]).

-export([is_index/2, is_index/3]).
-export([is_type/3, is_type/4]).
-export([is_doc/4, is_doc/5]).

-export([health/1, health/2]).
-export([cluster_state/1, cluster_state/2, cluster_state/3]).
-export([state/1, state/2, state/3]).
-export([nodes_info/1, nodes_info/2, nodes_info/3, nodes_info/4]).
-export([nodes_stats/1, nodes_stats/2, nodes_stats/3, nodes_stats/4]).

-export([create_index/2, create_index/3, create_index/4]).
-export([delete_index/1, delete_index/2, delete_index/3]).
-export([open_index/2, open_index/3]).
-export([close_index/2, close_index/3]).

-export([insert_doc/5, insert_doc/6, insert_doc/7]).
-export([update_doc/5, update_doc/6, update_doc/7]).
-export([get_doc/4, get_doc/5, get_doc/6]).
-export([mget_doc/2, mget_doc/3, mget_doc/4, mget_doc/5]).
-export([delete_doc/4, delete_doc/5, delete_doc/6]).
-export([search/4, search/5, search/6]).
-export([count/2, count/3, count/4, count/5, count/6]).
-export([delete_by_query/2, delete_by_query/3, delete_by_query/4, delete_by_query/5, delete_by_query/6]).
-export([bulk/2, bulk/3, bulk/4, bulk/5]).

-export([status/2, status/3]).
-export([indices_stats/2, indices_stats/3]).
-export([refresh/1, refresh/2, refresh/3]).
-export([flush/1, flush/2, flush/3]).
-export([optimize/1, optimize/2, optimize/3]).
-export([clear_cache/1, clear_cache/2, clear_cache/3, clear_cache/4]).
-export([segments/1, segments/2, segments/3]).

-export([put_mapping/4, put_mapping/5]).
-export([get_mapping/3, get_mapping/4]).
-export([delete_mapping/3, delete_mapping/4]).

-export([aliases/2, aliases/3]).
-export([insert_alias/3, insert_alias/4, insert_alias/5]).
-export([delete_alias/3, delete_alias/4]).
-export([is_alias/3, is_alias/4]).
-export([get_alias/3, get_alias/4]).

-export([join/2]).

-define(APP, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).

-spec start() -> ok.
start() ->
    reltool_util:application_start(?APP).

-spec start(params()) -> {ok, pid()}.
start(Options) when is_list(Options) ->
    gen_server:start(?MODULE, [Options], []).

-spec stop() -> ok.
stop() ->
    reltool_util:application_stop(?APP).

-spec stop(pid()) -> ok | error().
stop(ServerRef) ->
    gen_server:call(ServerRef, {stop}, ?DEFAULT_TIMEOUT).

-spec start_pool(pool_name()) -> supervisor:startchild_ret().
start_pool(PoolName) ->
    PoolOptions = application:get_env(erlasticsearch, pool_options, ?DEFAULT_POOL_OPTIONS),
    ConnectionOptions = application:get_env(erlasticsearch, connection_options, ?DEFAULT_CONNECTION_OPTIONS),
    start_pool(PoolName, PoolOptions, ConnectionOptions).

-spec start_pool(pool_name(), params()) -> supervisor:startchild_ret().
start_pool(PoolName, PoolOptions) when is_list(PoolOptions) ->
    ConnectionOptions = application:get_env(erlasticsearch, connection_options, ?DEFAULT_CONNECTION_OPTIONS),
    start_pool(PoolName, PoolOptions, ConnectionOptions).

-spec start_pool(pool_name(), params(), params()) -> supervisor:startchild_ret().
start_pool(PoolName, PoolOptions, ConnectionOptions) when is_list(PoolOptions),
                                                          is_list(ConnectionOptions) ->
    erlasticsearch_poolboy_sup:start_pool(PoolName, PoolOptions, ConnectionOptions).

-spec stop_pool(pool_name()) -> ok | error().
stop_pool(PoolName) ->
    erlasticsearch_poolboy_sup:stop_pool(PoolName).

-spec health(pool_name()) -> response().
health(PoolName) ->
    health(PoolName, ?DEFAULT_TIMEOUT).

-spec health(pool_name(), integer()) -> response().
health(PoolName, Timeout) when is_integer(Timeout) ->
    pool_call(PoolName, {health}, Timeout).

-spec cluster_state(pool_name()) -> response().
cluster_state(PoolName) ->
    cluster_state(PoolName, []).

-spec cluster_state(pool_name(), params()) -> response().
cluster_state(PoolName, Params) when is_list(Params) ->
    cluster_state(PoolName, Params, ?DEFAULT_TIMEOUT).

-spec cluster_state(pool_name(), params(), integer()) -> response().
cluster_state(PoolName, Params, Timeout) when is_list(Params), is_integer(Timeout) ->
    state(PoolName, Params, Timeout).

-spec state(pool_name()) -> response().
state(PoolName) ->
    state(PoolName, []).

-spec state(pool_name(), params()) -> response().
state(PoolName, Params) when is_list(Params) ->
    state(PoolName, Params, ?DEFAULT_TIMEOUT).

-spec state(pool_name(), params(), integer()) -> response().
state(PoolName, Params, Timeout) when is_list(Params), is_integer(Timeout) ->
    pool_call(PoolName, {state, Params}, Timeout).

-spec nodes_info(pool_name()) -> response().
nodes_info(PoolName) ->
    nodes_info(PoolName, [], []).

-spec nodes_info(pool_name(), node_name()) -> response().
nodes_info(PoolName, NodeName) when is_binary(NodeName) ->
    nodes_info(PoolName, [NodeName], []);
nodes_info(PoolName, NodeNames) when is_list(NodeNames) ->
    nodes_info(PoolName, NodeNames, []).

-spec nodes_info(pool_name(), [node_name()], params()) -> response().
nodes_info(PoolName, NodeNames, Params) when is_list(NodeNames), is_list(Params) ->
    nodes_info(PoolName, NodeNames, Params, ?DEFAULT_TIMEOUT).

-spec nodes_info(pool_name(), [node_name()], params(), integer()) -> response().
nodes_info(PoolName, NodeNames, Params, Timeout) when is_list(NodeNames), is_list(Params), is_integer(Timeout) ->
    pool_call(PoolName, {nodes_info, NodeNames, Params}, Timeout).

-spec nodes_stats(pool_name()) -> response().
nodes_stats(PoolName) ->
    nodes_stats(PoolName, [], []).

-spec nodes_stats(pool_name(), node_name()) -> response().
nodes_stats(PoolName, NodeName) when is_binary(NodeName) ->
    nodes_stats(PoolName, [NodeName], []);
nodes_stats(PoolName, NodeNames) when is_list(NodeNames) ->
    nodes_stats(PoolName, NodeNames, []).

-spec nodes_stats(pool_name(), [node_name()], params()) -> response().
nodes_stats(PoolName, NodeNames, Params) when is_list(NodeNames), is_list(Params) ->
    nodes_stats(PoolName, NodeNames, Params, ?DEFAULT_TIMEOUT).

-spec nodes_stats(pool_name(), [node_name()], params(), integer()) -> response().
nodes_stats(PoolName, NodeNames, Params, Timeout) when is_list(NodeNames), is_list(Params), is_integer(Timeout) ->
    pool_call(PoolName, {nodes_stats, NodeNames, Params}, Timeout).

-spec status(pool_name(), index() | [index()]) -> response().
status(PoolName, Index) when is_binary(Index) ->
    status(PoolName, [Index]);
status(PoolName, Indexes) when is_list(Indexes)->
    status(PoolName, Indexes, ?DEFAULT_TIMEOUT).

-spec status(pool_name(), index() | [index()], integer()) -> response().
status(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    status(PoolName, [Index], Timeout);
status(PoolName, Indexes, Timeout) when is_list(Indexes), is_integer(Timeout) ->
    pool_call(PoolName, {status, Indexes}, Timeout).

-spec indices_stats(pool_name(), index() | [index()]) -> response().
indices_stats(PoolName, Index) when is_binary(Index) ->
    indices_stats(PoolName, [Index]);
indices_stats(PoolName, Indexes) when is_list(Indexes)->
    indices_stats(PoolName, Indexes, ?DEFAULT_TIMEOUT).

-spec indices_stats(pool_name(), index() | [index()], integer()) -> response().
indices_stats(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    indices_stats(PoolName, [Index], Timeout);
indices_stats(PoolName, Indexes, Timeout) when is_list(Indexes), is_integer(Timeout) ->
    pool_call(PoolName, {indices_stats, Indexes}, Timeout).

-spec create_index(pool_name(), index()) -> response().
create_index(PoolName, Index) when is_binary(Index) ->
    create_index(PoolName, Index, <<>>).

-spec create_index(pool_name(), index(), doc()) -> response().
create_index(PoolName, Index, Doc) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    create_index(PoolName, Index, Doc, ?DEFAULT_TIMEOUT).

-spec create_index(pool_name(), index(), doc(), integer()) -> response().
create_index(PoolName, Index, Doc, Timeout) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_integer(Timeout) ->
    pool_call(PoolName, {create_index, Index, Doc}, Timeout).

-spec delete_index(pool_name()) -> response().
delete_index(PoolName) ->
    delete_index(PoolName, ?ALL).

-spec delete_index(pool_name(), index() | [index()]) -> response().
delete_index(PoolName, Index) when is_binary(Index) ->
    delete_index(PoolName, [Index]);
delete_index(PoolName, Index) when is_list(Index) ->
    delete_index(PoolName, Index, ?DEFAULT_TIMEOUT).

-spec delete_index(pool_name(), index() | [index()], integer()) -> response().
delete_index(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    delete_index(PoolName, [Index], Timeout);
delete_index(PoolName, Index, Timeout) when is_list(Index), is_integer(Timeout) ->
    pool_call(PoolName, {delete_index, Index}, Timeout).

-spec open_index(pool_name(), index()) -> response().
open_index(PoolName, Index) when is_binary(Index) ->
    open_index(PoolName, Index, ?DEFAULT_TIMEOUT).

-spec open_index(pool_name(), index(), integer()) -> response().
open_index(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    pool_call(PoolName, {open_index, Index}, Timeout).

-spec close_index(pool_name(), index()) -> response().
close_index(PoolName, Index) when is_binary(Index) ->
    close_index(PoolName, Index, ?DEFAULT_TIMEOUT).

-spec close_index(pool_name(), index(), integer()) -> response().
close_index(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    pool_call(PoolName, {close_index, Index}, Timeout).

-spec is_index(pool_name(), index() | [index()]) -> response().
is_index(PoolName, Index) when is_binary(Index) ->
    is_index(PoolName, [Index]);
is_index(PoolName, Indexes) when is_list(Indexes) ->
    is_index(PoolName, Indexes, ?DEFAULT_TIMEOUT).

-spec is_index(pool_name(), index() | [index()], integer()) -> response().
is_index(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    is_index(PoolName, [Index], Timeout);
is_index(PoolName, Indexes, Timeout) when is_list(Indexes), is_integer(Timeout) ->
    pool_call(PoolName, {is_index, Indexes}, Timeout).

-spec count(pool_name(), doc()) -> response().
count(PoolName, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    count(PoolName, ?ALL, [], Doc, []).

-spec count(pool_name(), doc(), params()) -> response().
count(PoolName, Doc, Params) when (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(PoolName, ?ALL, [], Doc, Params).

-spec count(pool_name(), index() | [index()], doc(), params()) -> response().
count(PoolName, Index, Doc, Params) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(PoolName, [Index], [], Doc, Params);
count(PoolName, Indexes, Doc, Params) when is_list(Indexes) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(PoolName, Indexes, [], Doc, Params).

-spec count(pool_name(), index() | [index()], type() | [type()], doc(), params()) -> response().
count(PoolName, Index, Type, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(PoolName, [Index], [Type], Doc, Params);
count(PoolName, Indexes, Type, Doc, Params) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(PoolName, Indexes, [Type], Doc, Params);
count(PoolName, Index, Types, Doc, Params) when is_binary(Index) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(PoolName, [Index], Types, Doc, Params);
count(PoolName, Indexes, Types, Doc, Params) when is_list(Indexes) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(PoolName, Indexes, Types, Doc, Params, ?DEFAULT_TIMEOUT).

-spec count(pool_name(), index() | [index()], type() | [type()], doc(), params(), integer()) -> response().
count(PoolName, Index, Type, Doc, Params, Timeout) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    count(PoolName, [Index], [Type], Doc, Params, Timeout);
count(PoolName, Indexes, Type, Doc, Params, Timeout) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    count(PoolName, Indexes, [Type], Doc, Params, Timeout);
count(PoolName, Index, Types, Doc, Params, Timeout) when is_binary(Index) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    count(PoolName, [Index], Types, Doc, Params, Timeout);
count(PoolName, Indexes, Types, Doc, Params, Timeout) when is_list(Indexes) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    pool_call(PoolName, {count, Indexes, Types, Doc, Params}, Timeout).

-spec delete_by_query(pool_name(), doc()) -> response().
delete_by_query(PoolName, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    delete_by_query(PoolName, ?ALL, [], Doc, []).

-spec delete_by_query(pool_name(), doc(), params()) -> response().
delete_by_query(PoolName, Doc, Params) when (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    delete_by_query(PoolName, ?ALL, [], Doc, Params).

-spec delete_by_query(pool_name(), index() | [index()], doc(), params()) -> response().
delete_by_query(PoolName, Index, Doc, Params) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(PoolName, [Index], [], Doc, Params);
delete_by_query(PoolName, Indexes, Doc, Params) when is_list(Indexes) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(PoolName, Indexes, [], Doc, Params).

-spec delete_by_query(pool_name(), index() | [index()], type() | [type()], doc(), params()) -> response().
delete_by_query(PoolName, Index, Type, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(PoolName, [Index], [Type], Doc, Params);
delete_by_query(PoolName, Indexes, Type, Doc, Params) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(PoolName, Indexes, [Type], Doc, Params);
delete_by_query(PoolName, Index, Types, Doc, Params) when is_binary(Index) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(PoolName, [Index], Types, Doc, Params);
delete_by_query(PoolName, Indexes, Types, Doc, Params) when is_list(Indexes) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(PoolName, Indexes, Types, Doc, Params, ?DEFAULT_TIMEOUT).

-spec delete_by_query(pool_name(), index() | [index()], type() | [type()], doc(), params(), integer()) -> response().
delete_by_query(PoolName, Index, Type, Doc, Params, Timeout) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    delete_by_query(PoolName, [Index], [Type], Doc, Params, Timeout);
delete_by_query(PoolName, Indexes, Type, Doc, Params, Timeout) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    delete_by_query(PoolName, Indexes, [Type], Doc, Params, Timeout);
delete_by_query(PoolName, Index, Types, Doc, Params, Timeout) when is_binary(Index) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    delete_by_query(PoolName, [Index], Types, Doc, Params, Timeout);
delete_by_query(PoolName, Indexes, Types, Doc, Params, Timeout) when is_list(Indexes) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    pool_call(PoolName, {delete_by_query, Indexes, Types, Doc, Params}, Timeout).

-spec is_type(pool_name(), index() | [index()], type() | [type()]) -> response().
is_type(PoolName, Index, Type) when is_binary(Index), is_binary(Type) ->
    is_type(PoolName, [Index], [Type]);
is_type(PoolName, Indexes, Type) when is_list(Indexes), is_binary(Type) ->
    is_type(PoolName, Indexes, [Type]);
is_type(PoolName, Index, Types) when is_binary(Index), is_list(Types) ->
    is_type(PoolName, [Index], Types);
is_type(PoolName, Indexes, Types) when is_list(Indexes), is_list(Types) ->
    is_type(PoolName, Indexes, Types, ?DEFAULT_TIMEOUT).

-spec is_type(pool_name(), index() | [index()], type() | [type()], integer()) -> response().
is_type(PoolName, Index, Type, Timeout) when is_binary(Index), is_binary(Type), is_integer(Timeout) ->
    is_type(PoolName, [Index], [Type], Timeout);
is_type(PoolName, Indexes, Type, Timeout) when is_list(Indexes), is_binary(Type), is_integer(Timeout) ->
    is_type(PoolName, Indexes, [Type], Timeout);
is_type(PoolName, Index, Types, Timeout) when is_binary(Index), is_list(Types), is_integer(Timeout) ->
    is_type(PoolName, [Index], Types, Timeout);
is_type(PoolName, Indexes, Types, Timeout) when is_list(Indexes), is_list(Types), is_integer(Timeout) ->
    pool_call(PoolName, {is_type, Indexes, Types}, Timeout).

-spec insert_doc(pool_name(), index(), type(), id(), doc()) -> response().
insert_doc(PoolName, Index, Type, Id, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    insert_doc(PoolName, Index, Type, Id, Doc, []).

-spec insert_doc(pool_name(), index(), type(), id(), doc(), params()) -> response().
insert_doc(PoolName, Index, Type, Id, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    insert_doc(PoolName, Index, Type, Id, Doc, Params, ?DEFAULT_TIMEOUT).

-spec insert_doc(pool_name(), index(), type(), id(), doc(), params(), integer()) -> response().
insert_doc(PoolName, Index, Type, Id, Doc, Params, Timeout) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    pool_call(PoolName, {insert_doc, Index, Type, Id, Doc, Params}, Timeout).

-spec update_doc(pool_name(), index(), type(), id(), doc()) -> response().
update_doc(PoolName, Index, Type, Id, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    update_doc(PoolName, Index, Type, Id, Doc, []).

-spec update_doc(pool_name(), index(), type(), id(), doc(), params()) -> response().
update_doc(PoolName, Index, Type, Id, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    update_doc(PoolName, Index, Type, Id, Doc, Params, ?DEFAULT_TIMEOUT).

-spec update_doc(pool_name(), index(), type(), id(), doc(), params(), integer()) -> response().
update_doc(PoolName, Index, Type, Id, Doc, Params, Timeout) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    pool_call(PoolName, {update_doc, Index, Type, Id, Doc, Params}, Timeout).

-spec is_doc(pool_name(), index(), type(), id()) -> response().
is_doc(PoolName, Index, Type, Id) when is_binary(Index), is_binary(Type) ->
    is_doc(PoolName, Index, Type, Id, ?DEFAULT_TIMEOUT).

-spec is_doc(pool_name(), index(), type(), id(), integer()) -> response().
is_doc(PoolName, Index, Type, Id, Timeout) when is_binary(Index), is_binary(Type), is_integer(Timeout) ->
    pool_call(PoolName, {is_doc, Index, Type, Id}, Timeout).

-spec get_doc(pool_name(), index(), type(), id()) -> response().
get_doc(PoolName, Index, Type, Id) when is_binary(Index), is_binary(Type) ->
    get_doc(PoolName, Index, Type, Id, []).

-spec get_doc(pool_name(), index(), type(), id(), params()) -> response().
get_doc(PoolName, Index, Type, Id, Params) when is_binary(Index), is_binary(Type), is_list(Params)->
    get_doc(PoolName, Index, Type, Id, Params, ?DEFAULT_TIMEOUT).

-spec get_doc(pool_name(), index(), type(), id(), params(), integer()) -> response().
get_doc(PoolName, Index, Type, Id, Params, Timeout) when is_binary(Index), is_binary(Type), is_list(Params), is_integer(Timeout) ->
    pool_call(PoolName, {get_doc, Index, Type, Id, Params}, Timeout).

-spec mget_doc(pool_name(), doc()) -> response().
mget_doc(PoolName, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    mget_doc(PoolName, <<>>, <<>>, Doc).

-spec mget_doc(pool_name(), index(), doc()) -> response().
mget_doc(PoolName, Index, Doc) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc))->
    mget_doc(PoolName, Index, <<>>, Doc).

-spec mget_doc(pool_name(), index(), type(), doc()) -> response().
mget_doc(PoolName, Index, Type, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc))->
    mget_doc(PoolName, Index, Type, Doc, ?DEFAULT_TIMEOUT).

-spec mget_doc(pool_name(), index(), type(), doc(), integer()) -> response().
mget_doc(PoolName, Index, Type, Doc, Timeout) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_integer(Timeout) ->
    pool_call(PoolName, {mget_doc, Index, Type, Doc}, Timeout).

-spec delete_doc(pool_name(), index(), type(), id()) -> response().
delete_doc(PoolName, Index, Type, Id) when is_binary(Index), is_binary(Type) ->
    delete_doc(PoolName, Index, Type, Id, []).

-spec delete_doc(pool_name(), index(), type(), id(), params()) -> response().
delete_doc(PoolName, Index, Type, Id, Params) when is_binary(Index), is_binary(Type), is_list(Params)->
    delete_doc(PoolName, Index, Type, Id, Params, ?DEFAULT_TIMEOUT).

-spec delete_doc(pool_name(), index(), type(), id(), params(), integer()) -> response().
delete_doc(PoolName, Index, Type, Id, Params, Timeout) when is_binary(Index), is_binary(Type), is_list(Params), is_integer(Timeout) ->
    pool_call(PoolName, {delete_doc, Index, Type, Id, Params}, Timeout).

-spec search(pool_name(), index(), type(), doc()) -> response().
search(PoolName, Index, Type, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc))->
    search(PoolName, Index, Type, Doc, []).

-spec search(pool_name(), index(), type(), doc(), params()) -> response().
search(PoolName, Index, Type, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    search(PoolName, Index, Type, Doc, Params, ?DEFAULT_TIMEOUT).

-spec search(pool_name(), index(), type(), doc(), params(), integer()) -> response().
search(PoolName, Index, Type, Doc, Params, Timeout) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) andalso is_integer(Timeout) ->
    pool_call(PoolName, {search, Index, Type, Doc, Params}, Timeout).

-spec bulk(pool_name(), doc()) -> response().
bulk(PoolName, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    bulk(PoolName, <<>>, <<>>, Doc).

-spec bulk(pool_name(), index(), doc()) -> response().
bulk(PoolName, Index, Doc) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    bulk(PoolName, Index, <<>>, Doc).

-spec bulk(pool_name(), index(), type(), doc()) -> response().
bulk(PoolName, Index, Type, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    bulk(PoolName, Index, Type, Doc, ?DEFAULT_TIMEOUT).

-spec bulk(pool_name(), index(), type(), doc(), integer()) -> response().
bulk(PoolName, Index, Type, Doc, Timeout) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_integer(Timeout) ->
    pool_call(PoolName, {bulk, Index, Type, Doc}, Timeout).

-spec refresh(pool_name()) -> response().
refresh(PoolName) ->
    refresh(PoolName, ?ALL).

-spec refresh(pool_name(), index() | [index()]) -> response().
refresh(PoolName, Index) when is_binary(Index) ->
    refresh(PoolName, [Index]);
refresh(PoolName, Indexes) when is_list(Indexes) ->
    refresh(PoolName, Indexes, ?DEFAULT_TIMEOUT).

-spec refresh(pool_name(), index() | [index()], integer()) -> response().
refresh(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    refresh(PoolName, [Index], Timeout);
refresh(PoolName, Indexes, Timeout) when is_list(Indexes), is_integer(Timeout) ->
    pool_call(PoolName, {refresh, Indexes}, Timeout).

-spec flush(pool_name()) -> response().
flush(PoolName) ->
    flush(PoolName, ?ALL).

-spec flush(pool_name(), index() | [index()]) -> response().
flush(PoolName, Index) when is_binary(Index) ->
    flush(PoolName, [Index]);
flush(PoolName, Indexes) when is_list(Indexes) ->
    flush(PoolName, Indexes, ?DEFAULT_TIMEOUT).

-spec flush(pool_name(), index() | [index()], integer()) -> response().
flush(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    flush(PoolName, [Index]);
flush(PoolName, Indexes, Timeout) when is_list(Indexes), is_integer(Timeout) ->
    pool_call(PoolName, {flush, Indexes}, Timeout).

-spec optimize(pool_name()) -> response().
optimize(PoolName) ->
    optimize(PoolName, ?ALL).

-spec optimize(pool_name(), index() | [index()]) -> response().
optimize(PoolName, Index) when is_binary(Index) ->
    optimize(PoolName, [Index]);
optimize(PoolName, Indexes) when is_list(Indexes) ->
    optimize(PoolName, Indexes, ?DEFAULT_TIMEOUT).

-spec optimize(pool_name(), index() | [index()], integer()) -> response().
optimize(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    optimize(PoolName, [Index], Timeout);
optimize(PoolName, Indexes, Timeout) when is_list(Indexes), is_integer(Timeout) ->
    pool_call(PoolName, {optimize, Indexes}, Timeout).

-spec segments(pool_name()) -> response().
segments(PoolName) ->
    segments(PoolName, ?ALL).

-spec segments(pool_name(), index() | [index()]) -> response().
segments(PoolName, Index) when is_binary(Index) ->
    segments(PoolName, [Index]);
segments(PoolName, Indexes) when is_list(Indexes) ->
    segments(PoolName, Indexes, ?DEFAULT_TIMEOUT).

-spec segments(pool_name(), index() | [index()], integer()) -> response().
segments(PoolName, Index, Timeout) when is_binary(Index), is_integer(Timeout) ->
    segments(PoolName, [Index], Timeout);
segments(PoolName, Indexes, Timeout) when is_list(Indexes), is_integer(Timeout) ->
    pool_call(PoolName, {segments, Indexes}, Timeout).

-spec clear_cache(pool_name()) -> response().
clear_cache(PoolName) ->
    clear_cache(PoolName, ?ALL, []).

-spec clear_cache(pool_name(), index() | [index()]) -> response().
clear_cache(PoolName, Index) when is_binary(Index) ->
    clear_cache(PoolName, [Index], []);
clear_cache(PoolName, Indexes) when is_list(Indexes) ->
    clear_cache(PoolName, Indexes, []).

-spec clear_cache(pool_name(), index() | [index()], params()) -> response().
clear_cache(PoolName, Index, Params) when is_binary(Index), is_list(Params) ->
    clear_cache(PoolName, [Index], Params);
clear_cache(PoolName, Indexes, Params) when is_list(Indexes), is_list(Params) ->
    clear_cache(PoolName, Indexes, Params, ?DEFAULT_TIMEOUT).

-spec clear_cache(pool_name(), index() | [index()], params(), integer()) -> response().
clear_cache(PoolName, Index, Params, Timeout) when is_binary(Index), is_list(Params), is_integer(Timeout) ->
    clear_cache(PoolName, [Index], Params, Timeout);
clear_cache(PoolName, Indexes, Params, Timeout) when is_list(Indexes), is_list(Params), is_integer(Timeout) ->
    pool_call(PoolName, {clear_cache, Indexes, Params}, Timeout).

-spec put_mapping(pool_name(), index() | [index()], type(), doc()) -> response().
put_mapping(PoolName, Index, Type, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    put_mapping(PoolName, [Index], Type, Doc);
put_mapping(PoolName, Indexes, Type, Doc) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    put_mapping(PoolName, Indexes, Type, Doc, ?DEFAULT_TIMEOUT).

-spec put_mapping(pool_name(), index() | [index()], type(), doc(), integer()) -> response().
put_mapping(PoolName, Index, Type, Doc, Timeout) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_integer(Timeout) ->
    put_mapping(PoolName, [Index], Type, Doc, Timeout);
put_mapping(PoolName, Indexes, Type, Doc, Timeout) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_integer(Timeout) ->
    pool_call(PoolName, {put_mapping, Indexes, Type, Doc}, Timeout).

-spec get_mapping(pool_name(), index() | [index()], type()) -> response().
get_mapping(PoolName, Index, Type) when is_binary(Index) andalso is_binary(Type) ->
    get_mapping(PoolName, [Index], Type);
get_mapping(PoolName, Indexes, Type) when is_list(Indexes) andalso is_binary(Type) ->
    get_mapping(PoolName, Indexes, Type, ?DEFAULT_TIMEOUT).

-spec get_mapping(pool_name(), index() | [index()], type(), integer()) -> response().
get_mapping(PoolName, Index, Type, Timeout) when is_binary(Index) andalso is_binary(Type) andalso is_integer(Timeout) ->
    get_mapping(PoolName, [Index], Type, Timeout);
get_mapping(PoolName, Indexes, Type, Timeout) when is_list(Indexes) andalso is_binary(Type) andalso is_integer(Timeout) ->
    pool_call(PoolName, {get_mapping, Indexes, Type}, Timeout).

-spec delete_mapping(pool_name(), index() | [index()], type()) -> response().
delete_mapping(PoolName, Index, Type) when is_binary(Index) andalso is_binary(Type) ->
    delete_mapping(PoolName, [Index], Type);
delete_mapping(PoolName, Indexes, Type) when is_list(Indexes) andalso is_binary(Type) ->
    delete_mapping(PoolName, Indexes, Type, ?DEFAULT_TIMEOUT).

-spec delete_mapping(pool_name(), index() | [index()], type(), integer()) -> response().
delete_mapping(PoolName, Index, Type, Timeout) when is_binary(Index) andalso is_binary(Type) andalso is_integer(Timeout) ->
    delete_mapping(PoolName, [Index], Type, Timeout);
delete_mapping(PoolName, Indexes, Type, Timeout) when is_list(Indexes) andalso is_binary(Type) andalso is_integer(Timeout) ->
    pool_call(PoolName, {delete_mapping, Indexes, Type}, Timeout).

-spec aliases(pool_name(), doc()) -> response().
aliases(PoolName, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    aliases(PoolName, Doc, ?DEFAULT_TIMEOUT).

-spec aliases(pool_name(), doc(), integer()) -> response().
aliases(PoolName, Doc, Timeout) when (is_binary(Doc) orelse is_list(Doc)) andalso is_integer(Timeout) ->
    pool_call(PoolName, {aliases, Doc}, Timeout).

-spec insert_alias(pool_name(), index(), index()) -> response().
insert_alias(PoolName, Index, Alias) when is_binary(Index) andalso is_binary(Alias) ->
    insert_alias(PoolName, Index, Alias, ?DEFAULT_TIMEOUT).

-spec insert_alias(pool_name(), index(), index(), doc() | integer()) -> response().
insert_alias(PoolName, Index, Alias, Timeout) when is_binary(Index) andalso is_binary(Alias) andalso is_integer(Timeout) ->
    pool_call(PoolName, {insert_alias, Index, Alias}, Timeout);
insert_alias(PoolName, Index, Alias, Doc) when is_binary(Index) andalso is_binary(Alias) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    insert_alias(PoolName, Index, Alias, Doc, ?DEFAULT_TIMEOUT).

-spec insert_alias(pool_name(), index(), index(), doc(), integer()) -> response().
insert_alias(PoolName, Index, Alias, Doc, Timeout) when is_binary(Index) andalso is_binary(Alias) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_integer(Timeout) ->
    pool_call(PoolName, {insert_alias, Index, Alias, Doc}, Timeout).

-spec delete_alias(pool_name(), index(), index()) -> response().
delete_alias(PoolName, Index, Alias) when is_binary(Index) andalso is_binary(Alias) ->
    delete_alias(PoolName, Index, Alias, ?DEFAULT_TIMEOUT).

-spec delete_alias(pool_name(), index(), index(), integer()) -> response().
delete_alias(PoolName, Index, Alias, Timeout) when is_binary(Index) andalso is_binary(Alias) andalso is_integer(Timeout) ->
    pool_call(PoolName, {delete_alias, Index, Alias}, Timeout).

-spec is_alias(pool_name(), index(), index()) -> response().
is_alias(PoolName, Index, Alias) when is_binary(Index) andalso is_binary(Alias) ->
    is_alias(PoolName, Index, Alias, ?DEFAULT_TIMEOUT).

-spec is_alias(pool_name(), index(), index(), integer()) -> response().
is_alias(PoolName, Index, Alias, Timeout) when is_binary(Index) andalso is_binary(Alias) andalso is_integer(Timeout) ->
    pool_call(PoolName, {is_alias, Index, Alias}, Timeout).

-spec get_alias(pool_name(), index(), index()) -> response().
get_alias(PoolName, Index, Alias) when is_binary(Index) andalso is_binary(Alias) ->
    get_alias(PoolName, Index, Alias, ?DEFAULT_TIMEOUT).

-spec get_alias(pool_name(), index(), index(), integer()) -> response().
get_alias(PoolName, Index, Alias, Timeout) when is_binary(Index) andalso is_binary(Alias) andalso is_integer(Timeout) ->
    pool_call(PoolName, {get_alias, Index, Alias}, Timeout).

-spec pool_call(pool_name(), tuple(), timeout()) ->
    response().
pool_call(PoolName, Command, Timeout) ->
    Call = fun(Worker) -> gen_server:call(Worker, Command, Timeout) end,
    Result = poolboy:transaction(PoolName, Call),
    quintana:notify_spiral({?POOL_IN_USE_METRIC, 1}),
    case Result of
        {ok, Response} ->
            Response;
        {error, _} = E->
            lager:error("Erlasticsearch error: ~p", [{E, Command}]),
            E
    end.

-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.

-spec set_env(Key :: atom(), Value :: term()) -> ok.
set_env(Key, Value) ->
    application:set_env(?APP, Key, Value).

-spec join([binary()], Sep::binary()) -> binary().
join(List, Sep) when is_list(List) ->
    list_to_binary(join_list_sep(List, Sep)).

-spec join_list_sep([binary()], binary()) -> [any()].
join_list_sep([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join_list_sep([], _Sep) ->
    [].
join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    lists:reverse(Acc).
