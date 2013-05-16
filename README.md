ErlasticSearch
=========
A thrift based erlang client for [ElasticSearch](http://www.elasticsearch.org/).



Installation
============
Add this as a rebar dependency to your project.

1. Be sure to set up ElasticSearch to support thrift!
   * You'll need to add (at least) the following settings to config.yaml
      * ```thrift.port: 9500```
      * ```thrift.protocol: 'binary'```
    * You might want to set the port to whatever you want instead of ```9500```
1. Update your environment with the following parameters (look in [app.config](https://github.com/dieswaytoofast/erlasticsearch/blob/master/app.config) for examples)
   * ```thrift.options```
   * ```thrift.host```
   * ```thrift.port```
1. Start a client process
	* ```erlasticsearch:start_client(<<"some_unique_name_here">>).```
1. Profit


Details
============

1. The thrift client is in the form of a _simple_one_for_one_ supervised client process (a _gen_server_).  You need to explicitly start at least one client process as follows
	* ```erlasticsearch:start_client(<<"some_unique_name_here">>).```
1. Once the client process has been started, you can use either of the following to refer to it
   * The _ClientName_ (e.g. <<"_some_unique_name_here_">>)  that you used when you started the process (a ```binary()```)
   * The Pid of the client process
   * The _registered_name_ of the client_process 
      * accessible as ```erlasticsearch:registered_name(ClientName)``` (which is an ```atom()```)
1. Any JSON expected by ElasticSearch will need to go in as JSON  
   * For example --> ```<<"{\"settings\":{\"number_of_shards\":3}}">>```
1. Output returned by most everything is in the form ```{ok, #restResponse{}} | error()```
   * See the format of ```#restResponse{}``` [here](https://github.com/dieswaytoofast/erlasticsearch/blob/master/src/elasticsearch_types.hrl).
   * See the format of ```error()``` [here](https://github.com/dieswaytoofast/erlasticsearch/blob/master/src/erlasticsearch.hrl)
   * The payload _from_ ElasticSearch - when it exists - will almost always be JSON
      * e.g. --> ```<<"{\"ok\":true,\"acknowledged\":true}">>```
1. Boolean methods (e.g. ```is_index/1```) return a ```boolean()``` (d-uh)


Client Process Management
-----
These methods are available to start and stop the thrift client processes.
Note that once the process has been started, you can reference it through either _ClientName_, the Pid, or the name it is registered under (```erlasticsearch:registered_name/1```)

Function | Parameters | Description
----- | ----------- | --------
start_client/1 | ClientName  | Start a client process reference-able as _ClientName_
start_client/2 | ClientName, Parameters | Start a client process reference-able as _ClientName_, with additional thrift parameters (also settable as _thrift_options_ in ```app.config```)
stop_client/1 | ClientName  | Stop the client process references as _ClientName_
registered_name/1 | ClientName  | The atom that the thrift client process is registered under


**EXAMPLES**

```erlang
erlasticsearch@pecorino)1> erlasticsearch:start_client(<<"bar1">>).
{ok,<0.178.0>}
erlasticsearch@pecorino)2>{ok, Pid} = erlasticsearch:start_client(<<"bar2">>, [{framed, true}]).
{ok,<0.182.0>}
erlasticsearch@pecorino)3> erlasticsearch:registered_name(<<"bar2">>).
'erlasticsearch_bar2.client'
erlasticsearch@pecorino)4> erlasticsearch:flush(<<"bar2">>).
{ok,{restResponse,200,undefined,<<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}}
erlasticsearch@pecorino)5> erlasticsearch:flush('erlasticsearch_bar2.client').
{ok,{restResponse,200,undefined,<<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}}
erlasticsearch@pecorino)6> erlasticsearch:flush(Pid).
{ok,{restResponse,200,undefined,<<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}}
erlasticsearch@pecorino)7> erlasticsearch:stop_client(<<"bar1">>).
ok
erlasticsearch@pecorino)8> erlasticsearch:stop_client(<<"bar2">>).
ok
```

Index CRUD
-----
These methods are available to perform CRUD activities on Indexes (kinda, sorta, vaguely the equivalent of Databases)

Function | Parameters | Description
----- | ----------- | --------
create_index/2 | ServerRef, IndexName  | Creates the Index called _IndexName_
create_index/3 | ServerRef, IndexName, Parameters | Creates the Index called _IndexName_, with additional options as specified [here](http://www.elasticsearch.org/guide/reference/api/admin-indices-create-index/)
delete_index/2 | ServerRef, IndexName  | Deletes the Index called _IndexName_
is_index/2 | ServerRef, IndexName  | Checks if the Index called _IndexName_ exists. (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```)
is_type/3 | ServerRef, IndexName, TypeName  | Checks if the Type called _TypeName exists in the index _IndexName_. (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```), as well as a list of types (e.g. ```[<<"type1">>, <<"type2">>]```)
open_index/2 | ServerRef, IndexName  | Opens the Index called _IndexName_
close_index/2 | ServerRef, IndexName  | Closes the Index called _IndexName_



**EXAMPLES**

```erlang
erlasticsearch@pecorino)1> erlasticsearch:start_client(<<"bar">>).
{ok,<0.178.0>}
erlasticsearch@pecorino)2> erlasticsearch:create_index(<<"bar">>, <<"foo2">>).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"acknowledged\":true}">>}}
erlasticsearch@pecorino)3> erlasticsearch:create_index(<<"bar">>, <<"foo3">>, <<"{\"settings\":{\"number_of_shards\":3}}">>).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"acknowledged\":true}">>}}
erlasticsearch@pecorino)4> erlasticsearch:create_index(<<"bar">>, <<"foo4">>, <<"{\"settings\":{\"number_of_shards\":3}}">>).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"acknowledged\":true}">>}}
```
```erlang
erlasticsearch@pecorino)5> erlasticsearch:delete_index(<<"bar">>, <<"foo2">>).                                               
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"acknowledged\":true}">>}}
```
```erlang
erlasticsearch@pecorino)6> erlasticsearch:is_index(<<"bar">>, <<"foo3">>).    
false
erlasticsearch@pecorino)7> erlasticsearch:is_index(<<"bar">>, <<"foo4">>).
true
erlasticsearch@pecorino)8> erlasticsearch:is_index(<<"bar">>, [<<"foo3">>, <<"foo4">>]).
true
erlasticsearch@pecorino)9> erlasticsearch:is_index(<<"bar">>, <<"no_such_index">>).
false
erlasticsearch@pecorino)10> erlasticsearch:is_type(<<"bar">>, <<"foo3">>, <<"existing_type_1">>).
true
erlasticsearch@pecorino)11> erlasticsearch:is_type(<<"bar">>, [<<"foo3">>, <<"foo4">>], <<"existing_type_1">>).
true
erlasticsearch@pecorino)11> erlasticsearch:is_type(<<"bar">>, [<<"foo3">>, <<"foo4">>], [<<"existing_type_1">>, <<"existing_type_2">>]).
true
```
```erlang
erlasticsearch@pecorino)12> erlasticsearch:open_index(<<"bar">>, <<"index1">>).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"acknowledged\":true}">>}}
erlasticsearch@pecorino)13> erlasticsearch:close_index(<<"bar">>, <<"index1">>).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"acknowledged\":true}">>}}  
```



Document CRUD
-----
These methods are available to perform CRUD activities on actual documents 

Function | Parameters | Description
----- | ----------- | --------
insert_doc/5 | IndexName, Type, Id, Doc  | Creates the Doc under _IndexName_, with type _Type_, and id _Id_
insert_doc/6 | IndexName, Type, Id, Doc, Params  | Creates the Doc under _IndexName_, with type _Type_, and id _Id_, and passes the tuple-list _Params_ to ElasticSearch
get_doc/4 | IndexName, Type, Id  | Gets the Doc under _IndexName_, with type _Type_, and id _Id_
get_doc/5 | IndexName, Type, Id, Params  | Gets the Doc under _IndexName_, with type _Type_, and id _Id_, and passes the tuple-list _Params_ to ElasticSearch
delete_doc/4 | IndexName, Type, Id  | Deleset the Doc under _IndexName_, with type _Type_, and id _Id_
delete_doc/5 | IndexName, Type, Id, Params  | Deletes the Doc under _IndexName_, with type _Type_, and id _Id_, and passes the tuple-list _Params_ to ElasticSearch

_Note_: For both ```insert_doc/4``` and ```insert_doc/5```, sending in ```undefined``` as the ```Id``` will result in ElasticSearch generating an Id for the document.  This Id will be returned as part of the result...


**EXAMPLES**

```erlang
erlasticsearch@pecorino)5> erlasticsearch:start_client(<<"bar">>).
{ok,<0.178.0>}
erlasticsearch@pecorino)6> erlasticsearch:insert_doc(<<"bar">>, <<"index1">>, <<"type1">>, <<"id1">>, <<"{\"some_key\":\"some_val\"}">>).
{ok,{restResponse,201,undefined,
                  <<"{\"ok\":true,\"_index\":\"index1\",\"_type\":\"type1\",\"_id\":\"id1\",\"_version\":1}">>}}
erlasticsearch@pecorino)7> erlasticsearch:insert_doc(<<"bar">>, <<"index2">>, <<"type3">>, <<"id2">>, <<"{\"some_key\":\"some_val\"}">>, [{'_ttl', '1d'}]). 
{ok,{restResponse,201,undefined,
                  <<"{\"ok\":true,\"_index\":\"index2\",\"_type\":\"type3\",\"_id\":\"id2\",\"_version\":1}">>}}
erlasticsearch@pecorino)8> erlasticsearch:insert_doc(<<"bar">>, <<"index3">>, <<"type3">>, undefined, <<"{\"some_key\":\"some_val\"}">>).
{ok,{restResponse,201,undefined,
                  <<"{\"ok\":true,\"_index\":\"index3\",\"_type\":\"type3\",\"_id\":\"8Ji9R-TtT4KXxUOvb14K8g\",\"_version\":1}">>}}
```
```erlang
erlasticsearch@pecorino)9> erlasticsearch:get_doc(<<"bar">>, <<"index1">>, <<"type1">>, <<"id1">>).
{ok,{restResponse,200,undefined,
                  <<"{\"_index\":\"index1\",\"_type\":\"type1\",\"_id\":\"id1\",\"_version\":1,\"exists\":true, \"_source\" : {\"som"...>>}}
erlasticsearch@pecorino)10> erlasticsearch:get_doc(<<"bar">>, <<"index1">>, <<"type1">>, <<"id1">>, [{fields, foobar}]).
{ok,{restResponse,200,undefined,
                  <<"{\"_index\":\"index1\",\"_type\":\"type1\",\"_id\":\"id1\",\"_version\":1,\"exists\":true}">>}}
erlasticsearch@pecorino)11> erlasticsearch:get_doc(<<"bar">>, <<"index1">>, <<"type1">>, <<"id1">>, [{fields, some_key}]).
{ok,{restResponse,200,undefined,
                  <<"{\"_index\":\"index1\",\"_type\":\"type1\",\"_id\":\"id1\",\"_version\":1,\"exists\":true,\"fields\":{\"some_ke"...>>}}
```
```erlang
erlasticsearch@pecorino)12> erlasticsearch:delete_doc(<<"bar">>, <<"index1">>, <<"type1">>, <<"id1">>).                   
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"found\":true,\"_index\":\"index1\",\"_type\":\"type1\",\"_id\":\"id1\",\"_version\":2}">>}}
```

Search
-----
API to perform searches against ElasticSearch (this _is_ why you are using ElasticSearch, right?)

Function | Parameters | Description
----- | ----------- | --------
search/4 | ServerRef, IndexName, Type, Doc  | Searches the index _IndexName_, with type _Type_ for the JSON query embedded in _Doc_
search/5 | ServerRef, IndexName, Type, Doc, Params  | Searches the index _IndexName_, with type _Type_ for the JSON query embedded in _Doc_, and passes the tuple-list _Params_ to ElasticSearch



**EXAMPLES**

```erlang
erlasticsearch@pecorino)1> erlasticsearch:start_client(<<"bar">>).
{ok,<0.178.0>}
erlasticsearch@pecorino)2> erlasticsearch:insert_doc(<<"bar">>, <<"index1">>, <<"type1">>, <<"id1">>, <<"{\"some_key\":\"some_val\"}">>).
{ok,{restResponse,201,undefined,
                  <<"{\"ok\":true,\"_index\":\"index1\",\"_type\":\"type1\",\"_id\":\"id1\",\"_version\":1}">>}}
erlasticsearch@pecorino)3> erlasticsearch:search(<<"bar">>, <<"index1">>, <<"type1">>, <<>>, [{q, "some_key:some_val"}]).     
{ok,{restResponse,200,undefined,
                  <<"{\"took\":1,\"timed_out\":false,\"_shards\":{\"total\":5,\"successful\":5,\"failed\":0},\"hits\":{\"total\":"...>>}}
```

Index Helpers
-----
A bunch of functions that do "things" to indices (flush, refresh, etc.)

Function | Parameters | Description
----- | ----------- | --------
flush/1 | ServerRef  | Flushes all the indices
flush/2 | ServerRef, Index | Flushes the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]```)
optimize/1 | ServerRef  | Optimizes all the indices
optimize/2 | ServerRef, Index | Optimizes the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
segments/1 | ServerRef  | Provides segment information for all the indices in the cluster
segments/2 | ServerRef, Index | Provides segment information for the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
refresh/1 | ServerRef  | Refreshes all the indices
refresh/2 | ServerRef, Index | Refreshes the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
status/2 | ServerRef, Index | Returns the status of index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
clear_cache/1 | ServerRef  | Clears all the caches in the cluster
clear_cache/2 | ServerRef, Index | Clears all the caches associated with the index _IndexName_.  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)
clear_cache/3 | ServerRef, Index, params | Clears all the caches associated with the index _IndexName_, using _Params_  (Note that a list of Indices can also be sent in (e.g., ```[<<"foo">>, <<"bar">>]``` This list can also be empty - ```[]```)


**EXAMPLES**

```erlang
erlasticsearch@pecorino)1> erlasticsearch:start_client(<<"bar">>).
{ok,<0.178.0>}
erlasticsearch@pecorino)2> erlasticsearch:refresh(<<"bar">>).                                                                                   {ok,{restResponse,200,undefined,                                                                                                                                               <<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}} 
erlasticsearch@pecorino)3> erlasticsearch:refresh(<<"bar">>, <<"index1">>).                                                                        {ok,{restResponse,200,undefined,                                                                                                                                               <<"{\"ok\":true,\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0}}">>}}
erlasticsearch@pecorino)4> erlasticsearch:refresh(<<"bar">>, [<<"index1">>, <<"index2">>]).
{ok,{restResponse,200,undefined,                                                                                                                                               <<"{\"ok\":true,\"_shards\":{\"total\":16,\"successful\":8,\"failed\":0}}">>}} 
```
```erlang
erlasticsearch@pecorino)5> erlasticsearch:flush(<<"bar">>).                                                                                   {ok,{restResponse,200,undefined,                                                                                                                                               <<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}} 
erlasticsearch@pecorino)6> erlasticsearch:refresh(<<"bar">>, <<"index1">>).                                                                        {ok,{restResponse,200,undefined,                                                                                                                                               <<"{\"ok\":true,\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0}}">>}}
erlasticsearch@pecorino)7> erlasticsearch:refresh(<<"bar">>, [<<"index1">>, <<"index2">>]).
{ok,{restResponse,200,undefined,                                                                                                                                               <<"{\"ok\":true,\"_shards\":{\"total\":16,\"successful\":8,\"failed\":0}}">>}} 
```
```erlang
erlasticsearch@pecorino)8> erlasticsearch:status(<<"bar">>, <<"index1">>).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":6,\"successful\":3,\"failed\":0},\"indices\":{\"index1\":{\"index\":{\"prim"...>>}}
erlasticsearch@pecorino)9> erlasticsearch:status(<<"bar">>, [<<"index1">>, <<"index2">>]).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":16,\"successful\":8,\"failed\":0},\"indices\":{\"index2\":{\"index\":{\"pri"...>>}}
erlasticsearch@pecorino)10> erlasticsearch:optimize(<<"bar">>, <<"index1">>).     
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0}}">>}}
erlasticsearch@pecorino)11> erlasticsearch:optimize(<<"bar">>, [<<"index1">>, <<"index2">>]).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":16,\"successful\":8,\"failed\":0}}">>}}
erlasticsearch@pecorino)12> erlasticsearch:optimize(<<"bar">>).                         
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":692,\"successful\":346,\"failed\":0}}">>}}                  
```
```erlang
erlasticsearch@pecorino)13> erlasticsearch:segments(<<"bar1">>).                                     
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":170,\"successful\":85,\"failed\":0},\"indices\":{\"test_1\":{\"shards\":"...>>}}
erlasticsearch@pecorino)14> erlasticsearch:segments(<<"bar1">>, [<<"index1">>]).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0},\"indices\":{\"index1\":{\"shards\":{\""...>>}}
erlasticsearch@pecorino)15> erlasticsearch:segments(<<"bar1">>, [<<"index1">>, <<"index2">>]).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":20,\"successful\":10,\"failed\":0},\"indices\":{\"index1\":{\"shards\":{"...>>}}
```
```erlang
erlasticsearch@pecorino)16> erlasticsearch:clear_cache(<<"bar1">>).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":130,\"successful\":65,\"failed\":0}}">>}}
erlasticsearch@pecorino)17> erlasticsearch:clear_cache(<<"bar1">>, [<<"index1">>]).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0}}">>}}
erlasticsearch@pecorino)18> erlasticsearch:clear_cache(<<"bar1">>, [<<"index1">>], [{filter, true}]).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0}}">>}}
erlasticsearch@pecorino)19> erlasticsearch:clear_cache(<<"bar1">>, [], [{filter, true}]).            
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"_shards\":{\"total\":140,\"successful\":70,\"failed\":0}}">>}}         
```


Cluster Helpers
-----
A bunch of functions that do "things" to clusters (health, etc.)

Function | Parameters | Description
----- | ----------- | --------
health/1 | ServerRef  | Reports the health of the cluster
state/1 | ServerRef  | Reports the state of the cluster
state/2 | ServerRef, Params  | Reports the state of the cluster, with optional parameters
nodes_info/1 | ServerRef  | Reports the state of all the nodes in the cluster
nodes_info/2 | ServerRef, NodeName  | Reports the state of the node _NodeName_ in the cluster. (Note that a list of Nodes can also be sent in (e.g., ```[<<"node1">>, <<"node2">>]``` This list can also be empty - ```[]```)
nodes_info/3 | ServerRef, NodeName, Params  | Reports the state of the node _NodeName_ in the cluster, with optional _Params_. (Note that a list of Nodes can also be sent in (e.g., ```[<<"node1">>, <<"node2">>]``` This list can also be empty - ```[]```)
nodes_stats/1 | ServerRef  | Reports stats on all the nodes in the cluster
nodes_stats/2 | ServerRef, NodeName  | Reports the stats of the node _NodeName_ in the cluster. (Note that a list of Nodes can also be sent in (e.g., ```[<<"node1">>, <<"node2">>]```)
nodes_stats/3 | ServerRef, NodeName, Params  | Reports the stats of the node _NodeName_ in the cluster, with optional _Params_. (Note that a list of Nodes can also be sent in (e.g., ```[<<"node1">>, <<"node2">>]``` This list can also be empty - ```[]```)


**EXAMPLES**
```erlang
erlasticsearch@pecorino)1> erlasticsearch:start_client(<<"bar">>).
{ok,<0.178.0>}
erlasticsearch@pecorino)2> erlasticsearch:refresh(<<"bar">>).                                                                                   {ok,{restResponse,200,undefined,                                                                                                                                               <<"{\"ok\":true,\"_shards\":{\"total\":552,\"successful\":276,\"failed\":0}}">>}}
erlasticsearch@pecorino)3> erlasticsearch:health(<<"bar">>).                          
{ok,{restResponse,200,undefined,
                  <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"status\":\"yellow\",\"timed_out\":false,\"number_of_nodes\""...>>}}
```
```erlang
erlasticsearch@pecorino)4> erlasticsearch:state(<<"bar">>).
{ok,{restResponse,200,undefined,
                  <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"master_node\":\"7k3ViuT5SQ67ayWsF1y8hQ\",\"blocks\":{\"ind"...>>}}
erlasticsearch@pecorino)5> erlasticsearch:state(<<"bar">>, [{filter_nodes, true}]).
{ok,{restResponse,200,undefined,
                  <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"blocks\":{\"indices\":{\"index1\":{\"4\":{\"description\":\"inde"...>>}}
```
```erlang
erlasticsearch@pecorino)6> erlasticsearch:nodes_info(<<"bar">>).                   
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"cluster_name\":\"elasticsearch_mahesh\",\"nodes\":{\"node1\":{\"name\":\""...>>}}
erlasticsearch@pecorino)7> erlasticsearch:nodes_info(<<"bar">>, <<"node1">>).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"cluster_name\":\"elasticsearch_mahesh\",\"nodes\":{\"node1\":{\"name\":\""...>>}}
erlasticsearch@pecorino)8> erlasticsearch:nodes_info(<<"bar">>, [<<"node1">>]).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"cluster_name\":\"elasticsearch_mahesh\",\"nodes\":{\"node1\":{\"name\":\""...>>}}
erlasticsearch@pecorino)9> erlasticsearch:nodes_info(<<"bar">>, [<<"node1">>], [{os, true}, {process, true}]).
{ok,{restResponse,200,undefined,
                  <<"{\"ok\":true,\"cluster_name\":\"elasticsearch_mahesh\",\"nodes\":{\"node1\":{\"name\":\""...>>}}
```
```erlang
erlasticsearch@pecorino)10> erlasticsearch:nodes_stats(<<"bar">>).                              
{ok,{restResponse,200,undefined,
                  <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"nodes\":{\"node1\":{\"timestamp\":136865"...>>}}
erlasticsearch@pecorino)11> erlasticsearch:nodes_stats(<<"bar">>, <<"node1">>).
{ok,{restResponse,200,undefined,
                  <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"nodes\":{\"node1\":{\"timestamp\":136865"...>>}}
erlasticsearch@pecorino)12> erlasticsearch:nodes_stats(<<"bar">>, [<<"node1">>]).
{ok,{restResponse,200,undefined,
                  <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"nodes\":{\"node1\":{\"timestamp\":136865"...>>}}
erlasticsearch@pecorino)13> erlasticsearch:nodes_stats(<<"bar">>, [<<"node1">>], [{process, true}, {transport, true}]).
{ok,{restResponse,200,undefined,
                  <<"{\"cluster_name\":\"elasticsearch_mahesh\",\"nodes\":{\"node1\":{\"timestamp\":136865"...>>}}
```



Credits
=======
This is _not_ to be confused with [erlastic_search](https://github.com/tsloughter/erlastic_search) by [Tristan Sloughter](https://github.com/tsloughter), which is HTTP/REST based, and quite probably more feature rich, but almost certainly did not involve quite this level of head-thumping associated w/ figuring out how Thrift works…

(Yes, this is a _Credit_)
