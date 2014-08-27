-module(serv_kv).
%%
-export([get/2]).
-export([put/3]).
-export([update/4]).
%%
-export([delete/3,delete/4,delete/5]).
-export([delete_vclock/4,delete_vclock/5,delete_vclock/6]).
-export([set_bucket/3,get_bucket/2,reset_bucket/2]).
-export([reload_all/2]).
-export([remove_from_cluster/2]).
-export([get_client_id/1]).
-export([for_dialyzer_only_ignore/3]).
-export([ensemble/1]).

-compile({no_auto_import,[put/2]}).
%% @type default_timeout() = 60000
-define(DEFAULT_TIMEOUT, 60000).
-define(DEFAULT_ERRTOL, 0.00003).

-spec get(Bucket :: binary(), Key :: binary()) ->
		 Result :: term() | {error, Reason :: term()}.
get(Bucket, Key) ->
    Bkey = {Bucket, Key},
    Ensemble = ensemble(Bkey),
    Timeout = recv_timeout([]),
    riak_ensemble_client:kget(Ensemble, Bkey, Timeout).

-spec update(Bucket :: binary(), Key :: binary(),
	     OldValue :: term(), NewValue :: term()) ->
		    Result :: term() | {error, Reason :: term()}.
update(Bucket, Key, OldValue, NewValue) ->
    BKey = {Bucket, Key},
    Ensemble = ensemble(BKey),
    Timeout = recv_timeout([]),
    riak_ensemble_client:kupdate(Ensemble, BKey, OldValue, NewValue, Timeout).

-spec put(Bucket :: binary(), Key :: binary(), Value :: term()) ->
		 Result :: term() | {error, Reason :: term()}.
put(Bucket, Key, Value) ->
    BKey = {Bucket, Key},
    Ensemble = ensemble(BKey),
    Timeout = recv_timeout([]),
    riak_ensemble_client:kput_once(Ensemble, BKey, Value, Timeout).

%% TODO: This type needs to be better specified and validated against
%%       any dependents on riak_kv.
%%
%%       We want this term to be opaque, but can't because Dialyzer
%%       doesn't like the way it's specified.
%%
%%       opaque type riak_client() is underspecified and therefore meaningless
-type riak_client() :: term().

%% @spec delete(riak_object:bucket(), riak_object:key(), riak_client()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as RW
%%      nodes have responded with a value or error.
%% @equiv delete(Bucket, Key, RW, default_timeout())
delete(Bucket,Key,{?MODULE, [_Node, _ClientId]}=THIS) ->
    delete(Bucket,Key,[],?DEFAULT_TIMEOUT,THIS).

%% @spec delete(riak_object:bucket(), riak_object:key(), RW :: integer(), riak_client()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as W/DW (or RW)
%%      nodes have responded with a value or error.
%% @equiv delete(Bucket, Key, RW, default_timeout())
delete(Bucket,Key,Options,{?MODULE, [_Node, _ClientId]}=THIS) when is_list(Options) ->
    delete(Bucket,Key,Options,?DEFAULT_TIMEOUT,THIS);

delete(Bucket,Key,RW,{?MODULE, [_Node, _ClientId]}=THIS) ->
    delete(Bucket,Key,[{rw, RW}],?DEFAULT_TIMEOUT,THIS).

%% @spec delete(riak_object:bucket(), riak_object:key(), RW :: integer(),
%%           TimeoutMillisecs :: integer(), riak_client()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, {n_val_violation, N::integer()}} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as W/DW (or RW)
%%      nodes have responded with a value or error, or TimeoutMillisecs passes.
delete(Bucket, Key, Options, _Timeout, {?MODULE, [Node, _ClientId]}) when is_list(Options) ->
    BKey = {Bucket, Key},
    Ensemble = ensemble(BKey),
    RTimeout = recv_timeout(Options),
    case riak_ensemble_client:kdelete(Node, Ensemble, BKey, RTimeout) of
	{error, _}=Err ->
	    Err;
	{ok, Obj} when element(1, Obj) =:= r_object ->
	    ok
    end;

delete(Bucket,Key,RW,Timeout,{?MODULE, [_Node, _ClientId]}=THIS) ->
    delete(Bucket,Key,[{rw, RW}], Timeout, THIS).

%% @spec delete_vclock(riak_object:bucket(), riak_object:key(), vclock:vclock(), riak_client()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as W/DW (or RW)
%%      nodes have responded with a value or error.
%% @equiv delete(Bucket, Key, RW, default_timeout())
delete_vclock(Bucket,Key,VClock,{?MODULE, [_Node, _ClientId]}=THIS) ->
    delete_vclock(Bucket,Key,VClock,[{rw,default}],?DEFAULT_TIMEOUT,THIS).

%% @spec delete_vclock(riak_object:bucket(), riak_object:key(), vclock:vclock(),
%%                     RW :: integer(), riak_client()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as W/DW (or RW)
%%      nodes have responded with a value or error.
%% @equiv delete(Bucket, Key, RW, default_timeout())
delete_vclock(Bucket,Key,VClock,Options,{?MODULE, [_Node, _ClientId]}=THIS) when is_list(Options) ->
    delete_vclock(Bucket,Key,VClock,Options,?DEFAULT_TIMEOUT,THIS);
delete_vclock(Bucket,Key,VClock,RW,{?MODULE, [_Node, _ClientId]}=THIS) ->
    delete_vclock(Bucket,Key,VClock,[{rw, RW}],?DEFAULT_TIMEOUT,THIS).

%% @spec delete_vclock(riak_object:bucket(), riak_object:key(), vclock:vclock(), RW :: integer(),
%%           TimeoutMillisecs :: integer(), riak_client()) ->
%%        ok |
%%       {error, too_many_fails} |
%%       {error, notfound} |
%%       {error, timeout} |
%%       {error, {n_val_violation, N::integer()}} |
%%       {error, Err :: term()}
%% @doc Delete the object at Bucket/Key.  Return a value as soon as W/DW (or RW)
%%      nodes have responded with a value or error, or TimeoutMillisecs passes.
delete_vclock(Bucket, Key, VClock, Options, _Timeout, {?MODULE, [Node, _ClientId]}) when is_list(Options) ->
    BKey = {Bucket, Key},
    Ensemble = ensemble(BKey),
    Current = riak_object:set_vclock(riak_object:new(Bucket, Key, <<>>),
				     VClock),
    RTimeout = recv_timeout(Options),
    case riak_ensemble_client:ksafe_delete(Node, Ensemble, BKey, Current, RTimeout) of
	{error, _}=Err ->
	    Err;
	{ok, Obj} when element(1, Obj) =:= r_object ->
	    ok
    end;

delete_vclock(Bucket,Key,VClock,RW,Timeout,{?MODULE, [_Node, _ClientId]}=THIS) ->
    delete_vclock(Bucket,Key,VClock,[{rw, RW}],Timeout,THIS).

%% @spec set_bucket(riak_object:bucket(), [BucketProp :: {atom(),term()}], riak_client()) -> ok
%% @doc Set the given properties for Bucket.
%%      This is generally best if done at application start time,
%%      to ensure expected per-bucket behavior.
%% See riak_core_bucket for expected useful properties.
set_bucket(BucketName,BucketProps,{?MODULE, [Node, _ClientId]}) ->
    rpc:call(Node,riak_core_bucket,set_bucket,[BucketName,BucketProps]).
%% @spec get_bucket(riak_object:bucket(), riak_client()) -> [BucketProp :: {atom(),term()}]
%% @doc Get all properties for Bucket.
%% See riak_core_bucket for expected useful properties.
get_bucket(BucketName, {?MODULE, [Node, _ClientId]}) ->
    rpc:call(Node,riak_core_bucket,get_bucket,[BucketName]).
%% @spec reset_bucket(riak_object:bucket(), riak_client()) -> ok
%% @doc Reset properties for this Bucket to the default values
reset_bucket(BucketName, {?MODULE, [Node, _ClientId]}) ->
    rpc:call(Node,riak_core_bucket,reset_bucket,[BucketName]).
%% @spec reload_all(Module :: atom(), riak_client()) -> term()
%% @doc Force all Riak nodes to reload Module.
%%      This is used when loading new modules for map/reduce functionality.
reload_all(Module, {?MODULE, [Node, _ClientId]}) ->
    rpc:call(Node,riak_core_util,reload_all,[Module]).

%% @spec remove_from_cluster(ExitingNode :: atom(), riak_client()) -> term()
%% @doc Cause all partitions owned by ExitingNode to be taken over
%%      by other nodes.
remove_from_cluster(ExitingNode, {?MODULE, [Node, _ClientId]}) ->
    rpc:call(Node, riak_core_gossip, remove_from_cluster,[ExitingNode]).

%% @doc Return the client id being used for this client
get_client_id({?MODULE, [_Node, ClientId]}) ->
    ClientId.

%% @private
%% This function exists only to avoid compiler errors (unused type).
%% Unfortunately, I can't figure out how to suppress the bogus "Contract for
%% function that does not exist" warning from Dialyzer, so ignore that one.
-spec for_dialyzer_only_ignore(term(), term(), riak_client()) -> riak_client().
for_dialyzer_only_ignore(_X, _Y, {?MODULE, [_Node, _ClientId]}=THIS) ->
    THIS.

%% @private
recv_timeout(Options) ->
    case proplists:get_value(recv_timeout, Options) of
	undefined ->
	    %% If no reply timeout given, use the FSM timeout + 100ms to give it a chance
	    %% to respond.
	    proplists:get_value(timeout, Options, ?DEFAULT_TIMEOUT) + 100;
	Timeout ->
	    %% Otherwise use the directly supplied timeout.
	    Timeout
    end.

ensemble(BKey={Bucket, _Key}) ->
    {ok, CHBin} = riak_core_ring_manager:get_chash_bin(),
    DocIdx = riak_core_util:chash_key(BKey),
    Partition = chashbin:responsible_index(DocIdx, CHBin),
    N = riak_core_bucket:n_val(riak_core_bucket:get_bucket(Bucket)),
    {kv, Partition, N}.
