%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_db).
-include("serv.hrl").
%% API
-export([init/0,
	 init/1
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-define(CREATE_TABLE(Nodes, Record),
	mnesia:create_table(Record,
			    [{disc_copies, Nodes},
			     {attributes, record_info(fields, Record)}
			     ])).

-spec init() -> ok | error.
init() ->
    init([erlang:node()]).

-spec init(Nodes::[node()]) -> ok | error.
init(Nodes) ->
    _Ignore = mnesia:delete_schema(Nodes),
    case mnesia:create_schema(Nodes) of
	ok ->
	    case mnesia:start() of
		ok ->
		    case ?CREATE_TABLE(Nodes, user) of
			{atomic, ok} ->
			    ok;
			_ ->
			    error
		    end;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
