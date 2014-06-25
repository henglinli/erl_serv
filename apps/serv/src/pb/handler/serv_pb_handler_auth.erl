%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 19 Jun 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_auth).

-include_lib("serv_pb/include/serv_pb_base_pb.hrl").
-include("serv_pb_chat_pb.hrl").
%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec handle(Auth :: binary(), Session :: term()) ->
		    {noreply | term(), nochange | term()}.
handle(Auth, _Session) ->
    case serv_pb_chat_pb:decode(auth, Auth) of
	#auth{user = User, password = Password} ->
	    lager:info("~p:~p", [User, Password]),
	    case erlang:get(User) of
		undefined ->
		    riak_core_metadata:put({<<"session">>, <<"user">>}, User, {pid, erlang:self()});
		{pid, _Pid} ->
		    undefined
	    end,
	    Ok = #response{errcode = 0, errmsg = <<"OK">>},
	    EncodedOk = serv_pb_base_pb:encode(Ok),
	    {[0, EncodedOk], #session{user = User}};
	_Other ->
	    Error = #response{errcode = 4, errmsg = <<"serv_pb_chat_pb:decode/2">>},
	    EncodedError = serv_pb_base_pb:encode(Error),
	    {[0, EncodedError], nochange}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
