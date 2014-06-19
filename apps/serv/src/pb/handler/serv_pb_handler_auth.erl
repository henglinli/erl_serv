%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 19 Jun 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_auth).

-include("serv_pb_chat_pb.hrl").
%% API
-export([handle/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec handle(Auth :: binary(), Session :: term()) ->
		    {noreply, nochange}.
handle(Auth, _Session) ->
    case serv_pb_chat_pb:decode(auth, Auth) of
	#auth{user = User, password = Password} ->
	    lager:info("~p: ~p", [User, Password]),
	    {[3, <<"auth">>], nochange};
	_Other -> 
	    {[2, <<"pong">>], nochange}
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
