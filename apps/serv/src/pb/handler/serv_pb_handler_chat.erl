%%%-------------------------------------------------------------------
%%% @author HenryLee <lee@OSX.local>
%%% @copyright (C) 2014, HenryLee
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2014 by HenryLee <lee@OSX.local>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_chat).

-include_lib("serv_pb/include/serv_pb_base_pb.hrl").
-include("serv_pb_chat_pb.hrl").
%% API
-export([handle/2]).
-spec handle(Chat :: binary(), Session :: term()) ->
		    {noreply | term(), nochange | term()}.
handle(Chat, Session) ->
    Ok = #response{errcode = 0, errmsg = <<"OK">>},
    EncodedOk = serv_pb_base_pb:encode(Ok),
    Self = Session#session.user,
    case serv_pb_chat_pb:decode(chat, Chat) of
	#chat{from = Self, to = To} ->
	    lager:info("chat to ~p", [To]),
	    case To of
		Self ->
		    {[0, EncodedOk], nochange};
		_To ->
		    case erlang:get(To) of
			undefined ->
			    case serv_pb_session:lookup(To) of
				undefined ->
				    case riak_core_metadata:get({<<"session">>, <<"user">>}, To) of
					undefined ->
					    lager:info("save ~p's message",[To]),
					    {[0, EncodedOk], nochange};
					{pid, ToPid} ->
					    lager:info("cluster get {~p, {pid, ~p}}", [To, ToPid]),
					    _Ignore = erlang:put(To, {pid, ToPid}),
					    _Ignore = erlang:put(ToPid, {user, To}),
					    %% send message to ToPid
					    serv_pb_server:sync_send(ToPid, [6, Chat]),
					    {[0, EncodedOk], nochange}
				    end;
				{pid, ToPid} ->
				    lager:info("sesssion get {~p, {pid, ~p}}", [To, ToPid]),
				    _Ignore = erlang:put(To, {pid, ToPid}),
				    _Ignore = erlang:put(ToPid, {user, To}),
				    %% send message to ToPid
				    serv_pb_server:sync_send(ToPid, [6, Chat]),
				    {[0, EncodedOk], nochange}
			    end;
			{pid, ToPid} ->
			    lager:info("dic get {~p, {pid, ~p}}", [To, ToPid]),
			    _Ignore = erlang:put(To, {pid, ToPid}),
			    _Ignore = erlang:put(ToPid, {user, To}),
			    %% send message to ToPid
			    serv_pb_server:sync_send(ToPid, [6, Chat]),
			    {[0, EncodedOk], nochange}
		    end
	    end;
	_Other ->
	    Error = #response{errcode = 4, errmsg = <<"serv_pb_chat_pb:decode/2">>},
	    EncodedError = serv_pb_base_pb:encode(Error),
	    {[0, EncodedError], nochange}
    end.
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
