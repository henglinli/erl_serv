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
    User = Session#session.user,
    case serv_pb_chat_pb:decode(chat, Chat) of
	ProtobufChat = #chat{from = User, to = To} ->
	    lager:info("chat to ~p", [To]),
	    case erlang:get(To) of
		undefined ->
		    case riak_core_metadata:get({<<"session">>, <<"user">>}, To) of
			undefined ->
			    lager:info("save ~p's message",[To]),
			    {[0, EncodedOk], nochange};
			{pid, ToPid} ->
			    serv_pb_server:send(ToPid, ProtobufChat),
			    {[0, EncodedOk], nochange}
		    end;
		{pid, ToPid} ->
		    serv_pb_server:send(ToPid, ProtobufChat),
		    {[0, EncodedOk], nochange}
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
