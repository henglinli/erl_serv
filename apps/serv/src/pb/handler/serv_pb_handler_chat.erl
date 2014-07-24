%%%-------------------------------------------------------------------
%%% @author HenryLee <lee@OSX.local>
%%% @copyright (C) 2014, HenryLee
%%% @doc
%%%
%%% @end
%%% Created : 25 Jun 2014 by HenryLee <lee@OSX.local>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_chat).

-include("serv.hrl").
-include("serv_pb_base_pb.hrl").
-include("serv_pb_chat_pb.hrl").
%% API
-export([handle/2]).

-record(state, {last_id :: integer}).
%% handle
-spec handle(Request :: term(), State :: term()) ->
		    {error, Reason :: term()} |
    {reply, Reply :: term(), NewState ::term()} |
    {noreply, NewState :: term()}.

handle(Chat, undefined) ->
    case serv_pb_chat_pb:decode(chat, Chat) of
	#chat{to = To} ->
	    lager:info("first chat to ~p", [To]),
	    %% send [mesasge] to server
	    case serv:send({pid, erlang:self()},
			   {forward, #{id => 1, to => To, msg => Chat},
			    ?N}) of
		ok ->
		    EncodedChatId = encode_chat_id(1),
		    %% reply [message id] to client
		    %% after [message] was sent, reply message was sent
		    {reply, [?CHAT_ID_CODE, EncodedChatId], 
		     #state{last_id=1}};
		_Else ->
		    {error, <<"serv:send/2 error">>}
	    end;
	_Other ->
	    {error, <<"serv_pb_chat_pb:decode/2">>}
    end;

handle(Chat, #state{last_id = LastId} = State) ->
    case serv_pb_chat_pb:decode(chat, Chat) of
	#chat{to = To} ->
	    lager:info("chat to ~p", [To]),
	    %% send [mesasge] to server
	    Id = LastId + 1,
	    case serv:send(erlang:self(),
			   {forward, #{id => Id, to => To, msg => Chat},
			    ?N}) of
		ok ->
		    EncodedChatId = encode_chat_id(Id),
		    %% reply [message id] to client
		    %% after [message] was sent, reply message was sent
		    {reply, [?CHAT_ID_CODE, EncodedChatId], 
		     State#state{last_id=Id}};
		_Else ->
		    {error, <<"serv:send/2 error">>}
	    end;
	_Other ->
	    {error, <<"serv_pb_chat_pb:decode/2">>}
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
-spec encode_chat_id(Id :: integer()) -> EncodedChatId :: binary().
encode_chat_id(Id) ->
    IdRecord = #chat_id{id = Id},
    serv_pb_chat_pb:encode(IdRecord).
