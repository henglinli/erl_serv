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
-include_lib("serv_pb/include/serv_pb_base_pb.hrl").
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
    Ok = #response{errcode = 0, errmsg = <<"OK">>},
    EncodedOk = serv_pb_base_pb:encode(Ok),
    case serv_pb_chat_pb:decode(chat, Chat) of
	#chat{from = Self, to = To} ->
	    lager:info("chat to ~p", [To]),
	    case To of
		Self ->
		    {reply, [0, EncodedOk], undefined};
		_To ->
		    %% send [mesasge] to server
		    ok = serv:send(erlang:self(), {forward, 1, To, Chat, ?N}),
		    EncodedChatId = encode_chat_id(1),
		    %% reply [message id] to client
		    %% after [message] was sent, reply message was sent
		    {reply, [6, EncodedChatId], #state{last_id=1}}
	    end;
	_Other ->
	    Error = #response{errcode = 4, errmsg = <<"serv_pb_chat_pb:decode/2">>},
	    EncodedError = serv_pb_base_pb:encode(Error),
	    {reply, [0, EncodedError], undefined}
    end;

handle(Chat, #state{last_id = LastId} = State) ->
    Ok = #response{errcode = 0, errmsg = <<"OK">>},
    EncodedOk = serv_pb_base_pb:encode(Ok),
    case serv_pb_chat_pb:decode(chat, Chat) of
	#chat{from = Self, to = To} ->
	    lager:info("chat to ~p", [To]),
	    case To of
		Self ->
		    {reply, [0, EncodedOk], State};
		_To ->
		    %% send [mesasge] to server
		    Id = LastId + 1,
		    ok = serv:send(erlang:self(), {forward, Id, To, Chat, ?N}),
		    EncodedChatId = encode_chat_id(Id),
		    %% reply [message id] to client
		    %% after [message] was sent, reply message was sent
		    {reply, [6, EncodedChatId], #state{last_id=Id}}
	    end;
	_Other ->
	    Error = #response{errcode = 4, errmsg = <<"serv_pb_chat_pb:decode/2">>},
	    EncodedError = serv_pb_base_pb:encode(Error),
	    {reply, [0, EncodedError], State}
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
