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
	%% ToDo: check from is self
	%% ToDo: check To is valid(is Who can send mesasge to)
        #chat{from = _Self, to = To} ->
	    %% send [mesasge] to server
	    Message = #message{id=1, from=erlang:self(), to=To, msg=Chat},
	    ok = serv:send({forward, Message, ?N}),
	    EncodedChatId = encode_chat_id(1),
	    %% reply [message id] to client
	    %% after [message] was sent, reply message was sent
	    {reply, [?CHAT_ID_CODE, EncodedChatId], #state{last_id=1}};
        _Other ->
            Error = #response{errcode = 4, errmsg = <<"serv_pb_chat_pb:decode/2">>},
            EncodedError = serv_pb_base_pb:encode(Error),
            {reply, [0, EncodedError], undefined}
    end;

handle(Chat, #state{last_id = LastId} = State) ->
    case serv_pb_chat_pb:decode(chat, Chat) of
        #chat{from = _Self, to = To} ->
	    %% send [mesasge] to server
	    Id = LastId + 1,
	    Message = #message{id=1, from=erlang:self(), to=To, msg=Chat},
	    ok = serv:send({forward, Message, ?N}),
	    EncodedChatId = encode_chat_id(Id),
	    %% reply [message id] to client
	    %% after [message] was sent, reply message was sent
	    {reply, [?CHAT_ID_CODE, EncodedChatId], #state{last_id=Id}};
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
