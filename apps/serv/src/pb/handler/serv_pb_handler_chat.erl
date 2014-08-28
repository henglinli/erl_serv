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
%% handler API
-export([decode/1, process/2, encode/1]).

%%%===================================================================
%%% API
%%%===================================================================
-record(state, {last_id :: integer}).

%% @doc decode/2 -> process/3 [-> procss_stream/3] -> encode/2
%% @doc 1, decode binary to record
-spec decode(Message :: binary()) ->
		    {ok, DecodedMessage :: term()} |
		    {error, Reason :: iodata()}.
decode(Message) ->
    case serv_pb_chat_pb:decode(chat, Message) of
	#chat{} = Record ->
	    {ok, Record};
	_Other ->
	    {error, serv_pb_error:get(17)}
    end.
%% @doc 2, process record and return record
-spec process(Message :: term(), State :: term()) ->
		     {reply, ReplyMessage :: term(), NewState :: term()} |
		     {async, Module :: module(), NewState :: term()} |
		     {error, Reason :: iodata(), NewState :: term()}.
process(#chat{from=_Self, to=To}=Record, undefined) ->
    %% send [mesasge] to server
    Message = #message{id=1, from=erlang:self(), to=To, msg=Record},
    ok = serv:send({forward, Message, ?N}),
    %% reply [message id] to client
    %% after [message] was sent, reply message was sent
    {reply, #chat_id{id=1}, #state{last_id=1}};

process(#chat{from=_Self, to=To}=Record, #state{last_id=LastId}) ->
    %% send [mesasge] to server
    Id = LastId + 1,
    Message = #message{id=Id, from=erlang:self(), to=To, msg=Record},
    ok = serv:send({forward, Message, ?N}),
    %% reply [message id] to client
    %% after [message] was sent, reply message was sent
    {reply, #chat_id{id=Id}, #state{last_id=Id}};

process(_Message, State) ->
    {error, serv_pb_error:get(18), State}.

%% @doc 3, encode record to iodata
-spec encode(Message :: term()) ->
		    {ok, EncodedMessage :: iodata()} |
		    {error, Reason :: iodata()}.
encode(#reply{}=Reply) ->
    Encoded = serv_pb_chat_pb:encode(Reply),
    [?REPLY_CODE, Encoded];

encode(#chat_id{}=ChatId) ->
    EncodedRecordId = serv_pb_chat_pb:encode(ChatId),
    [?CHAT_ID_CODE, EncodedRecordId];

encode(_ChatId) ->
    serv_pb_error:get(19).

%%%===================================================================
%%% Internal functions
%%%===================================================================
