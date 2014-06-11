%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 11 Jun 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_service_common).

%% API
-behaviour(riak_api_pb_service).

-export([init/0,
         decode/2,
         encode/1,
         process/2,
         process_stream/3]).
%%%===================================================================
%%% API
%%%===================================================================

%% @doc init/0 callback. Returns the service internal start
%% state. This service has no state.
-spec init() -> undefined.
init() ->
    undefined.

%% @doc decode/2 callback. Decodes an incoming message.
decode(Code, Bin) when Code == 1 ->
    {ok, serv_pb_codec:decode(Code, Bin)}.

%% @doc encode/1 callback. Encodes an outgoing response message.
encode(Message) ->
    {ok, serv_pb_codec:encode(Message)}.

%% @doc process/2 callback. Handles an incoming request message.
process(ping, State) ->
    {reply, pong, State}.

%% @doc process_stream/3 callback. Handles a streaming message
%% received by the server on behalf of the service. This service
%% implements no streaming responses, so all messages are ignored.
process_stream(_,_,State) ->
    {ignore, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
