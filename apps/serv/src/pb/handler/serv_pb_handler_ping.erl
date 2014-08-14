%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014, 
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_ping).

-include("serv.hrl").
-include("serv_pb_base_pb.hrl").
%% handler API
-export([decode/1, process/2, process_stream/3, encode/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc decode/2 -> process/3 [-> procss_stream/3] -> encode/2
%% @doc 1, decode binary to record
-spec decode(Message :: binary()) ->
		    {ok, DecodedMessage :: term()} |
		    {error, Reason :: term()}.
decode(_Message) ->
    {ok, ping}.
%% @doc 2, process record and return record
-spec process(Message :: term(), State :: term()) ->
		     {reply, ReplyMessage :: term(), NewState :: term()} |
		     {stream, ReqId :: term(), NewState :: term()} |
		     {error, Reason :: term(), NewState :: term()}.
process(_Message, State) ->
    {reply, pong, State}.

%% @doc 3, if return stream procss it
-spec process_stream(Message :: term(), ReqId :: term(), State :: term()) ->
			    {reply, Reply :: [term()] | term(), NewState :: term()} |
			    {ignore, NewState :: term()} |
			    {done, Reply :: [term()] | term(), NewState :: term()} |
			    {done, NewState :: term()} |
			    {error, Reason :: term(), NewState :: term()}.
process_stream(_Message, _ReqId, State) ->
    {ignore, State}.

%% @doc 4, encode record to iodata
-spec encode(Message :: term()) ->
		    {ok, EncodedMessage :: iodata()} |
		    {error, Reason :: term()}.
encode(_Response) ->
    Ok=serv_pb_error:get(0),
    [?RESPONSE_CODE, serv_pb_base_pb:encode(Ok)].

%%%===================================================================
%%% Internal functions
%%%===================================================================
