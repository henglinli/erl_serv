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
-export([decode/1, process/2, encode/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc decode/2 -> process/3 [-> procss_stream/3] -> encode/2
%% @doc 1, decode binary to record
-spec decode(Message :: binary()) ->
		    {ok, DecodedMessage :: term()} |
		    {error, Reason :: iodata()}.
decode(_Message) ->
    {ok, ping}.
%% @doc 2, process record and return record
-spec process(Message :: term(), State :: term()) ->
		     {reply, ReplyMessage :: term(), NewState :: term()} |
		     {async, Module :: module(), NewState :: term()} |
		     {error, Reason :: iodata(), NewState :: term()}.
process(_Message, State) ->
    {reply, pong, State}.

%% @doc 3, encode record to iodata
-spec encode(Message :: term()) ->
		    {ok, EncodedMessage :: iodata()} |
		    {error, Reason :: iodata()}.
encode(_Response) ->
    serv_pb_error:get(0).

%%%===================================================================
%%% Internal functions
%%%===================================================================
