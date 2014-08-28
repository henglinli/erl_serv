%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 13 Aug 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_empty).
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
    {error, serv_pb_error:get(17)}.
%% @doc 2, process record and return record
-spec process(Message :: term(), State :: term()) ->
		     {reply, ReplyMessage :: term(), NewState :: term()} |
		     {async, Module :: module(), NewState :: term()} |
		     {error, Reason :: iodata(), NewState :: term()}.
process(_Message, State) ->
    {error, serv_pb_error:get(18), State}.

%% @doc 3, encode record to iodata
-spec encode(Message :: term()) ->
		    {ok, EncodedMessage :: iodata()} |
		    {error, Reason :: iodata()}.
encode(_Response) ->
    serv_pb_error:get(19).


%%%===================================================================
%%% Internal functions
%%%===================================================================
