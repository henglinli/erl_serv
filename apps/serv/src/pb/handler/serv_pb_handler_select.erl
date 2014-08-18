%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 13 Aug 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_select).
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
		    {error, Reason :: iodata()}.
decode(Message) ->
    case serv_pb_base_pb:decode(chat, Message) of
	#select{} = Record ->
	    {ok, Record};
	_Other ->
	    {error, serv_pb_error:get(17)}
    end.
%% @doc 2, process record and return record
-spec process(Message :: term(), State :: term()) ->
		     {reply, ReplyMessage :: term(), NewState :: term()} |
		     {stream, ReqId :: term(), NewState :: term()} |
		     {error, Reason :: iodata(), NewState :: term()}.
process(#select{user=User}, State) ->
    ok=serv:send({select, erlang:self(), User, 1}),
    Response = #response{errcode=0, errmsg = <<"Ok">>},
    {reply, Response, State};

process(_Message, State) ->
    {error, serv_pb_error:get(18), State}.

%% @doc 3, if return stream procss it
-spec process_stream(Message :: term(), ReqId :: term(), State :: term()) ->
			    {reply, Reply :: [term()] | term(), NewState :: term()} |
			    {ignore, NewState :: term()} |
			    {done, Reply :: iodata(), NewState :: term()} |
			    {done, NewState :: term()} |
			    {error, Reason :: iodata(), NewState :: term()}.
process_stream(_Message, _ReqId, State) ->
    {ignore, State}.

%% @doc 4, encode record to iodata
-spec encode(Message :: term()) ->
		    {ok, EncodedMessage :: iodata()} |
		    {error, Reason :: iodata()}.
encode(#response{}=Response) ->
    Encoded = serv_pb_base_pb:encode(Response),
    [?RESPONSE_CODE, Encoded];

encode(_Response) ->
   serv_pb_error:get(19).
%%%===================================================================
%%% Internal functions
%%%===================================================================