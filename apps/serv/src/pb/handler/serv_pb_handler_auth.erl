%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 13 Aug 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_auth).
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
decode(Message) ->
    case serv_pb_base_pb:decode(auth, Message) of
	#auth{} = Record ->
	    {ok, Record};
	_Other ->
	    {error, <<"decode">>}
    end.
%% @doc 2, process record and return record
-spec process(Message :: term(), State :: term()) ->
		     {reply, ReplyMessage :: term(), NewState :: term()} |
		     {stream, ReqId :: term(), NewState :: term()} |
		     {error, Reason :: term(), NewState :: term()}.
process(#auth{user=User, password=Password, how=How}, State) ->
    Response = #response{errmsg = <<"Ok">>,
			 errcode=0},
    case How of
	1 -> % register
	    {reply, Response, State};
	2 -> % login
	    case serv:register(#session{pid=erlang:self(),
					user=User,
					token=Password}) of
		ok ->
		    {reply, Response, State};
		_Else ->

		    {error, <<"Registration failed">>, State}
	    end
    end;

process(_Message, State) ->
    {error, <<"process">>, State}.

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
encode(#response{}=Response) ->
    [?RESPONSE_CODE, serv_pb_base_pb:encode(Response)];

encode(_Response) ->
    Response = #response{errcode=17, errmsg = <<"Bad response">>},
    [?RESPONSE_CODE, serv_pb_base_pb:encode(Response)].

%%%===================================================================
%%% Internal functions
%%%===================================================================
