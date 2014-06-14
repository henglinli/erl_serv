%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_base).
-author('HenryLee<henglinli@gmail.com>').

-include("serv_pb.hrl").
-include("serv_pb_base_pb.hrl").

-behaviour(serv_pb_handler).

%% API
-export([handle_request/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec handle_request(Request::ping,
		     Session::#session{}) ->
			    {Response::binary(), NewSession::#session{}}.

handle_request(ping, _Session) ->
    Response = serv_pb_codec:encode(pong),
    {Response, nochange};

handle_request(_, _) ->
    Response = serv_pb_codec:encode(
		 #response{errmsg = <<"not implement">>,
				 errcode = 1}),
    {Response, nochange}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
