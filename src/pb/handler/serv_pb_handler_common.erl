%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_handler_common).
-author('HenryLee<henglinli@gmail.com>').

-include("serv_pb.hrl").
-include("serv.hrl").

-behaviour(serv_handler).

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
-spec handle_request(Request::info_request
			    | #auth_request{},
		    Session::#session{}) ->
			    {Response::binary(), NewSession::#session{}}.

handle_request(info_request, _Session) ->
    Response = serv_pb_codec:encode(
		 #info_response{node = erlang:atom_to_binary(erlang:node(), utf8),
				server_version = <<"1">>}),
    {Response, nochange};

handle_request(#auth_request{user = User,
			    password = _Password}, Session) ->
    true = ets:insert(serv_session_map:tid(), {User, self()}),
    Response = serv_pb_codec:encode(
		 #error_response{errmsg = <<"OK">>,
				 errcode = 1}),
    NewSession = Session#session{user = User},
    {Response, NewSession};

%% handle_request(#chat{from = From,
%%		     to = To,
%%		     msg = Msg
%%		 }, Session) ->
%%     case From == Session#session.user of
%%	true ->
%%	    gen_server:cast(#chat{from = From,
%%				  to = To,
%%				  time = utils:now(),
%%				  msg = Msg}),
%%	    {noreply, nochange};
%%	false ->
%%	    Response = serv_pb_codec:encode(
%%			 #error_response{errmsg = <<"not from you">>,
%%					 errcode = 3}),
%%	    {Response, nochange}
%%     end;

handle_request(_, _) ->
    Response = serv_pb_codec:encode(
		 #error_response{errmsg = <<"not implement">>,
				 errcode = 1}),
    {Response, nochange}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
