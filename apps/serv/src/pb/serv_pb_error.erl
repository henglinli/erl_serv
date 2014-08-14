%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_pb_error).

-include("serv.hrl").
-include("serv_pb_base_pb.hrl").

%% API
-export([get/1]).

-define(ERRORS, [
		 {0, <<"Ok">>},
		 {1, <<"Bad packet">>},
		 {2, <<"Not support">>},
		 {17, <<"Cannot decode">>},
		 {18, <<"Cannot process">>}
		]).

-define(UNKNOWN, <<"Unkown error code">>).
%%%===================================================================
%%% API
%%%===================================================================
-compile({inline,[get/1]}).
-spec get(non_neg_integer()) -> iolist().
get(ErrCode) ->
    Error = #response{errcode=ErrCode,
	       errmsg=proplists:get_value(ErrCode, ?ERRORS, ?UNKNOWN)},
    [?RESPONSE_CODE, Error].
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
