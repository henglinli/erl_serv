%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
-module(serv_handler).

-author('HenryLee<henglinli@gmail.com>').

-include("serv.hrl").

%% API

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

-callback handle_request(Request :: any(), Session :: #session{}) ->
    {Response :: binary() | noreply, NewSession :: #session{} | nochange}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
