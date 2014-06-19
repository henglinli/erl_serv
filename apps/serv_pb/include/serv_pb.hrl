%%%-------------------------------------------------------------------
%%% @author  <lee@lee>
%%% @copyright (C) 2014,
%%% @doc
%%%
%%% @end
%%% Created : 11 Jun 2014 by  <lee@lee>
%%%-------------------------------------------------------------------
%% session
-record(session, {pid::pid(),
		  user::[byte() | bitstring()],
		  token::non_neg_integer()
		 }).
%% ets session
-define(ETS_SERV_SESSION_NAME, serv_pb_session).
-define(ETS_SERV_SESSION_HEIR, {heir, self(), undefined}).
-define(ETS_SERV_SESSION_OPTS, [public,
				named_table,
				set,
				?ETS_SERV_SESSION_HEIR,
				{write_concurrency, true},
				{read_concurrency, true}
			       ]).
%% ets handler
-define(ETS_SERV_HANDLER_NAME, serv_pb_handler).
-define(ETS_SERV_HANDLER_HEIR, {heir, self(), undefined}).
-define(ETS_SERV_HANDLER_OPTS, [public,
				named_table,
				set,
				?ETS_SERV_HANDLER_HEIR,
				{read_concurrency, true}]).
