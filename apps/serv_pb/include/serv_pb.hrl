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
-define(ETS_SESSION_MAP_NAME, serv_pb_session_map).
-define(ETS_SESSION_MAP_HEIR, {heir, self(), undefined}).
-define(ETS_SESSION_MAP_OPTS, [public,
			       named_table,
			       set,
			       ?ETS_SESSION_MAP_HEIR,
			       {write_concurrency, true},
			       {read_concurrency, true}
			      ]).
% ets handler
-define(ETS_SERV_HANDLER_MAP_NAME, serv_pb_handlers_map).
-define(ETS_SERV_HANDLER_MAP_HEIR, {heir, self(), undefined}).
-define(ETS_SERV_HANDLER_MAP_OPTS, [protected,
				    named_table,
				    set,
				    ?ETS_SERV_HANDLER_MAP_HEIR,
				    {read_concurrency, true}]).
