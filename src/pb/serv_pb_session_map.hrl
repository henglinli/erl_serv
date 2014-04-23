-define(ETS_SESSION_MAP_NAME, serv_pb_session_map).
-define(ETS_SESSION_MAP_HEIR, {heir, self(), undefined}).
-define(ETS_SESSION_MAP_OPTS, [public,
			       named_table,
			       set,
			       ?ETS_SESSION_MAP_HEIR,
			       {write_concurrency, true},
			       {read_concurrency, true}
			      ]).
-define(ETS_SERV_HANDLERS_NAME, serv_pb_handlers).
-define(ETS_SERV_HANDLERS_HEIR, {heir, self(), undefined}).
-define(ETS_SERV_HANDLERS_OPTS, [protected,
				 named_table,
				 set,
				 ?ETS_SERV_HANDLERS_HEIR,
				 {read_concurrency, true}]).
