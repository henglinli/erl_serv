%% user data base
-record(user, {name::binary(),
	       password::binary()
	      }).
%% user session
-record(session, {pid::pid(),
		  user::[byte() | bitstring()],
		  token::non_neg_integer()
		 }).
%% riak_core nrw
-define(N, 3).
-define(R, 1).
-define(W, 3).
%% riak_core_vnode_master
-define(SERV_VMASTER, serv_vnode_master).
-define(STAT_VMASTER, serv_vnode_stat_master).
-define(ENTRY_VMASTER, serv_vnode_entry_master).
%% riakk_core service
-define(STAT_SERVICE, serv_service_stat).
-define(ENTRY_SERVICE, serv_service_entry).
