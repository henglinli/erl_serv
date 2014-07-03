%% user data base
-record(user, {name :: binary(),
	       password :: binary()
	      }).
%% user session
-record(session, {pid :: pid(),
		  user :: binary(),
		  token :: non_neg_integer()
		 }).
%% riak_core nrw
-define(N, 3).
-define(R, 1).
-define(W, 3).
%% riak_core_vnode
-define(SERV, serv_vnode_master).
