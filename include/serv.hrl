%% user data base
-record(user, {name::binary(),
	       password::binary()
	      }).
%% user session
-record(session, {pid::pid(),
		  user::[byte() | bitstring()],
		  token::non_neg_integer()
		 }).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-define(N, 3).
-define(R, 2).
-define(W, 2).
