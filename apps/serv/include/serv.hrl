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

%% bucket
%% message
-define(MESSAGE, <<"Message">>).
%% user
-define(USER, <<"User">>).
%% ping
-define(PING, <<"Ping">>).
%% timeout
-define(TIMEOUT, 5000).
%% client request code
%% serv_pb_base
-define(PING_CODE, 1).
-define(SELECT_CODE, 3).
-define(AUTH_CODE, 5).
%% serv_pb_chat
-define(CHAT_CODE, 11).
%% serv reponse code
%% serv_pb_base
-define(RESPONSE_CODE, 0).
-define(SERVER_CODE, 4).
%% serv_pb_chat
-define(CHAT_ID_CODE, 12).
-define(REPLY_CODE, 14).
-define(SERVER_CHAT_CODE, 16).
