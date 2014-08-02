-ifndef(RESPONSE_PB_H).
-define(RESPONSE_PB_H, true).
-record(response, {
    errcode = erlang:error({required, errcode}),
    errmsg
}).
-endif.

-ifndef(PING_PB_H).
-define(PING_PB_H, true).
-record(ping, {
    
}).
-endif.

-ifndef(SELECT_PB_H).
-define(SELECT_PB_H, true).
-record(select, {
    user = erlang:error({required, user})
}).
-endif.

-ifndef(SERVER_PB_H).
-define(SERVER_PB_H, true).
-record(server, {
    errcode = erlang:error({required, errcode}),
    errmsg,
    ip
}).
-endif.

-ifndef(AUTH_PB_H).
-define(AUTH_PB_H, true).
-record(auth, {
    user = erlang:error({required, user}),
    password = erlang:error({required, password}),
    how = erlang:error({required, how})
}).
-endif.

