-ifndef(ERROR_RESPONSE_PB_H).
-define(ERROR_RESPONSE_PB_H, true).
-record(error_response, {
    errmsg = erlang:error({required, errmsg}),
    errcode = erlang:error({required, errcode})
}).
-endif.

-ifndef(INFO_RESPONSE_PB_H).
-define(INFO_RESPONSE_PB_H, true).
-record(info_response, {
    node,
    server_version
}).
-endif.

-ifndef(PB_PAIR_PB_H).
-define(PB_PAIR_PB_H, true).
-record(pb_pair, {
    key = erlang:error({required, key}),
    value
}).
-endif.

-ifndef(AUTH_REQUEST_PB_H).
-define(AUTH_REQUEST_PB_H, true).
-record(auth_request, {
    user = erlang:error({required, user}),
    password = erlang:error({required, password})
}).
-endif.

-ifndef(CHAT_PB_H).
-define(CHAT_PB_H, true).
-record(chat, {
    from = erlang:error({required, from}),
    to = erlang:error({required, to}),
    time = erlang:error({required, time}),
    msg = erlang:error({required, msg})
}).
-endif.

