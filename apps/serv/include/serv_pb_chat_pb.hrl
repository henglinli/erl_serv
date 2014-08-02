-ifndef(CHAT_PB_H).
-define(CHAT_PB_H, true).
-record(chat, {
    from,
    to = erlang:error({required, to}),
    time,
    msg = erlang:error({required, msg})
}).
-endif.

-ifndef(CHAT_ID_PB_H).
-define(CHAT_ID_PB_H, true).
-record(chat_id, {
    id = erlang:error({required, id})
}).
-endif.

-ifndef(REPLY_PB_H).
-define(REPLY_PB_H, true).
-record(reply, {
    id = erlang:error({required, id}),
    errcode = erlang:error({required, errcode}),
    errmsg
}).
-endif.

