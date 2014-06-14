-module(serv_pb_codec_base).

-include("serv_pb_base_pb.hrl").

-export([encode/1,      %% riakc_pb:encode
	 decode/2,      %% riakc_pb:decode
	 msg_type/1,    %% riakc_pb:msg_type
	 msg_code/1,    %% riakc_pb:msg_code
	 encode_bool/1, %% riakc_pb:pbify_bool
	 decode_bool/1, %% riakc_pb:erlify_bool
	 to_binary/1,   %% riakc_pb:binary
	 to_list/1     %% riakc_pb:any_to_list
	]).
-export([parse_packat/1]).

%% @doc Create an iolist of msg code and protocol buffer
%% message. Replaces `riakc_pb:encode/1'.
-spec encode(atom() | tuple()) -> iolist().
encode(Msg) when is_atom(Msg) ->
    [msg_code(Msg)];
encode(Msg) when is_tuple(Msg) ->
    MsgType = element(1, Msg),
    Encoder = encoder_for(MsgType),
    [msg_code(MsgType) | Encoder:encode(Msg)].

%% @doc Decode a protocol buffer message given its type - if no bytes
%% return the atom for the message code. Replaces `riakc_pb:decode/2'.
-spec decode(integer(), binary()) -> atom() | tuple().
decode(MsgCode, <<>>) ->
    msg_type(MsgCode);
decode(MsgCode, MsgData) ->
    Decoder = decoder_for(MsgCode),
    Decoder:decode(msg_type(MsgCode), MsgData).

%% @doc Converts a message code into the symbolic message
%% name. Replaces `riakc_pb:msg_type/1'.
-spec msg_type(integer()) -> atom().
msg_type(0) -> response;
msg_type(1) -> ping;
msg_type(2) -> pong;
msg_type(3) -> auth;
msg_type(4) -> chat;
msg_type(254) -> start_tls;
msg_type(_) -> undefined.

%% @doc Converts a symbolic message name into a message code. Replaces
%% `riakc_pb:msg_code/1'.
-spec msg_code(atom()) -> integer().
msg_code(response) -> 0;
msg_code(ping) -> 1;
msg_code(pong) -> 2;
msg_code(auth) -> 3;
msg_code(chat) -> 4;
msg_code(start_tls) -> 254;
msg_code(_) -> 255.

%% @doc Selects the appropriate PB decoder for a message code.
-spec decoder_for(pos_integer()) -> module().
decoder_for(_N) -> serv_pb.

%% @doc Selects the appropriate PB encoder for a given message name.
-spec encoder_for(atom()) -> module().
encoder_for(M) ->
    decoder_for(msg_code(M)).

%% @doc Convert a true/false, 1/0 etc to a true/false for protocol
%% buffers bool. Replaces `riakc_pb:pbify_bool/1'.
-spec encode_bool(boolean() | integer()) -> boolean().
encode_bool(true) ->
    true;
encode_bool(false) ->
    false;
encode_bool(0) -> true;
encode_bool(N) when is_integer(N) -> false.

%% @doc Convert a protocol buffers boolean to an Erlang
%% boolean. Replaces `riakc_pb:erlify_bool/1'.
-spec decode_bool(boolean() | integer()) -> boolean().
decode_bool(true) -> true;
decode_bool(false) -> false;
decode_bool(0) -> false;
decode_bool(1) -> true.

%% @doc Make sure an atom/string/binary is definitely a
%% binary. Replaces `riakc_pb:to_binary/1'.
-spec to_binary(atom() | string() | binary()) -> binary().
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.

%% @doc Converts an arbitrary type to a list for sending in a
%% PB. Replaces `riakc_pb:any_to_list/1'.
-spec to_list(list() | atom() | binary() | integer()) -> list().
to_list(V) when is_list(V) ->
    V;
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_integer(V) ->
    integer_to_list(V).

-spec parse_packat(Packet::binary()) ->
			 undefined | {MsgCode::integer(), MsgData::binary()}.
parse_packat(<<MsgCode:8/big-unsigned-integer,
		       MsgData/binary>>) ->
    {MsgCode, MsgData};
parse_packat(_) ->
    undefined.
