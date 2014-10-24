%%%-------------------------------------------------------------------
%%% @author HenryLee <lee@OSX.local>
%%% @copyright (C) 2014, HenryLee
%%% @doc
%%%
%%% @end
%%% Created : 24 Oct 2014 by HenryLee <lee@OSX.local>
%%%-------------------------------------------------------------------
-module(serv_mc_binary_protocol).

%% API
-export([parse/1, parse/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec parse(Packet :: binary()) -> any().
parse(<<Header:192/binary,
	Rest/binary>>) ->
    case parse_header(Header) of
	{request, Command, KeyLength, ExtrasLength, WhichData, VbucketID, BodyLength, Opaque, CAS} = Header->
	    case parse_body(Rest, ExtrasLength, KeyLength, BodyLength) of
		{rest_body, RestBody} ->
		    {rest_body, RestBody, Header};
		{ok, Extras, Key, Value, Rest} ->
		    {request, WhichData, Command, Extras, Key, Value, VbucketID, Opaque, CAS, Rest}
	    end;
	{response, Command, KeyLength, ExtrasLength, WhichData, Status, BodyLength, Opaque, CAS} = Header ->
	    case parse_body(Rest, ExtrasLength, KeyLength, BodyLength) of
		{rest_body, RestBody} ->
		    {rest_body, RestBody, Header};
		{ok, Extras, Key, Value, Rest} ->
		    {response, WhichData, Command, Extras, Key, Value, Status, Opaque, CAS, Rest}
	    end;
	BadHeader ->
	    BadHeader
    end;
parse(Packet) ->
    {rest, Packet}.

parse({rest, Rest}, NewPacket) ->
    parse(<<Rest/binary, NewPacket/binary>>);

parse({rest_body, RestBody,
       {request, Command, KeyLength, ExtrasLength, WhichData, VbucketID, BodyLength, Opaque, CAS} = Header},
      NewPacket) ->
    case parse_body(RestBody, ExtrasLength, KeyLength, BodyLength, NewPacket) of
	{rest_body, NewRestBody} ->
	    {rest_body, NewRestBody, Header};
	{ok, Extras, Key, Value, Rest} ->
	    {request, WhichData, Command, Extras, Key, Value, VbucketID, Opaque, CAS, Rest}
    end;

parse({rest_body, RestBody,
       {response, Command, KeyLength, ExtrasLength, WhichData, Status, BodyLength, Opaque, CAS} = Header},
      NewPacket) ->
    case parse_body(RestBody, ExtrasLength, KeyLength, BodyLength, NewPacket) of
	{rest_body, NewRestBody} ->
	    {rest_body, NewRestBody, Header};
	{ok, Extras, Key, Value, Rest} ->
	    {response, WhichData, Command, Extras, Key, Value, Status, Opaque, CAS, Rest}
    end.

-spec parse_header(Header::binary()) -> any().
parse_header(<<Magic:8/big-unsigned-integer,
	       Opcode:8/big-unsigned-integer,
	       KeyLength:16/big-unsigned-integer,
	       ExtrasLength:8/big-unsigned-integer,
	       DataType:8/big-unsigned-integer,
	       StatusOrVbucketID:16/big-unsigned-integer,
	       BodyLength:32/big-unsigned-integer,
	       Opaque:32,
	       CAS:32>>) ->
    case data_type(DataType) of
	undefined ->
	    bad_data_type;
	WhichData ->
	    case command(Opcode) of
		undefined ->
		    bad_command;
		Command ->
		    case magic(Magic) of
			request ->
			    VbucketID = StatusOrVbucketID,
			    {request, Command, KeyLength, ExtrasLength, WhichData, VbucketID, BodyLength, Opaque, CAS};
			response ->
			    Status = status(StatusOrVbucketID),
			    {response, Command, KeyLength, ExtrasLength, WhichData, Status, BodyLength, Opaque, CAS};
			_Else ->
			    bad_magic
		    end
	    end
    end.

-spec parse_body(Rest::binary(),
		 ExtrasLength::integer(),
		 KeyLength::integer(),
		 BodyLength::integer()) -> any().
parse_body(Body, ExtrasLength, KeyLength, BodyLength) ->
    case erlang:byte_size(Body) >= BodyLength of
	true ->
	    ExtrasBits = ExtrasLength * 8,
	    KeyBits = KeyLength * 8,
	    ValueBits = BodyLength * 8 - ExtrasBits - KeyBits,
	    <<Extras:ExtrasBits/binary,
	      Key:KeyBits/binary,
	      Value:ValueBits/binary,
	      Rest/binary>> = Body,
	    {ok, Extras, Key, Value, Rest};
	false ->
	    {rest_body, Body}
    end.

parse_body(RestBody, ExtrasLength, KeyLength, BodyLength, NewPacket) ->
    parse_body(<<RestBody/binary, NewPacket/binary>>, ExtrasLength, KeyLength, BodyLength).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Magic
%% Magic number identifying the package
-spec magic(MagicNumber::integer()) -> request | response | undefined.
magic(16#80) ->
    request;
magic(16#81) ->
    response;
magic(_) ->
    undefined.

%% Opcode
%% Command code
-spec command(Opcode::integer()) -> Command::atom().
command(16#00) ->
    get;
command(16#01) ->
    set;
command(16#02) ->
    add;
command(16#03) ->
    replace;
command(16#04) ->
    delete;
command(16#05) ->
    increment;
command(16#06) ->
    decrement;
command(16#07) ->
    quit;
command(16#08) ->
    flush;
command(16#09) ->
    getq;
command(16#0a) ->
    noop;
command(16#0b) ->
    version;
command(16#0c) ->
    getk;
command(16#0d) ->
    getkq;
command(16#0e) ->
    append;
command(16#0f) ->
    prepend;
command(16#10) ->
    stat;
command(16#11) ->
    setq;
command(16#12) ->
    addq;
command(16#13) ->
    replaceq;
command(16#14) ->
    deleteq;
command(16#15) ->
    incrementq;
command(16#16) ->
    decrementq;
command(16#17) ->
    quitq;
command(16#18) ->
    flushq;
command(16#19) ->
    appendq;
command(16#1a) ->
    prependq;
command(16#1b) ->
    verbosity;
command(16#1c) ->
    touch;
command(16#1d) ->
    gat;
command(16#1e) ->
    gatq;
command(16#20) ->
    sasl_list_mechs;
command(16#21) ->
    sasl_auth;
command(16#22) ->
    sasl_step;
%% [23--2f] not used
command(16#30) ->
    rget;
command(16#31) ->
    rset;
command(16#32) ->
    rsetq;
command(16#33) ->
    rappend;
command(16#34) ->
    rappendq;
command(16#35) ->
    rprepend;
command(16#36) ->
    rprependq;
command(16#37) ->
    rdelete;
command(16#38) ->
    rdeleteq;
command(16#39) ->
    rincrement;
command(16#3a) ->
    rincrementq;
command(16#3b) ->
    rdecrement;
command(16#3c) ->
    rdecrementq;
command(16#3d) ->
    set_vbucket;
command(16#3e) ->
    get_vbucket;
command(16#3f) ->
    del_vbucket;
command(16#40) ->
    tap_connect;
command(16#41) ->
    tap_mutation;
command(16#42) ->
    tap_delete;
command(16#43) ->
    tap_flush;
command(16#44) ->
    tap_opaque;
command(16#45) ->
    tap_vbucket_set;
command(16#46) ->
    tap_checkpoint_start;
command(16#47) ->
    tap_checkpoint_end;
command(_Opcode) ->
    undefined.
%% Key length
%% Length in bytes of the text key that follows the command extras

%% Status
%% Status of the response (non-zero on error)
-spec status(StatusNubmer::integer()) -> Status::atom().
status(16#0000) ->
    no_error;
status(16#0001) ->
    key_not_found;
status(16#0002) ->
    key_exists;
status(16#0003) ->
    value_too_large;
status(16#0004) ->
    invalid_arguments;
status(16#0005) ->
    item_not_stored;
status(16#0006) ->
    incr_or_dcr_on_non_numeric_value;
status(16#0007) ->
    vbucket_in_another;
status(16#0008) ->
    auth_error;
status(16#0009) ->
    auth_continue;
%% [000a~0080] not used
status(16#0081) ->
    unknown_command;
status(16#0082) ->
    out_of_memory;
status(16#0083) ->
    not_supported;
status(16#0084) ->
    internal_error;
status(16#0085) ->
    busy;
status(16#0086) ->
    temporary_failure;
status(_Else) ->
    undefined.

%% Extras length
%% Length in bytes of the command extras

%% Data type
%% Reserved for future use
-spec data_type(DataType::integer()) -> atom().
data_type(16#00) ->
    raw_byte;
data_type(_) ->
    undefined.

%% vbucket id
%% The virtual bucket for this command

%% Total body length
%% Length in bytes of extra + key + value

%% Opaque
%% Will be copied back to you in the response

%% CAS
%% Data version check
