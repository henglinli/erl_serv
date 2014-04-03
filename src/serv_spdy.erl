%% reference https://github.com/RJ/erlang-spdy
%% Copyright (c) 2014,HenryLee<henglinli@gmail.com>
%% just stream related function

-module(serv_spdy).
-include("serv_spdy.hrl").
%% utils
-export([data_flag_code/1, data_flag_name/1]).
-export([control_flag_code/1, control_flag_name/1]).
-export([goaway_status_code/1, goaway_status_name/1]).
%% parse
-export([split_data/1]).
-export([parse_frame/1]).
%% build
-export([build_frame/1]).
%% data flag
-spec data_flag_code(atom()) -> integer().
data_flag_code(data_flag_none) ->
    ?DATA_FLAG_NONE;
data_flag_code(data_flag_fin) ->
    ?DATA_FLAG_FIN;
data_flag_code(_) ->
    ?DATA_FLAG_NONE.

-spec data_flag_name(integer()) -> atom().
data_flag_name(?DATA_FLAG_NONE) ->
    data_flag_none;
data_flag_name(?DATA_FLAG_FIN) ->
    data_flag_fin;
data_flag_name(_) ->
    undefined.

%% control flag
-spec control_flag_code(atom()) -> integer().
control_flag_code(control_flag_none) ->
    ?CONTROL_FLAG_NONE;
control_flag_code(control_flag_fin) ->
    ?CONTROL_FLAG_FIN;
control_flag_code(control_flag_unidirectional) ->
    ?CONTROL_FLAG_UNIDIRECTIONAL;
control_flag_code(_) ->
    ?CONTROL_FLAG_NONE.

-spec control_flag_name(integer()) -> atom().
control_flag_name(?CONTROL_FLAG_NONE) ->
    control_flag_none;
control_flag_name(?CONTROL_FLAG_FIN) ->
    control_flag_fin;
control_flag_name(?CONTROL_FLAG_UNIDIRECTIONAL) ->
    control_flag_unidirectional;
control_flag_name(_) ->
    undefined.

%% goaway status
-spec goaway_status_code(atom()) -> integer().
goaway_status_code(goaway_ok) ->
    ?GOAWAY_OK;
goaway_status_code(goaway_protocol_error) ->
    ?GOAWAY_PROTOCOL_ERROR;
goaway_status_code(goaway_internal_error) ->
    ?GOAWAY_INTERNAL_ERROR;
goaway_status_code(_) ->
    ?GOAWAY_OK.

-spec goaway_status_name(integer()) -> atom().
goaway_status_name(?GOAWAY_OK) ->
    goaway_ok;
goaway_status_name(?GOAWAY_PROTOCOL_ERROR) ->
    goaway_protocol_error;
goaway_status_name(?GOAWAY_INTERNAL_ERROR) ->
    goaway_internal_error;
goaway_status_name(_) ->
    undefined.

%% split date, got frame
-spec split_data(binary()) -> {true, binary(), binary()} | false.
split_data(Data = << _:40, Length:24, _/binary >>)
  when byte_size(Data) >= Length + 8 ->
    Length2 = Length + 8,
    << Frame:Length2/binary, Rest/binary >> = Data,
    {true, Frame, Rest};

split_data(_) ->
    false.
%% 2.2.2 Data frames

%% +----------------------------------+
%% |C|       Stream-ID (31bits)       |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+

-spec parse_frame(binary()) ->{#spdy_data{}
			       | #spdy_syn_stream{}
			       | #spdy_syn_reply{}
			       | #spdy_rst_stream{}
			       | #spdy_ping{}
			       | #spdy_goaway{},
			       binary()}
				  | undefined.
%% parse data frame
parse_frame(<< 0:1, %% alwalys 0
	       StreamID:31/big-unsigned-integer,
	       Flags:8/big-unsigned-integer,
	       Length:24/big-unsigned-integer,
	       Data:Length/binary,
	       Rest/binary>>) ->
    {#spdy_data{stream_id = StreamID,
		flags = Flags,
		data = Data},
     Rest};
%% 2.2.1 Control frames

%% +----------------------------------+
%% |C| Version(15bits) | Type(16bits) |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+

%% parse control frame
parse_frame(<< 1:1, %% always 1
	       Version:15/big-unsigned-integer,
	       Type:16/big-unsigned-integer,
	       Flags:8/big-unsigned-integer,
	       Length:24/big-unsigned-integer,
	       Data:Length/binary,
	       Rest/binary >>) ->
    SpdyControl= #spdy_control{
		     version = Version,
		     type = Type,
		     flags = Flags,
		     length = Length,
		     data = Data},
    {parse_control_frame(SpdyControl), Rest};

parse_frame(_) ->
    undefined.

-spec parse_control_frame(binary()) ->
				 #spdy_syn_stream{}
				     | #spdy_syn_reply{}
				     | #spdy_rst_stream{}
				     | #spdy_ping{}
				     | #spdy_goaway{}.
%% +------------------------------------+
%% |1|    version    |         1        |
%% +------------------------------------+
%% |  Flags (8)  |  Length (24 bits)    |
%% +------------------------------------+
%% |X|           Stream-ID (31bits)     |
%% +------------------------------------+
%% |X| Associated-To-Stream-ID (31bits) |
%% +------------------------------------+
%% | Pri|Unused | Slot |                |
%% +-------------------+                |
%% | Number of Name/Value pairs (int32) |   <+
%% +------------------------------------+    |
%% |     Length of name (int32)         |    | This section is the
%% +------------------------------------+    | "Name/Value
%% |           Name (string)            |    | Header Block",
%% +------------------------------------+    | and is compressed.
%% |     Length of value  (int32)       |    |
%% +------------------------------------+    |
%% |          Value   (string)          |    |
%% +------------------------------------+    |
%% |           (repeats)                |   <+


parse_control_frame(#spdy_control{
		     version = Version,
		     type = ?SYN_STREAM,
		     flags = Flags,
		     length = _Length,
		     data = <<_:1, StreamID:32/big-unsigned-integer,
			      _:1, AssocStreamID:31/big-unsigned-integer,
			      Priority:3/big-unsigned-integer,
			      _Unused:5/binary,
			      Slot:8/big-unsigned-integer,
			      NVHeaderBlock/binary
			    >>}) ->
    #spdy_syn_stream{version = Version,
		     flags = Flags,
		     stream_id = StreamID,
		     assoc_id = AssocStreamID,
		     priority = Priority,
		     slot = Slot,
		     headers = NVHeaderBlock};

%% +------------------------------------+
%% |1|    version    |         2        |
%% +------------------------------------+
%% |  Flags (8)  |  Length (24 bits)    |
%% +------------------------------------+
%% |X|           Stream-ID (31bits)     |
%% +------------------------------------+
%% | Number of Name/Value pairs (int32) |   <+
%% +------------------------------------+    |
%% |     Length of name (int32)         |    | This section is the "Name/Value
%% +------------------------------------+    | Header Block", and is compressed.
%% |           Name (string)            |    |
%% +------------------------------------+    |
%% |     Length of value  (int32)       |    |
%% +------------------------------------+    |
%% |          Value   (string)          |    |
%% +------------------------------------+    |
%% |           (repeats)                |   <+

parse_control_frame(#spdy_control{
		       version = Version,
		       type = ?SYN_REPLY,
		       flags = Flags,
		       length = _Length,
		       data = <<_:1, StreamID:32/big-unsigned-integer,
				NVHeaderBlock/binary >>
		      }) ->
    #spdy_syn_reply{version = Version,
		    flags = Flags,
		    stream_id = StreamID,
		    headers = NVHeaderBlock};

%% +----------------------------------+
%% |1|   version    |         3       |
%% +----------------------------------+
%% | Flags (8)  |         8           |
%% +----------------------------------+
%% |X|          Stream-ID (31bits)    |
%% +----------------------------------+
%% |          Status code             |
%% +----------------------------------+

parse_control_frame(#spdy_control{
		       version = Version,
		       type = ?RST_STREAM,
		       flags = _Flags,
		       length = 8,
		       data = <<_:1, StreamID:32/big-unsigned-integer,
				StatusCode:32/big-unsigned-integer>>
		      }) ->
    #spdy_rst_stream{version = Version,
		     flags = 0,
		     stream_id = StreamID,
		     status_code = StatusCode};

%% +----------------------------------+
%% |1|   version    |         6       |
%% +----------------------------------+
%% | 0 (flags) |     4 (length)       |
%% +----------------------------------|
%% |            32-bit ID             |
%% +----------------------------------+

parse_control_frame(#spdy_control{
		       version = Version,
		       type = ?PING,
		       flags = _Flags,
		       length = 4,
		       data = <<PingID:32/big-unsigned-integer
			     >>}) ->
    #spdy_ping{version = Version,
	       id = PingID};

%% +----------------------------------+
%% |1|   version    |         7       |
%% +----------------------------------+
%% | 0 (flags) |     8 (length)       |
%% +----------------------------------|
%% |X|  Last-good-stream-ID (31 bits) |
%% +----------------------------------+
%% |          Status code             |
%% +----------------------------------+

parse_control_frame(#spdy_control{
		       version = Version,
		       type = ?GOAWAY,
		       flags = _Flags,
		       length = 8,
		       data = <<_:1, LastGoodID:31/big-unsigned-integer,
				StatusCode:32/big-unsigned-integer >>
		      }) ->
    #spdy_goaway{version = Version,
		 last_good_id = LastGoodID,
		 status_code = StatusCode}.

-spec build_frame(#spdy_data{}
		  | #spdy_syn_stream{}
		  | #spdy_syn_reply{}
		  | #spdy_rst_stream{}
		  | #spdy_ping{}
		  | #spdy_goaway{}) ->
			 binary().
%% build frame
%% +----------------------------------+
%% |C|       Stream-ID (31bits)       |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+
build_frame(#spdy_data{stream_id = StreamID,
		       flags = Flags,
		       data = Data}) ->
	    Length = size(Data),
	    << 0:1, StreamID:31/big-unsigned-integer,
	       Flags:8/big-unsigned-integer,
	       Length:24/big-unsigned-integer,
	       Data/binary
	    >>;

%% +------------------------------------+
%% |1|    version    |         1        |
%% +------------------------------------+
%% |  Flags (8)  |  Length (24 bits)    |
%% +------------------------------------+
%% |X|           Stream-ID (31bits)     |
%% +------------------------------------+
%% |X| Associated-To-Stream-ID (31bits) |
%% +------------------------------------+
%% | Pri|Unused | Slot |                |
%% +-------------------+                |
%% | Number of Name/Value pairs (int32) |   <+
%% +------------------------------------+    |
%% |     Length of name (int32)         |    | This section is the
%% +------------------------------------+    | "Name/Value
%% |           Name (string)            |    | Header Block",
%% +------------------------------------+    | and is compressed.
%% |     Length of value  (int32)       |    |
%% +------------------------------------+    |
%% |          Value   (string)          |    |
%% +------------------------------------+    |
%% |           (repeats)                |   <+

build_frame(#spdy_syn_stream{version = Version = $l,
			     flags = Flags,
			     stream_id = StreamID,
			     assoc_id = AssocStreamID,
			     priority = Priority = 0,
			     slot = Slot = 0,
			     headers = _Headers = <<>>
			    }) ->
    build_control_frame(Version, ?SYN_STREAM, Flags,
			<<0:1, StreamID:31/big-unsigned-integer,
			  0:1, AssocStreamID:31/big-unsigned-integer,
			  Priority:3/big-unsigned-integer,
			  0:5, % unuesd
			  Slot:8/big-unsigned-integer
			>>);

%% +------------------------------------+
%% |1|    version    |         2        |
%% +------------------------------------+
%% |  Flags (8)  |  Length (24 bits)    |
%% +------------------------------------+
%% |X|           Stream-ID (31bits)     |
%% +------------------------------------+
%% | Number of Name/Value pairs (int32) |   <+
%% +------------------------------------+    |
%% |     Length of name (int32)         |    | This section is the "Name/Value
%% +------------------------------------+    | Header Block", and is compressed.
%% |           Name (string)            |    |
%% +------------------------------------+    |
%% |     Length of value  (int32)       |    |
%% +------------------------------------+    |
%% |          Value   (string)          |    |
%% +------------------------------------+    |
%% |           (repeats)                |   <+
build_frame(#spdy_syn_reply{version = Version = $l,
			    flags = Flags,
			    stream_id = StreamID,
			    headers = _Headers = <<>>
			   }) ->
    build_control_frame(Version, ?SYN_REPLY, Flags,
			<<0:1, StreamID:31/big-unsigned-integer >>);

%% +----------------------------------+
%% |1|   version    |         3       |
%% +----------------------------------+
%% | Flags (8)  |         8           |
%% +----------------------------------+
%% |X|          Stream-ID (31bits)    |
%% +----------------------------------+
%% |          Status code             |
%% +----------------------------------+
build_frame(#spdy_rst_stream{version = Version = $l,
			     stream_id = StreamID,
			     status_code = StatusCode
			    }) ->
    build_control_frame(Version, ?RST_STREAM, ?CONTROL_FLAG_NONE,
			<<0:1, StreamID:31/big-unsigned-integer,
			  StatusCode:32/big-unsigned-integer
			>>);

%% +----------------------------------+
%% |1|   version    |         6       |
%% +----------------------------------+
%% | 0 (flags) |     4 (length)       |
%% +----------------------------------|
%% |            32-bit ID             |
%% +----------------------------------+
build_frame(#spdy_ping{version = Version = $l,
		       id = PingID
	       }) ->
    build_control_frame(Version, ?PING, ?CONTROL_FLAG_NONE,
			<< PingID:32/big-unsigned-integer >>);

%% +----------------------------------+
%% |1|   version    |         7       |
%% +----------------------------------+
%% | 0 (flags) |     8 (length)       |
%% +----------------------------------|
%% |X|  Last-good-stream-ID (31 bits) |
%% +----------------------------------+
%% |          Status code             |
%% +----------------------------------+
build_frame(#spdy_goaway{version = Version = $l,
			 last_good_id = LastGoodID,
			 status_code = StatusCode
	      }) ->
    build_control_frame(Version, ?GOAWAY, ?CONTROL_FLAG_NONE,
			<<0:1, LastGoodID:31/big-unsigned-integer,
			  StatusCode:32/big-unsigned-integer >>);
build_frame(_) ->
    <<>>.

-spec build_control_frame(integer(), integer(), integer(), binary()) ->
				binary().

build_control_frame(Version = $l, Type, Flags, Data) ->
    Length = size(Data),
    <<1:1, Version:15/big-unsigned-integer,
      Type:16/big-unsigned-integer,
      Flags:8/big-unsigned-integer,
      Length:24/big-unsigned-integer,
      Data/binary
    >>.
