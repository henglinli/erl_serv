%% reference https://github.com/RJ/erlang-spdy
%% Copyright (c) 2014,HenryLee<henglinli@gmail.com>
%% just stream related function

-module(serv_spdy).
-include("serv_spdy.hrl").
%% Parse.
-export([split_data/1]).
-export([parse_frame/1]).

%% Build.

%% split date, got frame
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

%% parse data frame
parse_frame(<< 0:1, %% alwalys 0
	       StreamID:31/big-unsigned-integer,
	       Flags:8/big-unsigned-integer,
	       Length:24/big-unsigned-integer,
	       Data:Length/binary,
	       Rest/binary>>) ->
    {#spdy_data{stream_id = StreamID,
		flags = Flags,
		length = Length,
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
