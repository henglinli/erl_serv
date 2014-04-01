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

parse_control_frame(#spdy_control{
		     version = _Version,
		     type = Type,
		     flags = Flags,
		     length = Length,
		     data = Data}) ->
    true.
