%% constants
%% enum SpdyHeadersFlags {
%%   HEADERS_FLAG_END_HEADERS = 0x4,
%%   HEADERS_FLAG_PRIORITY = 0x8
%% };

%% // Flags on the SETTINGS control frame.
%% enum SpdySettingsControlFlags {
%%   SETTINGS_FLAG_CLEAR_PREVIOUSLY_PERSISTED_SETTINGS = 0x1
%% };

%% enum Http2SettingsControlFlags {
%%   SETTINGS_FLAG_ACK = 0x1,
%% };

%% // Flags for settings within a SETTINGS frame.
%% enum SpdySettingsFlags {
%%   SETTINGS_FLAG_NONE = 0x0,
%%   SETTINGS_FLAG_PLEASE_PERSIST = 0x1,
%%   SETTINGS_FLAG_PERSISTED = 0x2
%% };

%% // List of known settings.
%% enum SpdySettingsIds {
%%   SETTINGS_UPLOAD_BANDWIDTH,
%%   SETTINGS_DOWNLOAD_BANDWIDTH,
%%   // Network round trip time in milliseconds.
%%   SETTINGS_ROUND_TRIP_TIME,
%%   // The maximum number of simultaneous live streams in each direction.
%%   SETTINGS_MAX_CONCURRENT_STREAMS,
%%   // TCP congestion window in packets.
%%   SETTINGS_CURRENT_CWND,
%%   // Downstream byte retransmission rate in percentage.
%%   SETTINGS_DOWNLOAD_RETRANS_RATE,
%%   // Initial window size in bytes
%%   SETTINGS_INITIAL_WINDOW_SIZE,
%%   // HPACK header table maximum size.
%%   SETTINGS_HEADER_TABLE_SIZE,
%%   // Whether or not server push (PUSH_PROMISE) is enabled.
%%   SETTINGS_ENABLE_PUSH,
%% };

%% 2.2.2 Data frames

%% +----------------------------------+
%% |C|       Stream-ID (31bits)       |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+
-record(spdy_data, {
	  stream_id :: integer(),
	  flags     :: integer(),
	  data      :: binary()
	 }).

%% // Flags on data packets.
%% enum SpdyDataFlags {
-define(DATA_FLAG_NONE, 16#00).
-define(DATA_FLAG_FIN, 16#01).
%% DATA_FLAG_END_SEGMENT = 0x02,
%% DATA_FLAG_PAD_LOW = 0x10,
%% DATA_FLAG_PAD_HIGH = 0x20

%% 2.2.1 Control frames

%% +----------------------------------+
%% |C| Version(15bits) | Type(16bits) |
%% +----------------------------------+
%% | Flags (8)  |  Length (24 bits)   |
%% +----------------------------------+
%% |               Data               |
%% +----------------------------------+
-record(spdy_control, {
	  version = $l :: integer(),
	  type         :: integer(),
	  flags        :: integer(),
	  length       :: integer(),
	  data         :: binary()
	 }).

%% // Types of SPDY frames.
%% enum SpdyFrameType {
-define(DATA, 0).
-define(FIRST_CONTROL_TYPE, 1).
-define(SYN_STREAM, 1).
-define(SYN_REPLY, 2).
-define(RST_STREAM, 3).
-define(SETTINGS, 4).
-define(NOOP, 5). %% Because it is valid in SPDY/2, kept for identifiability/enum order.
-define(PING, 6).
-define(GOAWAY, 7).
-define(HEADERS, 8).
-define(WINDOW_UPDATE, 9).
-define(CREDENTIAL, 10). %% // No longer valid.  Kept for identifiability/enum order.
-define(BLOCKED, 11).
-define(PUSH_PROMISE, 12).
-define(CONTINUATION, 13).
-define(LAST_CONTROL_TYPE, 13).

%% // Flags on control packets
%% enum SpdyControlFlags {
-define(CONTROL_FLAG_NONE, 0).
-define(CONTROL_FLAG_FIN, 1).
-define(CONTROL_FLAG_UNIDIRECTIONAL, 2).

-record(spdy_syn_stream, {
	  version = $l   :: integer(),
	  flags = 0      :: integer(),
	  stream_id      :: integer(),
	  assoc_id       :: integer(),
	  priority = 0   :: integer(),
	  slot = 0       :: integer(),
	  headers = <<>> :: binary()
	 }).

-record(spdy_syn_reply, {
	  version = $l   :: integer(),
	  flags = 0      :: integer(),
	  stream_id      :: integer(),
	  headers = <<>> :: binary()
	 }).
%% enum SpdyPushPromiseFlags {
-define(PUSH_PROMISE_FLAG_END_PUSH_PROMISE, 16#4).

-record(spdy_rst_stream, {
	  version = $l :: integer(),
	  flags = 0    :: integer(),
	  stream_id    :: integer(),
	  status_code  :: integer()
	 }).
%% // Status codes for RST_STREAM frames.
%% enum SpdyRstStreamStatus {
-define(RST_STREAM_INVALID,  0).
-define(RST_STREAM_PROTOCOL_ERROR, 1).
-define(RST_STREAM_INVALID_STREAM, 2).
-define(RST_STREAM_REFUSED_STREAM, 3).
-define(RST_STREAM_UNSUPPORTED_VERSION, 4).
-define(RST_STREAM_CANCEL, 5).
-define(RST_STREAM_INTERNAL_ERROR, 6).
-define(RST_STREAM_FLOW_CONTROL_ERROR, 7).
-define(RST_STREAM_STREAM_IN_USE, 8).
-define(RST_STREAM_STREAM_ALREADY_CLOSED, 9).
-define(RST_STREAM_INVALID_CREDENTIALS, 10).
-define(RST_STREAM_FRAME_TOO_LARGE, 11).
-define(RST_STREAM_NUM_STATUS_CODES, 12).

-record(spdy_ping, {
	  version = $l :: integer(),
	  id           :: integer()
	 }).

%% enum SpdyPingFlags {
-define(PING_FLAG_ACK,  16#1).

-record(spdy_goaway, {
	  version = $l :: integer(),
	  last_good_id :: integer(),
	  status_code  :: integer()
	 }).

%% // Status codes for GOAWAY frames.
%% enum SpdyGoAwayStatus {
-define(GOAWAY_INVALID, -1).
-define(GOAWAY_OK, 0).
-define(GOAWAY_PROTOCOL_ERROR, 1).
-define(GOAWAY_INTERNAL_ERROR, 2).
-define(GOAWAY_NUM_STATUS_CODES, 3).
