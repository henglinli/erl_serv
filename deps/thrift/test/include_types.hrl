-ifndef(_include_types_included).
-define(_include_types_included, yeah).
-include("thriftTest_types.hrl").


%% struct includeTest

-record(includeTest, {bools = #bools{} :: #bools{}}).

-endif.
