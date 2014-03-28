#
all: erl cpp
#
erl: user.erl echo.erl
#
cpp: user.tcc echo.tcc rm_skeleton
#
move_skeleton:
	@mv gen-cpp/*.skeleton.* .
#
rm_skeleton:
	@rm gen-cpp/*.skeleton.*
#
%.tcc: 	%.thrift
#	@thrift --gen cpp:cob_style=true:pure_enums=true:templates=true $<
	@thrift -r --gen cpp $<
#
%.java: %.thrift
	@thrift -r --gen java $<
#
clean:
	@rm -rf gen-cpp gen-java *~ *.skeleton.* gen-erl
#
%.erl: %.thrift
	@thrift -r --gen erl $<
