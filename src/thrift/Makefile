#
erl: user.erl echo

echo: echo.erl

move_skeleton:
	@mv gen-cpp/*.skeleton.* .
#
%.tcc: 	%.thrift
#	@thrift --gen cpp:cob_style=true:pure_enums=true:templates=true $<
	@thrift --gen cpp $<
#
%.java: %.thrift
	@thrift --gen java $<
#
clean:
	@rm -rf gen-cpp gen-java *~ *.skeleton.* gen-erl
#
%.erl: %.thrift
	@thrift -r --gen erl $<
