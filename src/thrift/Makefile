#
erl: user_service_erlang

all: user_service_java user_service_cpp user_service_erlang move_skeleton
#
user_service_cpp: time.tcc date.tcc location.tcc account.tcc nakama.tcc captcha.tcc profile.tcc user.tcc
#
user_service_java: time.java date.java location.java account.java nakama.java captcha.java profile.java user.java
#
user_service_erlang: time.erl date.erl location.erl account.erl nakama.erl captcha.erl profile.erl user.erl
#
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
	@thrift --gen erl $<
