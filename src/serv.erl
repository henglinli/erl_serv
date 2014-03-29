-module(serv).

-include("thrift/gen-erl/serv_thrift.hrl").

-compile(export_all).

-define(VERSION, "1@20140328").

-define(LOGIN_SERVER, 'theChat@127.0.0.1').

connect_to_login_server() ->
    case net_kernel:connect_node(?LOGIN_SERVER) of
	true ->
	    error_logger:info_msg("connected to ~p.~n", [?LOGIN_SERVER]);
	_ ->
	    error_logger:info_msg("connect to ~p failed.~n", [?LOGIN_SERVER])
    end.

debug(Format, Data) ->
    error_logger:info_msg(Format, Data).

handle_function(Function, Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok ->
             ok;
        Else -> {reply, Else}
    end.

handle_error(undefined, Reason) ->
    debug("error: ~p", [Reason]).

'Ping'() ->
    debug("ping()", []),
    ok.

'GetVersion'() ->
    debug("get_version()", []),
    ?VERSION.

'Login'(Account) ->
    {Type, Name, Token} = {Account#account.type,
			   Account#account.name,
			   Account#account.token},
    debug("~p: ~p ~p ~p", [Account, Type, Name, Token]),
    case Type of
	?account_AccountType_KSelf ->
	    case rpc:call(?LOGIN_SERVER, utils, remote_login,
			  [erlang:binary_to_list(Name), erlang:binary_to_list(Token)], 5000) of
		{error, Reason} ->
		    throw(#error{type="Login", message=Reason});
		{badrpc, Reason} ->
		    throw(#error{type="Login", message=Reason});
		ok ->
		    ok
	    end;
	_ ->
	    throw(#error{type="Login", message="Bad account type"})
    end.
