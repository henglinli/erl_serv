%% @doc Interface for riak_searchng-admin commands.
-module(serv_console).

-export([join/1,
	 staged_join/1,
	 leave/1,
	 remove/1,
	 ringready/1,
	 cluster_info/1,
	 down/1]).

join([NodeStr]) ->
    join(NodeStr, fun riak_core:join/1,
	 "Sent join request to ~s~n", [NodeStr]).

staged_join([NodeStr]) ->
    Node = list_to_atom(NodeStr),
    join(NodeStr, fun riak_core:staged_join/1,
	 "Success: staged join request for ~p to ~p~n", [node(), Node]).

join(NodeStr, JoinFn, SuccessFmt, SuccessArgs) ->
    try
	case JoinFn(NodeStr) of
	    ok ->
		io:format(SuccessFmt, SuccessArgs),
		ok;
	    {error, not_reachable} ->
		io:format("Node ~s is not reachable!~n", [NodeStr]),
		error;
	    {error, different_ring_sizes} ->
		io:format("Failed: ~s has a different ring_creation_size~n",
			  [NodeStr]),
		error;
	    {error, unable_to_get_join_ring} ->
		io:format("Failed: Unable to get ring from ~s~n", [NodeStr]),
		error;
	    {error, not_single_node} ->
		io:format("Failed: This node is already a member of a "
			  "cluster~n"),
		error;
	    {error, self_join} ->
		io:format("Failed: This node cannot join itself in a "
			  "cluster~n"),
		error;
	    {error, _} ->
		io:format("Join failed. Try again in a few moments.~n", []),
		error
	end
    catch
	Exception:Reason ->
	    lager:error("Join failed ~p:~p", [Exception, Reason]),
	    io:format("Join failed, see log for details~n"),
	    error
    end.


leave([]) ->
    try
	case riak_core:leave() of
	    ok ->
		io:format("Success: ~p will shutdown after handing off "
			  "its data~n", [node()]),
		ok;
	    {error, already_leaving} ->
		io:format("~p is already in the process of leaving the "
			  "cluster.~n", [node()]),
		ok;
	    {error, not_member} ->
		io:format("Failed: ~p is not a member of the cluster.~n",
			  [node()]),
		error;
	    {error, only_member} ->
		io:format("Failed: ~p is the only member.~n", [node()]),
		error
	end
    catch
	Exception:Reason ->
	    lager:error("Leave failed ~p:~p", [Exception, Reason]),
	    io:format("Leave failed, see log for details~n"),
	    error
    end.

remove([Node]) ->
    try
	case riak_core:remove(list_to_atom(Node)) of
	    ok ->
		io:format("Success: ~p removed from the cluster~n", [Node]),
		ok;
	    {error, not_member} ->
		io:format("Failed: ~p is not a member of the cluster.~n",
			  [Node]),
		error;
	    {error, only_member} ->
		io:format("Failed: ~p is the only member.~n", [Node]),
		error
	end
    catch
	Exception:Reason ->
	    lager:error("Remove failed ~p:~p", [Exception, Reason]),
	    io:format("Remove failed, see log for details~n"),
	    error
    end.

down([Node]) ->
    try
	case riak_core:down(list_to_atom(Node)) of
	    ok ->
		io:format("Success: ~p marked as down~n", [Node]),
		ok;
	    {error, legacy_mode} ->
		io:format("Cluster is currently in legacy mode~n"),
		ok;
	    {error, is_up} ->
		io:format("Failed: ~s is up~n", [Node]),
		error;
	    {error, not_member} ->
		io:format("Failed: ~p is not a member of the cluster.~n",
			  [Node]),
		error;
	    {error, only_member} ->
		io:format("Failed: ~p is the only member.~n", [Node]),
		error
	end
    catch
	Exception:Reason ->
	    lager:error("Down failed ~p:~p", [Exception, Reason]),
	    io:format("Down failed, see log for details~n"),
	    error
    end.

%% Check if all nodes in the cluster agree on the partition assignment
-spec(ringready([]) -> ok | error).
ringready([]) ->
    try
	case riak_core_status:ringready() of
	    {ok, Nodes} ->
		io:format("TRUE All nodes agree on the ring ~p\n", [Nodes]);
	    {error, {different_owners, N1, N2}} ->
		io:format("FALSE Node ~p and ~p list different partition owners\n", [N1, N2]),
		error;
	    {error, {nodes_down, Down}} ->
		io:format("FALSE ~p down.  All nodes need to be up to check.\n", [Down]),
		error
	end
    catch
	Exception:Reason ->
	    lager:error("Ringready failed ~p:~p", [Exception,
		    Reason]),
	    io:format("Ringready failed, see log for details~n"),
	    error
    end.

cluster_info([OutFile|Rest]) ->
    try
	case lists:reverse(atomify_nodestrs(Rest)) of
	    [] ->
		cluster_info:dump_all_connected(OutFile);
	    Nodes ->
		cluster_info:dump_nodes(Nodes, OutFile)
	end
    catch
	error:{badmatch, {error, eacces}} ->
	    io:format("Cluster_info failed, permission denied writing to ~p~n", [OutFile]);
	error:{badmatch, {error, enoent}} ->
	    io:format("Cluster_info failed, no such directory ~p~n", [filename:dirname(OutFile)]);
	error:{badmatch, {error, enotdir}} ->
	    io:format("Cluster_info failed, not a directory ~p~n", [filename:dirname(OutFile)]);
	Exception:Reason ->
	    lager:error("Cluster_info failed ~p:~p",
		[Exception, Reason]),
	    io:format("Cluster_info failed, see log for details~n"),
	    error
    end.

atomify_nodestrs(Strs) ->
    lists:foldl(fun("local", Acc) -> [node()|Acc];
		   (NodeStr, Acc) -> try
					 [list_to_existing_atom(NodeStr)|Acc]
				     catch error:badarg ->
					 io:format("Bad node: ~s\n", [NodeStr]),
					 Acc
				     end
		end, [], Strs).

%% reload_code([]) ->
%%     case app_helper:get_env(riak_kv, add_paths) of
%%	List when is_list(List) ->
%%	    _ = [ reload_path(filename:absname(Path)) || Path <- List ],
%%	    ok;
%%	_ -> ok
%%     end.

%% reload_path(Path) ->
%%     {ok, Beams} = file:list_dir(Path),
%%     [ reload_file(filename:absname(Beam, Path)) || Beam <- Beams, ".beam" == filename:extension(Beam) ].

%% reload_file(Filename) ->
%%     Mod = list_to_atom(filename:basename(Filename, ".beam")),
%%     case code:is_loaded(Mod) of
%%	{file, Filename} ->
%%	    code:soft_purge(Mod),
%%	    {module, Mod} = code:load_file(Mod),
%%	    io:format("Reloaded module ~w from ~s.~n", [Mod, Filename]);
%%	{file, Other} ->
%%	    io:format("CONFLICT: Module ~w originally loaded from ~s, won't reload from ~s.~n", [Mod, Other, Filename]);
%%	_ ->
%%	    io:format("Module ~w not yet loaded, skipped.~n", [Mod])
%%     end.
