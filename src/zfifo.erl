-module('zfifo').

%% API exports
-export([node_count/0, get_nodes/0, wait_for_nodes/0, register/0, register/1]).

-define(UTIL, "/opt/local/lib/fifo-tools/fifo").
%%====================================================================
%% API functions
%%====================================================================

node_count() ->
    case is_local() of
        true ->
            {ok, 1};
        _ ->
            case metadata_get() of
                {ok, Data} ->
                    jsxd:get([<<"eqc">>, <<"num_servers">>], Data);
                E ->
                    E
            end
    end.

get_nodes() ->
    case is_local() of
        true ->
            {ok, [atom_to_binary(node(), utf8)]};
        _ ->
            case metadata_get() of
                {ok, Data} ->
                    jsxd:get([<<"eqc">>, <<"nodes">>], Data);
                E ->
                    E
            end
    end.

wait_for_nodes() ->
    {ok, N} = node_count(),
    wait_for_nodes(N).

register() ->
    register(true).

register(V) ->
    case is_local() of
        true ->
            ok;
        _ ->
            Data = maps:put(node(), V, #{}),
            stack_set(#{eqc => #{nodes => Data}})
    end.
%%====================================================================
%% Internal functions
%%====================================================================
wait_for_nodes(N) ->
    case get_nodes() of
        {ok, Nodes} when length(Nodes) =:= N ->
            ok;
        _ ->
            timer:sleep(1000),
            wait_for_nodes(N)
    end.

metadata_get() ->
    exec(#{action => <<"stack-get">>}).
stack_set(Data) ->
    exec(#{action => <<"stack-set">>,  data => Data}).

is_local() ->
    not filelib:is_file("/opt/local/lib/fifo-tools/fifo").

exec(Payload) ->
    JSON = jsx:encode(Payload),
    Spawn = {spawn_executable, ?UTIL},
    Opts = [{args, JSON}, use_stdio, exit_status, binary, stderr_to_stdout],
    Port = open_port(Spawn, Opts),
    wait_for_result(Port, <<>>).

wait_for_result(Port, Acc) ->
    receive
        {Port, {exit_status, 0}} ->
            {ok, jsx:decode(Acc)};
        {Port, {exit_status, E}} ->
            {error, E};
        {Port, {data, Data}} ->
            wait_for_result(Port, <<Acc/binary, Data/binary>>)
    end.
