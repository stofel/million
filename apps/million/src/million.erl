-module(million).

-include_lib("eunit/include/eunit.hrl").

-export([
    ping/0,
    ping/2,
    hello/2
]).


ping() ->
    <<"Usage: million:ping(\<\<\"123\"\>\>, with_init) or million:ping(\<\<\"123\"\>\>, without_init)">>.

-spec ping(WorkerName::binary(), with_init|without_init) -> pong.
ping(WorkerName, with_init) ->
    million_worker:call(WorkerName, {million_worker_msg, ping});
ping(WorkerName, without_init) ->
    million_worker:info(WorkerName, {million_worker_msg, ping}).



-spec hello(WorkerName::binary(), Name::binary()) -> {hello, binary(), binary()}.
hello(WorkerName, Name) ->
    Msg = {hello, Name},
    million_worker:call(WorkerName, {million_worker_msg, Msg}).



%% Tests
many_workers_test_() ->
    PingFun = fun
        Fu(N, M) when N < M ->
            pong = million:ping(integer_to_binary(N), with_init),
            Fu(N+1, M);
        Fu(_N,_M) ->
            ?debugFmt("Cluster stats ~p", [ecldb:stat(million_cluster)])
    end,
    PingFun(1, 1000*1000 + 1),
    {timeout, 60, [
        ?_assert(pong =:= ping(<<"1">>, without_init)),
        ?_assert(pong =:= ping(<<"1000000">>, without_init)),
        ?_assert(not_started =:= ping(<<"1000001">>, without_init))
    ]}.
