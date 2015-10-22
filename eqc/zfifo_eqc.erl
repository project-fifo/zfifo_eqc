-module(zfifo_eqc).
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

prop_wait_for_nodes() ->
    ?SETUP(fun() ->
                   zfifo:register(),
                   zfifo:wait_for_nodes(),
                   fun() ->  ok end
           end,
           ?FORALL(_, int(), true)).
