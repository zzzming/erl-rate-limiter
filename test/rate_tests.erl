-module(rate_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

stats_test() ->
  %{ok, _Pid} = rate_app:start(no_type, no_args),
  ok = application:start(rate_app),
  rate:add(100),
  ?assertEqual({ok, 101}, rate:add()), % add() is equivelant to add(1)
  timer:sleep(500),
  ?assertEqual({ok, 120}, rate:add(19)),

  timer:sleep(980),
  ?assertEqual({ok, 13}, rate:add(13)),

  ?assertEqual({error, -12}, rate:remain(99, 100)),

  timer:sleep(3001),
  {rates, 0, Last3second, N, N, N, _Peak} = rate:get_rates(),
  ?assertEqual(true, Last3second < N),
  ?assertEqual(true, Last3second =/= 0),

  % just to add test coverage
  rate:print(),
  application:stop(rate),
  end_of_test.

-endif.
