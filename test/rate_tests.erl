-module(rate_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

stats_test() ->
  %{ok, _Pid} = rate_app:start(no_type, no_args),
  ok = application:start(rate_app),
  rate:add(100),
  ?assertEqual({ok, 101}, rate:add(1)),
  timer:sleep(500),
  ?assertEqual({ok, 120}, rate:add(19)),
  timer:sleep(305),
  ?assertEqual({ok, 125}, rate:add(5)),
  timer:sleep(980),
  ?assertEqual({ok, 13}, rate:add(13)),

  ?assertEqual({error, -12}, rate:remain(99, 100)),
  timer:sleep(580),
  ?assertEqual({ok, 5}, rate:remain(95, 100)),
  end_of_test.

-endif.
