-module(example).

example() ->
  ok = application:start(rate_app),
  {ok, SecondRate} = rate:add(100),
  io:format("the current rate is ~p ~n", [SecondRate]),

  case rate:remain(99, 150) of
    {ok, _Current_rate} ->
      proceed;
    {error, OverLimit} ->
      AbsOverLimit = abs(OverLimit),
      io:format("Over the limite of ~p~n", [AbsOverLimit])
    end,
  rate:print(), %print the current rates
  end_of_fun.
