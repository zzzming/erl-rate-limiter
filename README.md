rate
=====

rate is an application to count the rates and provide rate limit count.

Use
---
 To start the rate app.
```
   1> ok = application:start(rate_app).
   ok
```

 To count the rate of 1. An automatic timer clears rate counts every second for the second bin.
```
   2> rate:add(1).
   {ok,1}
```

 To count the rate of 100 and check the current counts for the second bin.
```
   3> {ok, 100} = rate:add(100).
   {ok,100}
```

 Print the rate counters for the latest second bin, latest minute, 5 and 15 minutes bin, and the peak count per second.
```
   4> rate:print().
   #rates{second = 0,minute = 101,last5min = 101,last15min = 101,peak = 100}ok
```

 Check the rate limit with an incremental count.
```
   5> {error, -1} = rate:remain(101, 100). % rate limit over 1
   {error,-1}
   6> {ok, 399} = rate:remain(1, 400).  % still 399 counts left for the current second
   {ok,399}
```

Build
-----
Commands to compile, static analysis, and unit testing.
```
    $ rebar3 compile
    $ rebar3 dialyzer
    $ rebar3 eunit
```
