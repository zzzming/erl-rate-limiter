rate
=====

rate is an application to count and limit rates.

Build
-----

    $ rebar3 compile
    $ rebar3 dialyzer

Use
---
 to start the rate app.
```
   rate_app:start().
```

 to count the rate.
```
   rate:add(1).
```
