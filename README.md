### folsom_cowboy, a cowboy based wrapper for folsom

This is a cowboy handler that exposes folsom metrics via http using cowboy(cowboy 2.0.0).

#### api

Query the list of available metrics:

        $ curl http://localhost:5565/_metrics

Query a specific metric:

        $ curl http://localhost:5565/_metrics/name

Query Erlang VM metrics:

        $ curl http://localhost:5565/_memory

Query Erlang VM stats:

        $ curl http://localhost:5565/_statistics

Query Erlang VM information:

        $ curl http://localhost:5565/_system

#### output

        $ curl http://localhost:5565/_metrics/a
        {"value":{"min":1,"max":1000,"mean":322.2,"median":100,"variance":185259.19999999998,"standard_deviation":430.4174717643325,"skewness":1.2670136514902162,"kurtosis":-1.2908313302242205,"percentile":{"75":500,"95":1000,"99":1000,"999":1000},"histogram":{"10":2,"20":0,"30":0,"50":0,"100":1,"200":0,"300":0,"400":0,"500":1,"1000":1,"99999999999999":0}}}


         $ curl http://localhost:5565/_metrics/test

         {"value":{"1303483997384193":{"event":"asdfasdf"}}}


         $ curl http://localhost:5565/_memory
         {"total":11044608,"processes":3240936,"processes_used":3233888,"system":7803672,"atom":532137,"atom_used":524918,"binary":696984,"code":4358030,"ets":385192}

#### JSONP

If enabled, you can request JSONP responses by including the query-string parameter:

        $ curl http://localhost:5565/_metrics?jsonp=AvailableMetrics
        AvailableMetrics=[]

The returned javascript code is an assignment rather than a function call, so the parameter must be a valid javascript identified, in particular it must be a string of letters, digits or underscores, otherwise a `400` error is returned.

[folsom_webmachine]: https://github.com/boundary/folsom_webmachine
[folsom]: https://github.com/boundary/folsom
[jsonp]: http://en.wikipedia.org/wiki/JSONP
