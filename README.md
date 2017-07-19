stepflow_source_swagger
========================

Application that implement REST, Websocket and TCP API endpoints
as source for [steflow](https://github.com/hachreak/stepflow).

Example
-------

```
          +------------------------------------------------------------------+
          |                              Agent 1                             |
User      |                                                                  |
 |        |     Source <---------------> Channel <--------> Sink             |
 +------->| (tcp, REST, Websocket)     (rabbitmq)       (echo on console)    |
   POST   |                                                                  |
   Event  +------------------------------------------------------------------+
 <<"hello">>
```

    $ rebar3 auto --sname pippo --apps stepflow_source_swagger

    1> SrcCtx = {[{stepflow_interceptor_counter, #{header => mycounter}}, {stepflow_interceptor_echo, #{}}], #{}}.
    2> Input = {stepflow_source_swagger_source, SrcCtx}.
    3> {ok, SkCtx2} = stepflow_sink:config(stepflow_sink_echo, nope, [{stepflow_interceptor_echo, {}}]).
    4> ChCtx2 = {stepflow_channel_rabbitmq, #{}, SkCtx2}.
    5> Output = [ChCtx2].
    6> {PidSub, PidS, PidC} = stepflow_agent_sup:new(Input, Output).

    # Append a new event
    $ http POST http://0.0.0.0:8080/api/events body=hello

Note
----

You can run rabbitmq with docker:

    $ docker run --rm --hostname my-rabbit --name some-rabbit -p 5672:5672 -p 15672:15672 rabbitmq:3-management

And open the web interface:

    $ firefox http://0.0.0.0:15672/#/
