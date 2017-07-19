%%%-------------------------------------------------------------------
%% @doc Manage services.
%% @end
%%%-------------------------------------------------------------------

-module(stepflow_source_swagger_services).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([start/1]).

-type appctx() :: any().

%%====================================================================
%% API
%%====================================================================

start(Ctx) ->
  Filename = swagger_filename(),
  Yaml = swagger_routerl:load(Filename),
  {ok, SwaggerFileRaw} = file:read_file(Filename),

  http(Yaml, SwaggerFileRaw, Ctx),
  tcp(Yaml, Ctx).

%%====================================================================
%% Internal functions
%%====================================================================

tcp(Yaml, #{port_tcp := Port, drvctx := DrvCtx}) ->
  {Name, Port, Handler, AppCtx} = swagger_routerl_tcp:compile(
    "stepflow_source_swagger_socket_", Yaml, DrvCtx, #{
      name => "stepflow_source_swagger_service", port => Port
    }),

  {ok, _} = ranch:start_listener(
    Name, 100, ranch_tcp, [{port, Port}], Handler, AppCtx).

http(Yaml, SwaggerFileRaw, #{port_http := Port}=Ctx) ->
  Dispatch = cowboy_router:compile([
    {'_', routes(Ctx, Yaml, SwaggerFileRaw)}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
    {env, [{dispatch, Dispatch}]}
  ]).

-spec routes(appctx(), list(), binary()) -> cowboy_router:routes().
routes(#{http_protocol := Protocol, drvctx := DrvCtx}, Yaml, SwaggerFileRaw) ->
  FileEndpoint = swagger_routerl_cowboy_rest:file_endpoint(
    SwaggerFileRaw, #{endpoint => endpoint(Yaml),
    protocol => swagger_routerl_utils:to_binary(Protocol)}),
  RestEndpoints = swagger_routerl_cowboy_rest:compile(
    "stepflow_source_swagger_rest_", Yaml, DrvCtx),
  WSEndpoint = swagger_routerl_cowboy_ws:compile(
    "stepflow_source_swagger_socket_", Yaml, DrvCtx, #{
    handler => swagger_routerl_cowboy_v1_ws_json_dispatcher
  }),

  FileEndpoint ++ RestEndpoints ++ WSEndpoint.

swagger_filename() ->
  PrivDir = code:priv_dir(stepflow_source_swagger),
  Filename = "swagger.yaml",
  filename:join([PrivDir, Filename]).

endpoint(Yaml) ->
  Version = swagger_routerl:get_version(Yaml),
  "/" ++ Version ++ "/docs/swagger.yaml".
