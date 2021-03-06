%%%-------------------------------------------------------------------
%% @doc REST handler for events
%% @end
%%%-------------------------------------------------------------------

-module(stepflow_source_swagger_rest_events).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  from_json/2
]).

-export([
    allow_missing_post/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    init/3,
    rest_init/2
]).

%%%_ * API -------------------------------------------------------------

init(_Transport, _Req, _AppCtx) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, AppCtx) ->
  {[<<"POST">>], Req, AppCtx}.

rest_init(Req, AppCtx) ->
  {ok, Req, AppCtx}.

content_types_accepted(Req, AppCtx) ->
  {[{<<"application/json">>, from_json}], Req, AppCtx}.

content_types_provided(Req, AppCtx) ->
  {[{<<"application/json">>, to_json}], Req, AppCtx}.

allow_missing_post(Req, AppCtx) ->
  {true, Req, AppCtx}.

from_json(Req, #{pid := PidS}=AppCtx) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  Events = jsx:decode(Body, [return_maps]),
  stepflow_source_swagger_source:sync_append(PidS, Events),
  {true, Req2, AppCtx}.
