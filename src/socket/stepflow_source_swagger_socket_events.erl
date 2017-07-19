%%%-------------------------------------------------------------------
%% @doc  public socket (tcp and websocket) API for sending events.
%% @end
%%%-------------------------------------------------------------------

-module(stepflow_source_swagger_socket_events).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([
  post/4
]).

%%%_ * API -------------------------------------------------------------

post(#{<<"event">> := Event}, Req, [], AppCtx) ->
  io:format("POST ~p~n", [Event]),
  % NewAppCtx = AppCtx#{Id => Event},
  {reply, #{}, Req, AppCtx}.
