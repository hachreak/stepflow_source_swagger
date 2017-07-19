%%%-------------------------------------------------------------------
%% @doc public API
%% @end
%%%-------------------------------------------------------------------

-module(stepflow_source_swagger_app).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  stepflow_source_swagger_sup:start_link().

stop(_State) ->
  ok.
