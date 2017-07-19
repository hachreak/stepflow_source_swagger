%%%-------------------------------------------------------------------
%% @doc stepflow source
%% @end
%%%-------------------------------------------------------------------

-module(stepflow_source_swagger_source).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(gen_server).

-export([
  % append/2,
  sync_append/2
]).

-export([
  start_link/1,
  init/1,
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

-type ctx()   :: stepflow_source:ctx().
-type event() :: stepflow_event:event().

%% API

-spec sync_append(pid(), event()) -> ok | {noproc, any()}.
sync_append(Pid, Event) ->
  gen_server:call(Pid, {append, Event}).

%% Callbacks gen_server

-spec start_link(ctx()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Config) ->
  gen_server:start_link(?MODULE, [Config], []).

-spec init(list(ctx())) -> {ok, ctx()}.
init([Config]) ->
  stepflow_source_swagger_services:start(
    #{port_tcp => 12345, port_http => 8080, http_protocol => http,
      drvctx => #{pid => self()}}),
  {ok, Config#{channels => []}}.

-spec handle_call({setup_channel, pid()}, {pid(), term()}, ctx()) ->
    {reply, ok, ctx()}.
handle_call({setup_channel, ChPid}, _From, #{channels := Channels}=Ctx) ->
  {reply, ok, Ctx#{channels => [ChPid | Channels]}};
handle_call({append, Event}, _From, Ctx) ->
  Ctx2 = append(Event, Ctx),
  {reply, ok, Ctx2};
handle_call(Input, _From, Ctx) ->
  {reply, Input, Ctx}.

-spec handle_cast({append, event()}, ctx()) -> {noreply, ctx()}.
handle_cast(_, Ctx) ->
  {noreply, Ctx}.

handle_info(_Info, Ctx) ->
  {noreply, Ctx}.

terminate(_Reason, _Ctx) ->
  io:format("Terminate!!~n"),
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  io:format("code changed !"),
  {ok, Ctx}.

%% Private functions

append(Event, #{inctxs := InCtxs, channels := ChPids}=Ctx) ->
  Event2 = stepflow_event:new(maps:get(<<"headers">>, Event, #{}),
                              maps:get(<<"body">>, Event, #{})),
  % FIXME return {ok, _}
  InCtxs2 = stepflow_source:append(ChPids, Event2, InCtxs),
  Ctx#{inctxs := InCtxs2}.
