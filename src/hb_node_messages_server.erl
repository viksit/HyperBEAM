-module(hb_node_messages_server).

-behaviour(gen_server).
-include("include/hb.hrl").
-export([start_link/0]).
-export([set/2, get/1]).

-export([init/1, terminate/2, handle_cast/2, handle_info/2, handle_call/3]).
-export([code_change/3]).

-define(TIMEOUT, 5000).

%% @doc Write given Key and Value to
-spec set(ServerId, NodeMessage) -> Result when
    ServerId :: binary(),
    NodeMessage :: map(),
    Result :: ok.
set(ServerId, NodeMessage) ->
    gen_server:call(?MODULE, {set, ServerId, NodeMessage}, ?TIMEOUT).

%% @doc Write given Key and Value to
-spec get(ServerId) -> Result when
    ServerId :: binary(),
    Result :: {ok, NodeMessage} | {error, not_found},
    NodeMessage :: map().
get(ServerId) ->
    gen_server:call(?MODULE, {get, ServerId}, ?TIMEOUT).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call({set, ServerId, NodeMessage}, _From, State) ->
    {reply, ok, maps:put(ServerId, NodeMessage, State)};
handle_call({get, ServerId}, _From, State) ->
    Reply = maps:get(ServerId, State, {error, not_found}),
    {reply, Reply, State}.

handle_cast(Message, State) ->
    ?event(warning, {unhandled_cast, {module, ?MODULE}, {message, Message}}),
    {noreply, State}.

handle_info(Message, State) ->
    ?event(warning, {unhandled_info, {module, ?MODULE}, {message, Message}}),
    {noreply, State}.


terminate(Reason, _State) ->
    ?event(info, {no_messages_terminating, {reason, Reason}}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    