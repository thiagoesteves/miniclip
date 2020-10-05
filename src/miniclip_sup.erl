%%%-------------------------------------------------------------------
%%% Created : 03 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%% This is the Miniclip top level supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(miniclip_sup).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(MSG_SERVER_NAME, miniclip).
-define(MSG_PROCESS_SUP_NAME, miniclip_msg_sup).

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 4,
               period => 30},
  %% In this supervisor we have two processes
  %%
  %% 1 - miniclip: server that is consuming messages from AWS SQS
  %% 2 - miniclip_msg_sup: supervisor that is handling all message consumers
  %%
  ChildSpecs = [#{id => ?MSG_PROCESS_SUP_NAME,
                  start => {?MSG_PROCESS_SUP_NAME, start_link, []},
                  restart => permanent,
                  type => supervisor,
                  shutdown => brutal_kill},

                #{id => ?MSG_SERVER_NAME,
                  start => {?MSG_SERVER_NAME, start_link, []},
                  restart => permanent,
                  type => worker,
                  shutdown => brutal_kill}],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

