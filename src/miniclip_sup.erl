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
-define(MINICLIP_NAME, miniclip).

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 4,
                 period => 30},
    %% Miniclip is linked at the initialisation
    ChildSpecs = [#{id => ?MINICLIP_NAME,
                    start => {?MINICLIP_NAME, start_link, []},
                    shutdown => brutal_kill}],
    % comment this line to stop trapping exits
    process_flag(trap_exit, true),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

