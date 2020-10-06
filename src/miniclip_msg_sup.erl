%%%-------------------------------------------------------------------
%%% Created : 04 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This is the supervisor that will supervise the message validation
%%%      gen_Server
%%% @end
%%%-------------------------------------------------------------------

-module(miniclip_msg_sup).

-author('Thiago Esteves').

%%====================================================================
%% API functions
%%====================================================================

-export([start_link/0]).

-export([init/1, process_msg/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

-define(SERVER,           ?MODULE).
-define(MSG_CONSUMER_NAME, miniclip_msg).

%%====================================================================
%% API functions implementation
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 30},
    %% The child won't start automatically
    ChildSpecs = #{ id => ?MSG_CONSUMER_NAME,
                    start => {?MSG_CONSUMER_NAME, start_link, []},
                    restart => transient,
                    shutdown => brutal_kill,
                    type => worker },
    {ok, {SupFlags, [ChildSpecs]}}.

%%--------------------------------------------------------------------
%% @doc This function creates a supervised gen_server to validate
%%      the received message and in case of any failure, the supervisor
%%      will recriate a newone with the same message
%%
%% @param Body Received receit in the string format
%% @end
%%--------------------------------------------------------------------
-spec process_msg(map()) -> { ok , undefined | pid() }.
process_msg(Body) ->
  {ok, Pid} = supervisor:start_child(?MODULE, [Body]),
  {ok, Pid}.

%%====================================================================
%% Internal functions
%%====================================================================

