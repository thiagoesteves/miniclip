%%%-------------------------------------------------------------------
%%% Created : 03 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(miniclip).

-behaviour(gen_server).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("miniclip.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% gen_server exports
-export([init/1,
         start_link/0,
         terminate/2,
         handle_continue/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% Public API export
-export([run/0]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% DDB2 defines (names, fields, etc)
-define(DDB2_NAME,     <<"TransactionTable">>).
-define(DDB2_F1,       <<"TransactionID">>).
-define(DDB2_F2,       <<"UserID">>).

%% DDB2 error defines
-define(DDB2_MSG_ALREADY, <<"Table already exists: ">>).
-define(DDB2_ERROR_ALREADY, <<?DDB2_MSG_ALREADY/binary, ?DDB2_NAME/binary>>).

%% HTTP defines
-define(MAX_SESSIONS,     200000).
-define(PIPELINE_TIMEOUT, 5000).

%% Supervisor that is handling the messages servers
-define(MSG_SUP, miniclip_msg_sup).

%%%===================================================================
%%% API
%%%===================================================================

run() ->
  gen_server:cast(?MODULE, run).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  %% Enable trapping exits
  process_flag(trap_exit, true),

  %% Initialise environmental variables
  application:set_env(erlcloud, aws_access_key_id, ?AWS_ACCESS_KEY_ID),
  application:set_env(erlcloud, aws_secret_access_key, ?AWS_SECRET_ACCESS_KEY),
  application:set_env(erlcloud, aws_region, ?AWS_REGION),

  %% Config HTTP options
  ok = httpc:set_options([{max_sessions, ?MAX_SESSIONS},
                          {pipeline_timeout, ?PIPELINE_TIMEOUT}], default),

  %% Create the database (or keep the one that already exist)
  DDB2_ERROR_ALREADY = ?DDB2_ERROR_ALREADY,
  case miniclip_erlcloud:ddb2_create_table( ?DDB2_NAME,
                                            [{?DDB2_F1, s}, {?DDB2_F2, s}],
                                            {?DDB2_F1, ?DDB2_F2}, 5, 5) of
    {ok, _} ->
      io:format("Table created with success\n"),
      {ok,[],{continue,receive_aws_sqs_msg}};
    {error,{_, DDB2_ERROR_ALREADY}} ->
      io:format("Table already exists\n"),
      {ok,[],{continue,receive_aws_sqs_msg}};
    {error, _} ->
      io:format("Table is not available, tha server won't start \n"),
      {stop, data_base_not_available}
  end.

handle_continue(receive_aws_sqs_msg, State) ->
  %% Check the number of active message servers because the AWS can't handle
  %% than 50 parallel request
  [_,{active,ActiveMsgServers},_,_] = supervisor:count_children(?MSG_SUP),
  %% Execute the aws request and create the message servers
  ok = request_aws_sqs_message(?AWS_LIMIT_OPERATIONS - ActiveMsgServers),
  {noreply, State, {continue,receive_aws_sqs_msg}}.

handle_cast(run, State) ->
  io:format("I'm still able to receive msgs\n"),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc This function request messages from AWS SQS and start a new gen_server
%%      to handle the validation
%%
%% @param MaxMsgServers Current maximum number of supported messages by this
%%        application and if greater than 10, it assigns the maximum supported
%%        by ercloud that is 10.
%% @end
%%--------------------------------------------------------------------
-spec request_aws_sqs_message(integer()) -> ok.
request_aws_sqs_message(0) ->
  ok;

request_aws_sqs_message(MaxMsgServers)
  when MaxMsgServers > ?AWS_MAX_READ_MSGS ->
  request_aws_sqs_message(?AWS_MAX_READ_MSGS );

request_aws_sqs_message(MaxMsgServers)
  when MaxMsgServers =< ?AWS_MAX_READ_MSGS ->
  %% read messages
  [{messages, RespList}] = erlcloud_sqs:receive_message(?AWS_SQS_NAME,
                                                        all,
                                                        MaxMsgServers),
  %% Check body message is ok, and create servers to handling it
  lists:foreach(
    fun([ {body, Body}, {md5_of_body, RcvMd5}, _, _, _, _ ]) ->
      %% Check message integrity
      RcvMd5 = miniclip_msg:md5sum(Body),
      %% Decode the body message to a map. It is execute before start the
      %% the gen_server that will validate the receipt because in case of
      %% invalid JASON message, it won't affect the others gen_servers that
      %% were already running.
      MapBody = jsone:decode(erlang:list_to_binary(Body)),
      %% Start the gen_server that will process the message
      {ok, _} = miniclip_msg_sup:process_msg(MapBody)
    end,
    RespList),
  ok.
