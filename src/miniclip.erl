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
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% HTTP defines
-define(MAX_SESSIONS,     200000).
-define(PIPELINE_TIMEOUT, 5000).

%% Supervisor that is handling the messages servers
-define(MSG_SUP, miniclip_msg_sup).

%% SLEEP for invalid read message server
-define(INV_SERVER_SLEEP, 1000).

%%%===================================================================
%%% API
%%%===================================================================

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

  %% Send message to itself to start the process of receiving messages
  %% from AWS SQS
  erlang:send(?MODULE, receive_aws_sqs_msg),
  {ok,#{msg_counter => 0}}.

handle_info(receive_aws_sqs_msg, State = #{msg_counter := MsgCounter}) ->
  %% Check the number of active message servers because the AWS can't handle
  %% than 50 parallel request
  [_,{active,Active},_,_] = supervisor:count_children(?MSG_SUP),
  %% Execute the aws request and create the message servers
  RcvMsg = request_aws_sqs_message(?AWS_DDB2_LIMIT_OPERATIONS - Active),
  %% Restart the cycle
  erlang:send(?MODULE, receive_aws_sqs_msg),
  {noreply, State#{msg_counter => MsgCounter + RcvMsg}};

handle_info(_Info, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

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
%%        by aws sqs that is 10.
%% @end
%%--------------------------------------------------------------------
-spec request_aws_sqs_message(integer()) -> integer().
request_aws_sqs_message(0) ->
  0;

request_aws_sqs_message(MaxMsgServers)
  when MaxMsgServers > ?AWS_SQS_MAX_READ_MSGS ->
  request_aws_sqs_message(?AWS_SQS_MAX_READ_MSGS );

request_aws_sqs_message(MaxMsgServers)
  when MaxMsgServers =< ?AWS_SQS_MAX_READ_MSGS ->
  %% read messages (If no server is available, an exception is raised)
  MessageList = try erlcloud_sqs:receive_message(?AWS_RECEIPT_SQS_NAME,
                                                 all,
                                                 MaxMsgServers) of
    [{messages, List}] -> List
  catch
    _:?AWS_SQS_INVALID_SERVER ->
        io:format("The AWQ server is not accessible"),
        timer:sleep(?INV_SERVER_SLEEP),
        %% return no messages available
        []
  end,

  %% Check body message is ok, and create servers to handling it
  lists:foreach(
    fun([ {body, Body}, {md5_of_body, RcvMd5}, _,
          {receipt_handle,HandleMsg}, _, _ ]) ->
      %% Check message integrity
      case miniclip_msg:md5sum(Body) of
        RcvMd5 ->
          %% Decode the body message to a map. It is execute before start the
          %% the gen_server that will validate the receipt because in case of
          %% invalid JASON message, it won't affect the others gen_servers that
          %% were already running.
          create_miniclip_msg_server(miniclip_msg:maybe_json_decode(Body),
                                     HandleMsg);
        _ -> % Corrputed message, discard
            none
      end
    end,
    MessageList),
  length(MessageList).

%%--------------------------------------------------------------------
%% @doc This function create the servers that will process the msg
%%      If the json is invalid, no messages are create
%%
%% @param Json return
%%        Message Handle to be added to the json decoded map
%% @end
%%--------------------------------------------------------------------
-spec create_miniclip_msg_server( {ok, map()} | {error, any()}, string()) -> ok.
create_miniclip_msg_server({error, _}, _) ->
  %% Invalid json, discard message
  ok;

create_miniclip_msg_server({ok, MapBody}, Handle) ->
  {ok, _} = miniclip_msg_sup:process_msg(MapBody#{?MAP_RCPT_HANDLE => Handle}),
  ok.



