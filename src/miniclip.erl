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

%% Public API export
-export([run/0]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

% DDB2 defines (names, fields, etc)
-define(DDB2_NAME,     <<"TransactionTable">>).
-define(DDB2_F1,       <<"TransactionID">>).
-define(DDB2_F2,       <<"UserID">>).

% DDB2 error defines
-define(DDB2_MSG_ALREADY, <<"Table already exists: ">>).
-define(DDB2_ERROR_ALREADY, <<?DDB2_MSG_ALREADY/binary, ?DDB2_NAME/binary>>).

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

  %% Create the database (or keep the one that already exist)
  DDB2_ERROR_ALREADY = ?DDB2_ERROR_ALREADY,
  case erlcloud_ddb2:create_table( ?DDB2_NAME,
                                   [{?DDB2_F1, s}, {?DDB2_F2, s}],
                                    {?DDB2_F1, ?DDB2_F2}, 5, 5) of
    {ok, _} ->
      io:format("Table created with success\n\r "),
      {ok, []};
    {error,{_, DDB2_ERROR_ALREADY}} ->
      io:format("Table already exists\n\r"),
      {ok, []};
    {error, _} ->
      io:format("Table is not available, tha application won't start \n\r"),
      {stop, data_base_not_available}
  end.

handle_cast(run, State) ->
  case erlcloud_sqs:receive_message(?AWS_SQS_NAME) of
    [{messages, RespList}] -> process_msg(RespList);
    _ -> none
  end,
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

process_msg(RespList) ->
  lists:foreach(fun( [ {body, Body}, _, _, _, _, _ ] ) ->
                  % Decode body message
                  #{?MAP_RECEIPT    := Receipt,
                    ?MAP_USER_ID    := User,
                    ?MAP_POST_QUEUE := PostQueue} =
                  jsone:decode(erlang:list_to_binary(Body)),

                  %% Verify your receipt first with the production URL;
                  %% proceed to verify with the sandbox URL if you receive a
                  %% 21007 status code. Following this approach ensures that
                  %% you do not have to switch between URLs while your
                  %% application is tested, reviewed by App Review, or live
                  %% in the App Store.
                  DecodedReceipt = validate_receipt(?APPLE_PRODUCTION, Receipt),

                  %% Check Status
                  process_receipt(User, PostQueue, DecodedReceipt)
                end,
                RespList).

validate_receipt(Url, Receipt) ->
  %% Prepare message
  Body    = jsone:encode( #{<<"receipt-data">> => Receipt} ),
  Request = {Url, [], ?APPLE_CONTENT_TYPE, Body},

  %% POST validation request and extract only JSON answer
  {ok,{{"HTTP/1.1",200,"OK"},_,InAppResponse}} =
                                          httpc:request(post, Request, [], []),
  %% Decode received message
  DecodedReceipt = jsone:decode(erlang:list_to_binary(InAppResponse)),

  case DecodedReceipt of
      #{<<"status">> := ?STATUS_SANDBOX} ->
          io:format("\n\r Redirecting to Sandbox ~p \n", [DecodedReceipt]),
          validate_receipt(?APPLE_SAND_BOX, Receipt);
      _ ->
          DecodedReceipt
  end.

%% Process the received receipt
process_receipt(User, PostQueue,
                #{<<"status">> := ?STATUS_OK} = DecodedReceipt) ->
  %% Extract the transaction ID
  #{<<"receipt">> :=
        #{ <<"transaction_id">> := TransactionId} } = DecodedReceipt,

  %% Check if the verification has already been done
  Status = case erlcloud_ddb2:q(?DDB2_NAME, {?DDB2_F1, TransactionId, eq}, []) of
    {ok, []}  -> io:format("First time validation for  ~p \n", [TransactionId]),
                 ?OK;
    {ok, [_]} -> io:format("Transaction ~p already validated \n", [TransactionId]),
                 ?ERROR
  end,

  %% Save in the database
  {ok, []} = erlcloud_ddb2:put_item( ?DDB2_NAME,
                             [{?DDB2_F1, TransactionId},
                              {?DDB2_F2, list_to_binary(integer_to_list(User))}]),

  %% Prepare and send message to the post queue
  Data = jsone:encode( #{?MAP_USER_ID  => User,
                         ?MAP_TRANS_ID => TransactionId,
                         ?MAP_STATUS   => Status }, [] ),
  [{message_id,_}, {md5_of_message_body,_}] =
             erlcloud_sqs:send_message(erlang:binary_to_list(PostQueue), Data);

process_receipt(User, _, #{<<"status">> := Status}) ->
  io:format("\n\r Receipt from User: ~p is Invalid ~p \n", [User, Status]).
