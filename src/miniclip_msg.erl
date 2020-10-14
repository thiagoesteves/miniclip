%%%-------------------------------------------------------------------
%%% Created : 03 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(miniclip_msg).

-behaviour(gen_server).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("miniclip.hrl").

-dialyzer({nowarn_function, handle_info/2}).

%%%===================================================================
%%% Function exports
%%%===================================================================

%% gen_server exports
-export([init/1,
         start_link/1,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

% These functions are not part of gen_server
-export([md5sum/1, maybe_json_decode/1]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% Timeouts
-define(DDB2_PUT_ITEM_WAIT_TIME, 2).
-define(APPLE_WAIT_TIME,         1000).
-define(DELETE_WAIT_TIME,        100).
-define(POST_QUEUE_WAIT_TIME,    100).

%% Maximum number of retries
-define(MAX_RETRIES, 3).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Body) ->
  gen_server:start_link(?MODULE, [Body], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([State]) ->
  %% Enable trapping exits
  process_flag(trap_exit, true),
  erlang:send(self(), apple_validation),
  %% Initiate the receipt as invalid and maximum number of retries
  {ok, State#{?MAP_VALIDATION => ?ERROR, ?MAP_RETRIES => ?MAX_RETRIES} }.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_info(_, #{retries := 0} = State) ->
  %% Maximum of retries reached, the message won't be processed and it will
  %% return to the queue
  {stop, normal, State};

handle_info(apple_validation, #{?MAP_RECEIPT := ReceiptToValidate,
                                ?MAP_RETRIES := Retries} = State) ->
  %% Validate receipt
  case request_apple_to_validate(?APPLE_PRODUCTION, ReceiptToValidate) of
    {ok, Receipt} ->
      erlang:send(self(), validate_receipt),
      {noreply, State#{?MAP_APPLE_RCPT => Receipt}};
    { error, _ } -> % Try again ...
      erlang:send_after(?APPLE_WAIT_TIME, self(), apple_validation),
      {noreply, State#{?MAP_RETRIES => Retries - 1}}
  end;

handle_info(validate_receipt, #{ ?MAP_APPLE_RCPT :=
                                  #{ <<"status">>  := ?APPLE_STATUS_OK,
                                    <<"receipt">> :=
                                     #{ <<"transaction_id">> := TransactionId}},
                                 ?MAP_RETRIES := Retries} = State) ->
  %% Check if the verification has already been done
  case erlcloud_ddb2:put_item(?DDB2_NAME, [{?DDB2_F1, TransactionId}], [
    { condition_expression, <<"attribute_not_exists(#id)">> },
    { expression_attribute_names, [{<<"#id">>, ?DDB2_F1}] } ]) of
    {ok, []} -> %% Send message to send the results
               erlang:send(self(), send_to_post_queue),
               {noreply, State#{?MAP_VALIDATION => ?OK}};
    {error, {?AWS_DDB2_CHECK_FAIL_MSG,_}} ->
               %% Send message to send the results
               erlang:send(self(), send_to_post_queue),
               {noreply, State};
    {error,{?AWS_DDB2_MAX_THROUGHPUT_MSG,_}} ->
               %% Try to validate again later (maximum throughput achieved)
               erlang:send_after(?DDB2_PUT_ITEM_WAIT_TIME, self(), validate_receipt),
               {noreply, State#{?MAP_RETRIES => Retries - 1}}
  end;

handle_info(validate_receipt, #{?MAP_APPLE_RCPT :=
                               #{ <<"status">>  := ?APPLE_STATUS_ERR_EXPIRED }}
                               = State) ->
  %% Do not Check if the receipt was validated before and keep error state
  erlang:send(self(), send_to_post_queue),
  {noreply, State};

handle_info(validate_receipt, #{?MAP_APPLE_RCPT :=
                               #{ <<"status">>  := ?APPLE_STATUS_ERR_CORRUPTED }}
                               = State) ->
  %% Corrupted message, stop here
  {stop, normal, State};

handle_info(send_to_post_queue, #{?MAP_APPLE_RCPT :=
                                  #{ <<"receipt">> :=
                                     #{ <<"transaction_id">> := TransactionId}},
                                  ?MAP_USER_ID     := User,
                                  ?MAP_POST_QUEUE  := PostQueue,
                                  ?MAP_VALIDATION  := Status,
                                  ?MAP_RETRIES     := Retries} = State) ->
  Data = jsone:encode( #{?MAP_USER_ID  => User,
                         ?MAP_TRANS_ID => TransactionId,
                         ?MAP_STATUS   => Status }, [] ),
  %% Send result message
  Md5 = md5sum(Data),
  case erlcloud_sqs:send_message(erlang:binary_to_list(PostQueue), Data) of
    [{message_id,_}, {md5_of_message_body, Md5}] ->
      %% Message was processed, proceed to delete it
      erlang:send(self(), delete_processed_msg),
      {noreply, State};
    [{message_id,_}, {md5_of_message_body, _}] -> % Invalid md5, try again
      erlang:send_after(?POST_QUEUE_WAIT_TIME, self(), send_to_post_queue),
      {noreply, State#{?MAP_RETRIES => Retries - 1}}
  end;

handle_info(delete_processed_msg, #{?MAP_RCPT_HANDLE := Handle,
                                    ?MAP_RETRIES     := Retries} = State) ->
  %% Try delete the message
  case erlcloud_sqs:delete_message(?AWS_RECEIPT_SQS_NAME, Handle) of
    ok -> % Finish, all OK
          {stop, normal, State};
    _  -> % Error, try again later
          erlang:send_after(?DELETE_WAIT_TIME, self(), delete_processed_msg),
          {noreply, State#{?MAP_RETRIES => Retries - 1}}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(normal, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validate the receipt
%%
%%      From Apple website:
%%      Verify your receipt first with the production URL;
%%      proceed to verify with the sandbox URL if you receive a
%%      21007 status code. Following this approach ensures that
%%      you do not have to switch between URLs while your
%%      application is tested, reviewed by App Review, or live
%%      in the App Store.
%%
%% @param Url Website to be checked against
%% @param Receipt Receipt to be validated
%% @end
%%--------------------------------------------------------------------
-spec request_apple_to_validate(string(), map()) ->
          {ok | error, map() | timeout | json}.
request_apple_to_validate(Url, Receipt) ->
  %% Prepare message
  Body    = jsone:encode( #{<<"receipt-data">> => Receipt} ),
  Request = {Url, [], ?APPLE_CONTENT_TYPE, Body},

  InAppResponse = case httpc:request(post, Request, [], []) of
    {ok, {?HTTP_OK,_,Res} } -> Res;
    %% In case of httpc error, the behaviour will be the same as apple
    %% response for try again
     _ ->
         "{\"status\":21005}"
  end,

  %% Check specific cases for sandbox and retries.
  case maybe_json_decode(InAppResponse) of
    %% Redirect to send box
    { ok, #{<<"status">> := ?APPLE_STATUS_SANDBOX} } ->
      request_apple_to_validate(?APPLE_SAND_BOX, Receipt);
    %% Server error, try again
    { ok, #{<<"status">> := ErrorCode} }
      when ErrorCode =:= ?APPLE_STATUS_ERR_SERVER_1 ;
           ErrorCode =:= ?APPLE_STATUS_ERR_SERVER_2 ->
      { error, timeout };
    Result ->
      Result
  end.

%%--------------------------------------------------------------------
%% @doc Calculate the hex md5 of a string
%%
%% @param String String to be calculated
%% @end
%%--------------------------------------------------------------------
-spec md5sum(jsone:json_value() | string()) -> string().
md5sum(String) ->
    X = erlang:md5(String),
    [begin if N < 10 -> 48 + N; true -> 87 + N end end || <<N:4>> <= X].

%%--------------------------------------------------------------------
%% @doc Try to decode json string
%%
%% @param String String to be decoded
%% @end
%%--------------------------------------------------------------------
-spec maybe_json_decode(string()) -> {ok | error, map() | json}.
maybe_json_decode(Str) ->
  try jsone:decode(erlang:list_to_binary(Str)) of
    DecodedStr -> {ok, DecodedStr}
  catch
    _:_ ->
        {error, json}
  end.
