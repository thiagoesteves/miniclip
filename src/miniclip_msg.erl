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

-dialyzer({nowarn_function, process_receipt/3}).

%%%===================================================================
%%% Function exports
%%%===================================================================

%% gen_server exports
-export([init/1,
         start_link/1,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         handle_call/3,
         code_change/3]).

% This function is not part of gen_server
-export([md5sum/1]).

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

%% Http defines
-define(HTTP_OK, {"HTTP/1.1",200,"OK"}).

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
  {ok, State, {continue, handle_message}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

handle_continue(handle_message, #{?MAP_RECEIPT      := ReceiptToValidate,
                                  ?MAP_USER_ID      := User,
                                  ?MAP_POST_QUEUE   := PostQueue } = State) ->
  %% Validate receipt
  Receipt = validate_sync(?APPLE_PRODUCTION, ReceiptToValidate),
  %% Process received receipt
  ok = process_receipt(User, PostQueue, Receipt),

  %% Return and terminate the gen_server
  {stop, normal, State}.

handle_info(Info, State) ->
  io:format("Invalid Handle info: ~p\n", [Info]),
  {noreply, State}.

%% @private
terminate(normal, _State) ->
  ok;

terminate(Reason, _State) ->
  io:format("The MSG may not be processed because ~p \n", [Reason]),
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
-spec validate_sync(string(), map()) -> jsone:decode_result().
validate_sync(Url, Receipt) ->
  %% Prepare message
  Body    = jsone:encode( #{<<"receipt-data">> => Receipt} ),
  Request = {Url, [], ?APPLE_CONTENT_TYPE, Body},

  {ok, {?HTTP_OK,_,InAppResponse} } = httpc:request(post, Request, [], []),

  %% Check specific cases for sandbox and retries.
  case jsone:decode(erlang:list_to_binary(InAppResponse)) of
    %% Redirect to send box
    #{<<"status">> := ?STATUS_SANDBOX} ->
      validate_sync(?APPLE_SAND_BOX, Receipt);
    %% Server error, try again
    #{<<"status">> := ErrorCode}
      when ErrorCode =:= ?STATUS_ERR_SERVER_1 ;
           ErrorCode =:= ?STATUS_ERR_SERVER_2 ->
      validate_sync(Url, Receipt);
    Result ->
      Result
  end.

%%--------------------------------------------------------------------
%% @doc Process the received receipt
%%
%% @param User User ID
%% @param PostQueue AWB SQS queue to receive the varification status
%% @param DecodedReceipt Received receipt in map format
%% @end
%%--------------------------------------------------------------------
-spec process_receipt(integer(), binary(), map()) -> ok.
process_receipt(User, PostQueue,
                #{ <<"status">>  := ?STATUS_OK,
                   <<"receipt">> :=
                     #{ <<"transaction_id">> := TransactionId}}) ->
  %% Check if the verification has already been done
  Status = case miniclip_erlcloud:ddb2_q(?DDB2_NAME,
                                         {?DDB2_F1, TransactionId, eq}, []) of
    {ok, []}  -> %io:format("First time validation for  ~p \n", [TransactionId]),
                 ?OK;
    {ok, _} -> %io:format("Transaction ~p already validated \n", [TransactionId]),
                 ?ERROR
  end,

  %% Save in the database
  {ok, []} = miniclip_erlcloud:ddb2_put_item(
               ?DDB2_NAME,
               [{?DDB2_F1, TransactionId},
                {?DDB2_F2, list_to_binary(integer_to_list(User))}]),

  %% Prepare and send message to the post queue and check returned md5
  Data = jsone:encode( #{?MAP_USER_ID  => User,
                         ?MAP_TRANS_ID => TransactionId,
                         ?MAP_STATUS   => Status }, [] ),
  Md5 = md5sum(Data),
  [{message_id,_}, {md5_of_message_body, Md5}] =
             erlcloud_sqs:send_message(erlang:binary_to_list(PostQueue), Data),
  ok;

process_receipt(_,_,_) ->
  ok.

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
