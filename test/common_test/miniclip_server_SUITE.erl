%%%-------------------------------------------------------------------
%%% Created : 08 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc Test suite file
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(miniclip_server_SUITE).

%%%===================================================================
%%% Includes
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("miniclip.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%%%===================================================================
%%% local Defines
%%%===================================================================

-define(AWS_SQS_BODY_MSG, "{\"post_queue\":\"ResultsSQS\",\"receipt\":\"ewoJInNpZ25hdHVyZSIgPSAiQWh0UkxxU0dlWHF3SEVNUmZkTk5hSWZBS2NSOHpSZ2tsd2ZpWkhLZWEvbkVJQVR5dE8xbWhDOWkyVVlQdmQ3WnVQR1B2Y3JMdnd2aVl0Nk"++
  "rU1FLeTJqcmRtVFhUU2txUnZGOXFla1VzenRoemlBQ0EyZnRjQUhQUGQ4QTI4MFBmdzEydEdUenNzaHFPZUNqYkRIejlJbERydU5yWGRldVZZUXVVMU9HNDdFc0FBQURWekNDQTFNd2dnSTdvQU1DQVFJQ0NHVVVrVTNaV0FTMU1BMEdDU3FHU0liM0RRRUJC"++
  "UVVBTUg4eEN6QUpCZ05WQkFZVEFsVlRNUk13RVFZRFZRUUtEQXBCY0hCc1pTQkpibU11TVNZd0pBWURWUVFMREIxQmNIQnNaU0JEWlhKMGFXWnBZMkYwYVc5dUlFRjFkR2h2Y21sMGVURXpNREVHQTFVRUF3d3FRWEJ3YkdVZ2FWUjFibVZ6SUZOMGIzSmxJRU"++
  "5sY25ScFptbGpZWFJwYjI0Z1FYVjBhRzl5YVhSNU1CNFhEVEE1TURZeE5USXlNRFUxTmxvWERURTBNRFl4TkRJeU1EVTFObG93WkRFak1DRUdBMVVFQXd3YVVIVnlZMmhoYzJWU1pXTmxhWEIwUTJWeWRHbG1hV05oZEdVeEd6QVpCZ05WQkFzTUVrRndjR3hs"++
  "SUdsVWRXNWxjeUJUZEc5eVpURVRNQkVHQTFVRUNnd0tRWEJ3YkdVZ1NXNWpMakVMTUFrR0ExVUVCaE1DVlZNd2daOHdEUVlKS29aSWh2Y05BUUVCQlFBRGdZMEFNSUdKQW9HQkFNclJqRjJjdDRJclNkaVRDaGFJMGc4cHd2L2NtSHM4cC9Sd1YvcnQvO"++
  "TFYS1ZoTmw0WElCaW1LalFRTmZnSHNEczZ5anUrK0RyS0pFN3VLc3BoTWRkS1lmRkU1ckdYc0FkQkVqQndSSXhleFRldngzSExFRkdBdDFtb0t4NTA5ZGh4dGlJZERnSnYyWWFWczQ5QjB1SnZOZHk2U01xTk5MSHNETHpEUzlvWkhBZ01CQUFHamNqQn"++
  "dNQXdHQTFVZEV3RUIvd1FDTUFBd0h3WURWUjBqQkJnd0ZvQVVOaDNvNHAyQzBnRVl0VEpyRHRkREM1RllRem93RGdZRFZSMFBBUUgvQkFRREFnZUFNQjBHQTFVZERnUVdCQlNwZzRQeUdVakZQaEpYQ0JUTXphTittVjhrOVRBUUJnb3Foa2lHOTJOa0JnVUJC"++
  "QUlGQURBTkJna3Foa2lHOXcwQkFRVUZBQU9DQVFFQUVhU2JQanRtTjRDL0lCM1FFcEszMlJ4YWNDRFhkVlhBZVZSZVM1RmFaeGMrdDg4cFFQOTNCaUF4dmRXLzNlVFNNR1k1RmJlQVlMM2V0cVA1Z204d3JGb2pYMGlreVZSU3RRKy9BUTBLRWp0cUIwN2tMcz"++
  "lRVWU4Y3pSOFVHZmRNMUV1bVYvVWd2RGQ0TndOWXhMUU1nNFdUUWZna1FRVnk4R1had1ZIZ2JFL1VDNlk3MDUzcEdYQms1MU5QTTN3b3hoZDNnU1JMdlhqK2xvSHNTdGNURXFlOXBCRHBtRzUrc2s0dHcrR0szR01lRU41LytlMVFUOW5wL0tsMW5qK2FCdzdD"++
  "MHhzeTBiRm5hQWQxY1NTNnhkb3J5L0NVdk02Z3RLc21uT09kcVRlc2JwMGJzOHNuNldxczBDOWRnY3hSSHVPTVoydG04bnBMVW03YXJnT1N6UT09IjsKCSJwdXJjaGFzZS1pbmZvIiA9ICJld29KSW05eWFXZHBibUZzTFhCMWNtTm9ZWE5sTFdSaGRHVXRjSE"++
  "4wSWlBOUlDSXlNREV5TFRBMExURTRJREExT2pBM09qRXpJRUZ0WlhKcFkyRXZURzl6WDBGdVoyVnNaWE1pT3dvSkltaHZjM1JsWkMxcFlYQXRkbVZ5YzJsdmJpSWdQU0FpTVM0d0xqRWlPd29KSW05eWFXZHBibUZzTFhSeVlXNXpZV04wYVc5dUxXbGt"++
  "JaUE5SUNJeE1EQXdNREF3TURReU9USTROVFkzSWpzS0NTSmlkbkp6SWlBOUlDSXhMakV1TVNJN0Nna2lkSEpoYm5OaFkzUnBiMjR0YVdRaUlEMGdJakV3TURBd01EQXdOREk1TWpnMU5qY2lPd29KSW5GMVlXNTBhWFI1SWlBOUlDSXhJanNLQ1NKdmNtbG5hV"++
  "zVoYkMxd2RYSmphR0Z6WlMxa1lYUmxMVzF6SWlBOUlDSXhNek0wTnpVd09ETXpNREF3SWpzS0NTSndjbTlrZFdOMExXbGtJaUE5SUNKamIyMHViV2x1YVdOc2FYQXVZVzVwYldGc2MyaGxiSFJsY2k1amRYSnlaVzVqZVZCaFkyc3hJanNLQ1NKcGRHVn"++
  "RMV2xrSWlBOUlDSTBOekF5TVRneE9EZ2lPd29KSW1KcFpDSWdQU0FpWTI5dExtMXBibWxqYkdsd0xtRnVhVzFoYkhOb1pXeDBaWElpT3dvSkluQjFjbU5vWVhObExXUmhkR1V0YlhNaUlEMGdJakV6TXpRM05UQTRNek13TURBaU93b0pJbkIxY21Ob1lYTmxM"++
  "V1JoZEdVaUlEMGdJakl3TVRJdE1EUXRNVGdnTVRJNk1EYzZNVE1nUlhSakwwZE5WQ0k3Q2draWNIVnlZMmhoYzJVdFpHRjBaUzF3YzNRaUlEMGdJakl3TVRJdE1EUXRNVGdnTURVNk1EYzZNVE1nUVcxbGNtbGpZUzlNYjNOZlFXNW5aV3hsY3lJN0Nna2liM0"++
  "pwWjJsdVlXd3RjSFZ5WTJoaGMyVXRaR0YwWlNJZ1BTQWlNakF4TWkwd05DMHhPQ0F4TWpvd056b3hNeUJGZEdNdlIwMVVJanNLZlE9PSI7CgkiZW52aXJvbm1lbnQiID0gIlNhbmRib3giOwoJInBvZCIgPSAi MTAwIjsKCSJzaWduaW5nLXN0YXR1cyIgPSA"++
  "iMCI7Cn0=\",\"user_id\":4505679655}").
-define(AWS_SQS_BODY_MSG_MD5, "4cd4f852412b85d2a7ab96782d83b54d").
-define(AWS_MAP_BODY_ENCODE, jsone:decode(erlang:list_to_binary(?AWS_SQS_BODY_MSG)) ).

-define(AWS_SQS_BODY_MSG_INVALID,     "{\"post_queue4505679655}").
-define(AWS_SQS_BODY_MSG_INVALID_MD5, "19e74c891f1c2acaedac79be8a3b21fd").

-define(IN_APP_RESP_MSG_OK,
  "{\"receipt\":{\"original_purchase_date_pst\":\"2012-04-18 05:07:13 America/Los_Angeles\", \"purchase_date_ms\":\"1334750833000\", \"original_transaction_id\":\"1000000042928567\", \"original_purchase_date_ms\":\"1334750833000\", \"transaction_id\":\"1000000042928567\", \"quantity\":\"1\", \"bvrs\":\"1.1.1\", \"hosted_iap_version\":\"1.0.1\", \"product_id\":\"com.miniclip.animalshelter.currencyPack1\", \"purchase_date\":\"2012-04-18 12:07:13 Etc/GMT\", \"original_purchase_date\":\"2012-04-18 12:07:13 Etc/GMT\", \"purchase_date_pst\":\"2012-04-18 05:07:13 America/Los_Angeles\", \"bid\":\"com.miniclip.animalshelter\", \"item_id\":\"470218188\"}, \"status\":0}").

-define(IN_APP_RESP_MSG_EXPIRED,
  "{\"receipt\":{\"original_purchase_date_pst\":\"2012-04-18 05:07:13 America/Los_Angeles\", \"purchase_date_ms\":\"1334750833000\", \"original_transaction_id\":\"1000000042928567\", \"original_purchase_date_ms\":\"1334750833000\", \"transaction_id\":\"1000000042928567\", \"quantity\":\"1\", \"bvrs\":\"1.1.1\", \"hosted_iap_version\":\"1.0.1\", \"product_id\":\"com.miniclip.animalshelter.currencyPack1\", \"purchase_date\":\"2012-04-18 12:07:13 Etc/GMT\", \"original_purchase_date\":\"2012-04-18 12:07:13 Etc/GMT\", \"purchase_date_pst\":\"2012-04-18 05:07:13 America/Los_Angeles\", \"bid\":\"com.miniclip.animalshelter\", \"item_id\":\"470218188\"},\"status\":21006}").

-define(IN_APP_RESP_MSG_ERR(CODE),
  "{\"status\":" ++ erlang:integer_to_list(CODE) ++ "}").

-define(POST_QUEUE_MSG_OK,
  <<"{\"status\":\"OK\",\"transaction_id\":\"1000000042928567\",\"user_id\":4505679655}">>).

-define(POST_QUEUE_MSG_INVALID,
  <<"{\"status\":\"INVALID\",\"transaction_id\":\"1000000042928567\",\"user_id\":4505679655}">>).

-define(POST_QUEUE_MSG_OK_MD5, "e72aa868fb11560208198605ad883029").

-define(POST_QUEUE_MSG_INVALID_MD5, "505971169db90e00e13cffdd94905ec6").

-define(RECEIPT_SQS_HANDLE, "505971169db").

%% Compose templates
-define(MSG_TEMPLATE(B, M, H), [ [{body, B}, {md5_of_body, M}, 0,
                                  {receipt_handle,H},0,0] ]).

%% Timeout between read AWB SQS message and post the validation
-define(VALIDATION_TIMEOUT, 5000).

%% Definition for Default number of messages to test
-define(DEFAULT_MSGS_TO_TEST, 1000).

%% Default number of tries
-define(MAX_NUM_TRIES, 3).

%%%===================================================================
%%% Test exports
%%%===================================================================
-export([miniclip_start_stop_ok/1,
         miniclip_receive_and_validate_msgs_ok/1,
         miniclip_receive_invalid_msgs_aws_sqs_error/1,
         miniclip_receive_invalid_json_msgs_error/1,
         miniclip_receive_invalid_server_aws_sqs_ok/1,
         miniclip_receive_invalid_server_aws_sqs_error/1,
         miniclip_msg_process_msg_ok/1,
         miniclip_msg_process_msg_redirect_sandbox_ok/1,
         miniclip_msg_process_msg_expired_ok/1,
         miniclip_msg_process_msg_server_err_ok/1,
         miniclip_msg_process_msg_server_err_error/1,
         miniclip_msg_process_msg_already_validated_ok/1,
         miniclip_msg_process_msg_max_parallel_dynamodb_request_ok/1,
         miniclip_msg_process_msg_send_results_invalid_md5_ok/1,
         miniclip_msg_process_msg_delete_message_invalid_ok/1,
         miniclip_msg_process_msg_http_invalid_ok/1,
         miniclip_msg_process_msg_send_apple_timeout_ok/1,
         miniclip_msg_process_msg_send_dynamoDB_timeout_ok/1,
         miniclip_msg_process_msg_send_post_queue_timeout_ok/1,
         miniclip_msg_process_msg_delete_timeout_ok/1]).

all() -> [miniclip_start_stop_ok,
          miniclip_receive_and_validate_msgs_ok,
          miniclip_receive_invalid_msgs_aws_sqs_error,
          miniclip_receive_invalid_json_msgs_error,
          miniclip_receive_invalid_server_aws_sqs_ok,
          miniclip_receive_invalid_server_aws_sqs_error,
          miniclip_msg_process_msg_ok,
          miniclip_msg_process_msg_redirect_sandbox_ok,
          miniclip_msg_process_msg_expired_ok,
          miniclip_msg_process_msg_server_err_ok,
          miniclip_msg_process_msg_server_err_error,
          miniclip_msg_process_msg_already_validated_ok,
          miniclip_msg_process_msg_max_parallel_dynamodb_request_ok,
          miniclip_msg_process_msg_send_results_invalid_md5_ok,
          miniclip_msg_process_msg_delete_message_invalid_ok,
          miniclip_msg_process_msg_http_invalid_ok,
          miniclip_msg_process_msg_send_apple_timeout_ok,
          miniclip_msg_process_msg_send_dynamoDB_timeout_ok,
          miniclip_msg_process_msg_send_post_queue_timeout_ok,
          miniclip_msg_process_msg_delete_timeout_ok
         ].

%%%===================================================================
%%% init_per_suite:  Contains common initializations for all test
%%%                  cases in the suite
%%%===================================================================
init_per_suite(Config) ->
  Config.

%%%===================================================================
%%% end_per_suite: It is called as the final stage of the test suite
%%%                execution
%%%===================================================================
end_per_suite(_Config) ->
 ok.

%%%===================================================================
%%% init_per_testcase: It is called before each test case in the suite.
%%%                    Contains initialization that must be done for
%%%                    each test case.
%%%
%%% @param Name    Name of the test case
%%% @param Config  Config key-value list of runtime configuration data,
%%%                which has the same value as the list returned by
%%%                init_per_suite
%%%===================================================================
init_per_testcase(_, Config) ->
  %% Configure default mecks
  meck:expect(httpc, set_options, fun(_,_) -> ok end),
  meck:expect(erlcloud_ddb2, put_item, fun(_,_,_) -> {ok, []} end),
  meck:expect(erlcloud_sqs, receive_message,
              fun(?AWS_RECEIPT_SQS_NAME,all,_) -> [{messages, [] }] end),
  meck:expect(erlcloud_sqs, send_message,
    fun(?AWS_DEBUG_POST_QUEUE,_) ->
           [{message_id,0}, {md5_of_message_body, ?POST_QUEUE_MSG_OK_MD5}]
    end),
  meck:expect(erlcloud_sqs, delete_message,
    fun(?AWS_RECEIPT_SQS_NAME,_) ->
           ok
    end),
  meck:expect(httpc, request,
    fun(post,_,[], []) ->
           {ok, {?HTTP_OK,0,?IN_APP_RESP_MSG_OK} }
    end),

  %% Reset expectations
  meck:reset(httpc),
  meck:reset(erlcloud_ddb2),
  meck:reset(erlcloud_sqs),
  Config.

%%%===================================================================
%%% end_per_testcase: It is called after each test case has finished,
%%%                   enabling cleanup after init_per_testcase
%%%===================================================================
end_per_testcase(_, _Config) ->
  meck:unload(),
  ok.

%%%===================================================================
%%%           Test case functions: Waiting for OK result
%%%===================================================================

%%%===================================================================
%%% Function: miniclip_start_stop_ok
%%%
%%% Description: Test the start and stop of the application
%%%===================================================================
miniclip_start_stop_ok(_Config) ->
  %% Start the Server
  application:ensure_all_started(miniclip),

  %% Check the server is still running
  ?assertNotEqual( undefined, try_get_state(miniclip) ),

  %% Stop Server
  application:stop(miniclip),

  %% Check the server is still running
  ?assertMatch( undefined, try_get_state(miniclip) ),

  ok.

%%%===================================================================
%%% Function: miniclip_receive_and_validate_msgs_ok
%%%
%%% Description: This test will configure the AWS SQS to return
%%% a maximum of N valid messages and all must be sent
%%% to the Post Queue
%%%===================================================================
miniclip_receive_and_validate_msgs_ok(_Config) ->
  %% mock one valid message from AWS SQS
  meck:expect(erlcloud_sqs, receive_message,
    fun(?AWS_RECEIPT_SQS_NAME,all,_) -> aws_sqs_available(?DEFAULT_MSGS_TO_TEST,
                                                  ?AWS_SQS_BODY_MSG,
                                                  ?AWS_SQS_BODY_MSG_MD5) end),
  %% Start the Server
  miniclip_sup:start_link(),

  %% Wait all messages being processed
  ?assertMatch( {ok, _}, wait_delete_message(?DEFAULT_MSGS_TO_TEST,
                                             ?VALIDATION_TIMEOUT) ),

  %% Check ALl messages are valid and deleted
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs, send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  ok.

%%%===================================================================
%%% Function: miniclip_receive_invalid_msgs_aws_sqs_error
%%%
%%% Description: This feeds the server with md5 invalid messages, no
%%% messages will be sent to the post queue
%%%===================================================================
miniclip_receive_invalid_msgs_aws_sqs_error(_Config) ->
  %% mock one valid message from AWS SQS
  meck:expect(erlcloud_sqs, receive_message,
    fun(?AWS_RECEIPT_SQS_NAME,all,_) ->
            aws_sqs_available(?DEFAULT_MSGS_TO_TEST,
                              ?AWS_SQS_BODY_MSG,
                              "deadbeef")
    end),

  %% Start the Server
  miniclip_sup:start_link(),

  %% Wait all messages being processed
  ?assertMatch( {ok, _}, wait_read_server_to_receive(?DEFAULT_MSGS_TO_TEST,
                                                     ?VALIDATION_TIMEOUT) ),

  % wait a time to guarantee no miniclip_msg server were create
  timer:sleep(100),

  %% Check no message were processed
  ?assertEqual( 0, meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_ddb2, put_item, ['_', '_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, send_message, ['_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, delete_message, ['_', '_'])),
  ok.

%%%===================================================================
%%% Function: miniclip_receive_invalid_json_msgs_error
%%%
%%% Description: This feeds the server with invalid json messages.
%%%              The miniclip server must not create any message server
%%%===================================================================
miniclip_receive_invalid_json_msgs_error(_Config) ->
  %% mock one valid message from AWS SQS
  meck:expect(erlcloud_sqs, receive_message,
    fun(?AWS_RECEIPT_SQS_NAME,all,_) ->
            aws_sqs_available(?DEFAULT_MSGS_TO_TEST,
                              ?AWS_SQS_BODY_MSG_INVALID,
                              ?AWS_SQS_BODY_MSG_INVALID_MD5)
    end),

  %% Start the Server
  miniclip_sup:start_link(),

  %% Wait all messages being processed
  ?assertMatch( {ok, _}, wait_read_server_to_receive(?DEFAULT_MSGS_TO_TEST,
                                                     ?VALIDATION_TIMEOUT) ),

  % wait a time to guarantee no miniclip_msg server were create
  timer:sleep(100),

  %% Check no message were processed
  ?assertEqual( 0, meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_ddb2, put_item, ['_', '_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, send_message, ['_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, delete_message, ['_', '_'])),
  ok.

%%%===================================================================
%%% Function: miniclip_receive_invalid_server_aws_sqs_ok
%%%
%%% Description: This function is going to test when the aws sqs
%%% server has an invalid name and a known exception, the server must
%%% not crash.
%%%===================================================================
miniclip_receive_invalid_server_aws_sqs_ok(_Config) ->
  %% mock one valid message from AWS SQS
  meck:expect(erlcloud_sqs, receive_message,
    fun(?AWS_RECEIPT_SQS_NAME,all,_) -> throw (
                                  {aws_error,{http_error,400,"Bad Request",0}})
    end),

  %% Start the Server
  miniclip_sup:start_link(),

  % wait a time to guarantee no miniclip_msg server were create
  timer:sleep(100),

  %% Check the server is still running
  ?assertNotEqual( undefined, try_get_state(miniclip) ),

  %% Check no message were processed
  ?assertEqual( 0, meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_ddb2, put_item, ['_', '_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, send_message, ['_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, delete_message, ['_', '_'])),
  ok.

%%%===================================================================
%%% Function: miniclip_receive_invalid_server_aws_sqs_error
%%%
%%% Description: This function is going to test when the aws sqs
%%% server has return an invalid exception, in this situation, the miniclip
%%% server crashes.
%%%===================================================================
miniclip_receive_invalid_server_aws_sqs_error(_Config) ->
  %% Prepare Invalid exception
  meck:expect(erlcloud_sqs, receive_message,
    fun(?AWS_RECEIPT_SQS_NAME,all,_) -> throw ( kill ) end),

  %% Start the Server (it is spawned to avoid the test to finish by exit signal)
  spawn(fun() -> miniclip_sup:start_link() end),

  % wait a time to guarantee no miniclip_msg server were create
  timer:sleep(100),

  %% Check the server is down
  ?assertEqual( undefined, whereis(miniclip) ),

  %% Check no message were processed
  ?assertEqual( 0, meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_ddb2, put_item, ['_', '_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, send_message, ['_', '_']) ),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, delete_message, ['_', '_'])),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_ok
%%%
%%% Description: This function is going to test if the msg was processed
%%%              correctly
%%%===================================================================
miniclip_msg_process_msg_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being processed
  ?assertMatch( {ok, _}, wait_delete_message(?DEFAULT_MSGS_TO_TEST,
                                             ?VALIDATION_TIMEOUT) ),

  %% Check ALl messages are valid and deleted
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs, send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_redirect_sandbox_ok
%%%
%%% Description: This function is going to test if the receipt was created
%%%              for sandbox and sent for prodution. In this case, the receipt
%%%              is redirect sandbox that will return OK
%%%===================================================================
miniclip_msg_process_msg_redirect_sandbox_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% If request is for SandBox return OK
  meck:expect(httpc, request,
    fun(_, Req, _, _) ->
      case Req of
        %% If request is for Sandbox return OK
        {?APPLE_SAND_BOX,_, ?APPLE_CONTENT_TYPE, _} ->
          {ok, {?HTTP_OK,0,?IN_APP_RESP_MSG_OK} };
        %% If request is for Production redirect to sandbox
        {?APPLE_PRODUCTION,_, ?APPLE_CONTENT_TYPE, _} ->
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_ERR(?APPLE_STATUS_SANDBOX)} }
      end
    end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being processed
  ?assertMatch( {ok, _}, wait_delete_message(?DEFAULT_MSGS_TO_TEST,
                                             ?VALIDATION_TIMEOUT) ),

  %% Check All messages are valid and deleted
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs, send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  %% Check there were twice the number of https request
  ?assertEqual( 2*?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_expired_ok
%%%
%%% Description: This function is going to test if the receipt was expired
%%%              In this case, the post queue will receive an invalid receipt
%%%===================================================================
miniclip_msg_process_msg_expired_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return MD5 for the invalid message
  meck:expect(erlcloud_sqs, send_message,
    fun(?AWS_DEBUG_POST_QUEUE,_) ->
           [{message_id,0}, {md5_of_message_body, ?POST_QUEUE_MSG_INVALID_MD5}]
    end),

  %% Force apple answer to be valid but expired
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_EXPIRED} }
    end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being processed
  ?assertMatch( {ok, _}, wait_delete_message(?DEFAULT_MSGS_TO_TEST,
                                             ?VALIDATION_TIMEOUT) ),

  %% Check All messages are invalid and deleted
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs, send_message,
                             [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_INVALID])),
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  %% Check the number of https request
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_server_err_ok
%%%
%%% Description: This function is going to test what happens when apple
%%%              server returns an error to retry, the gen_server must
%%%              retry and succeed.
%%%===================================================================
miniclip_msg_process_msg_server_err_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return error at the first request
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      case get({number_of_calls}) of
        undefined -> % Return error at the first request
          put({number_of_calls}, 1),
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_ERR(?APPLE_STATUS_ERR_SERVER_1)} };
        _ -> % return OK for all other requests
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_OK} }
      end
    end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %% Check ALl messages are valid and deleted
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs, send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  %% Check there were twice the number of https request
  ?assertEqual( 2*?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_server_err_error
%%%
%%% Description: This function is going to test what happens when apple
%%%              server returns an invalid receipt, it should be discarded
%%%              because there is no transaction_id associated with.
%%%              No messages should be sent to post queue
%%%===================================================================
miniclip_msg_process_msg_server_err_error(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return error at the first request
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_ERR(?APPLE_STATUS_ERR_CORRUPTED)} }
    end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %% No message were sent
  ?assertEqual(0, meck:num_calls(erlcloud_sqs, send_message, ['_','_'])),
  ?assertEqual( 0, meck:num_calls(erlcloud_sqs, delete_message, ['_', '_'])),
  ok.


%%%===================================================================
%%% Function: miniclip_msg_process_msg_already_validated_ok
%%%
%%% Description: This function is going to test what happens when the
%%%              receipt is already validated (The default for all tests
%%%              is not validated before). The server should post
%%%              a message with invalid status.
%%%===================================================================
miniclip_msg_process_msg_already_validated_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return MD5 for the invalid message
  meck:expect(erlcloud_sqs, send_message,
    fun(?AWS_DEBUG_POST_QUEUE,_) ->
           [{message_id,0}, {md5_of_message_body, ?POST_QUEUE_MSG_INVALID_MD5}]
    end),

  %% Configure to return already validated
  meck:expect(erlcloud_ddb2, put_item,
    fun(_,_,_) ->
      {error, {?AWS_DDB2_CHECK_FAIL_MSG,'_'}}
    end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %% Check All messages were sent and are invalid, check that no valid
  %% messages were sent as well. The messages must be deleted as well
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs, send_message,
                             [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_INVALID])),
  ?assertEqual(0, meck:num_calls(erlcloud_sqs, send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_max_parallel_dynamodb_request_ok
%%%
%%% Description: This function is going to test what happens when the
%%%              maximum number of parellel requests for dynamodb is
%%%              achieved. The server is going to retry later and send
%%%              it.
%%%===================================================================
-define(PARALLEL_REQ_ERR_MSG_NUM, 2). % To avoid a long delay in tests
miniclip_msg_process_msg_max_parallel_dynamodb_request_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return already validated
  meck:expect(erlcloud_ddb2, put_item,
    fun(_,_,_) ->
      case get({number_of_calls}) of
        undefined -> % Return error at the first request
          put({number_of_calls}, 1),
          {error,{?AWS_DDB2_MAX_THROUGHPUT_MSG,0}};
        _ -> % return OK for all other requests
          {ok, [] }
      end
   end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?PARALLEL_REQ_ERR_MSG_NUM),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %%Check query was called twice for the number of messages
  ?assertEqual(2*?PARALLEL_REQ_ERR_MSG_NUM, meck:num_calls(erlcloud_ddb2, put_item,
                                                              ['_', '_', '_'])),
  %% Check All messages were sent and are valid and deleted
  ?assertEqual(?PARALLEL_REQ_ERR_MSG_NUM, meck:num_calls(erlcloud_sqs, send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  ?assertEqual(?PARALLEL_REQ_ERR_MSG_NUM,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_send_results_invalid_md5_ok
%%%
%%% Description: This function is going to test what happens when the
%%%              server sends the result and receive an invalid md5
%%%              It is expected to try again.
%%%===================================================================
miniclip_msg_process_msg_send_results_invalid_md5_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return already validated
  meck:expect(erlcloud_sqs, send_message,
    fun(_,_) ->
      case get({number_of_calls}) of
        undefined -> % Return error at the first request
          put({number_of_calls}, 1),
          [{message_id,0}, {md5_of_message_body, "deadbeef"}];
        _ -> % return OK for all other requests
          [{message_id,0}, {md5_of_message_body, ?POST_QUEUE_MSG_OK_MD5}]
      end
   end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %%Check query was called one time only per message
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_ddb2, put_item,
                                                              ['_', '_', '_'])),
  %% Check the sender was called twice (first invalid md5, second valid md5)
  ?assertEqual(2 * ?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs,
                                          send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_delete_message_invalid_ok
%%%
%%% Description: This function is going to test what happens when the
%%%              server delete a message and receive an error
%%%              It is expected to try again.
%%%===================================================================
miniclip_msg_process_msg_delete_message_invalid_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return error when deleting the message for the first time
  meck:expect(erlcloud_sqs, delete_message,
    fun(_,_) ->
      case get({number_of_calls}) of
        undefined -> % Return error at the first request
          put({number_of_calls}, 1),
          error;
        _ -> % return OK for all other requests
          ok
      end
   end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %%Check query was called one time only per message
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_ddb2, put_item,
                                                              ['_', '_', '_'])),
  %% Check the sender was called only once
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs,
                                          send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  %% Check the delete_message was called twice
  ?assertEqual(2 * ?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_http_invalid_ok
%%%
%%% Description: This function is going to test what happens when the
%%%              server receives an http error when validating Apple receipt
%%%              It is expected to try again.
%%%===================================================================
miniclip_msg_process_msg_http_invalid_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return an error at first request
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      case get({number_of_calls}) of
        undefined -> % Return error at the first request
          put({number_of_calls}, 1),
          error;
        _ -> % return OK for all other requests
          {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_OK} }
      end
   end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %%Check query was called one time only per message
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_ddb2, put_item,
                                                              ['_', '_', '_'])),
  %% Check the sender was called only once
  ?assertEqual(?DEFAULT_MSGS_TO_TEST, meck:num_calls(erlcloud_sqs,
                                          send_message,
                                  [?AWS_DEBUG_POST_QUEUE, ?POST_QUEUE_MSG_OK])),
  %% Check the delete_message only once
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),
  %% Check the httpc request was called twice
  ?assertEqual(2*?DEFAULT_MSGS_TO_TEST,
                          meck:num_calls(httpc, request, ['_', '_', '_', '_'])),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_send_apple_timeout_ok
%%%
%%% Description: This function is testing the maximum number of tries
%%%              for apple website.
%%%===================================================================
miniclip_msg_process_msg_send_apple_timeout_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return always OK
  meck:expect(httpc, request,
    fun(_, _, _, _) ->
      {ok, {?HTTP_OK,0, ?IN_APP_RESP_MSG_ERR(?APPLE_STATUS_ERR_SERVER_1)} }
    end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %% Check no messages were sent or processed
  ?assertEqual(0, meck:num_calls(erlcloud_sqs, send_message, ['_', '_'])),
  ?assertEqual(0, meck:num_calls(erlcloud_ddb2, put_item, ['_', '_', '_'])),
  ?assertEqual(0, meck:num_calls(erlcloud_sqs, delete_message, ['_', '_'])),

  %% Check the httpc request by the number of tries
  ?assertEqual( ?MAX_NUM_TRIES*?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_send_dynamoDB_timeout_ok
%%%
%%% Description: This function is testing the maximum number of tries
%%%              for dynamoDB.
%%%===================================================================
-define(DDB2_TIMEOUT_MSG_NUM, 2). % keep low numer to avoid long delays
miniclip_msg_process_msg_send_dynamoDB_timeout_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return try again
  meck:expect(erlcloud_ddb2, put_item,
    fun(_,_,_) ->
      {error,{?AWS_DDB2_MAX_THROUGHPUT_MSG,0}}
    end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DDB2_TIMEOUT_MSG_NUM),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %% Check no messages were sent
  ?assertEqual(0, meck:num_calls(erlcloud_sqs, send_message, ['_', '_'])),
  ?assertEqual(0, meck:num_calls(erlcloud_sqs, delete_message, ['_', '_'])),

  %% Check number of put_items must be the double
  ?assertEqual(?MAX_NUM_TRIES*?DDB2_TIMEOUT_MSG_NUM,
               meck:num_calls(erlcloud_ddb2, put_item, ['_', '_', '_'])),

  %% Check the httpc must be called
  ?assertEqual( ?DDB2_TIMEOUT_MSG_NUM,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_send_post_queue_timeout_ok
%%%
%%% Description: This function is testing the maximum number of tries
%%%              for post queue.
%%%===================================================================
-define(SQS_TIMEOUT_MSG_NUM, 2). % keep low numer to avoid long delays
miniclip_msg_process_msg_send_post_queue_timeout_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to invalids md5 for send_message
  meck:expect(erlcloud_sqs, send_message,
   fun(_,_) ->
      [{message_id,0}, {md5_of_message_body, "deadbeef"}]
   end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?SQS_TIMEOUT_MSG_NUM),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %% Check the message was called twice
  ?assertEqual(?MAX_NUM_TRIES*?SQS_TIMEOUT_MSG_NUM,
               meck:num_calls(erlcloud_sqs, send_message, ['_', '_'])),
  %% No messages were deleted due the timeout
  ?assertEqual(0, meck:num_calls(erlcloud_sqs, delete_message,['_', '_'])),
  %% Check the put_items must be the double
  ?assertEqual( ?SQS_TIMEOUT_MSG_NUM,
               meck:num_calls(erlcloud_ddb2, put_item, ['_', '_', '_'])),
  %% Check the httpc must be called
  ?assertEqual( ?SQS_TIMEOUT_MSG_NUM,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% Function: miniclip_msg_process_msg_delete_timeout_ok
%%%
%%% Description: This function is testing the maximum number of tries
%%%              for deleting the message.
%%%===================================================================
miniclip_msg_process_msg_delete_timeout_ok(_Config) ->
  %% Start the process message server
  miniclip_msg_sup:start_link(),

  %% Configure to return error when messages are deleted
  meck:expect(erlcloud_sqs, delete_message,
    fun(?AWS_RECEIPT_SQS_NAME,_) ->
      error
    end),

  % Create all servers that will process the validation
  create_default_miniclip_msg(?DEFAULT_MSGS_TO_TEST),

  %% Wait all messages being terminated
  ?assertMatch( {ok, _},
                  wait_for_no_active_miniclip_msg_server(?VALIDATION_TIMEOUT) ),

  %% Check the message was called twice
  ?assertEqual(?DEFAULT_MSGS_TO_TEST,
               meck:num_calls(erlcloud_sqs, send_message, ['_', '_'])),

  %% Messages were deleted only once
  ?assertEqual(?MAX_NUM_TRIES*?DEFAULT_MSGS_TO_TEST,
                           meck:num_calls(erlcloud_sqs, delete_message,
                                 [?AWS_RECEIPT_SQS_NAME, ?RECEIPT_SQS_HANDLE])),

  %% Check the put_items
  ?assertEqual( ?DEFAULT_MSGS_TO_TEST,
               meck:num_calls(erlcloud_ddb2, put_item, ['_', '_', '_'])),

  %% Check the httpc must be called
  ?assertEqual( ?DEFAULT_MSGS_TO_TEST,
                meck:num_calls(httpc, request, [post,'_','_', '_']) ),
  ok.

%%%===================================================================
%%% local functions
%%%===================================================================
-define(SLEEP_TO_CHECK, 1).

%%--------------------------------------------------------------------
%% @doc This function will wait for messages being deleted by the post queue,
%%      which means the whole validation cycle was completed
%%
%% @param ExpectedNumberOfMsgs Expected number of messages
%% @param Timeout maximum timeout for receiving messages
%% @end
%%--------------------------------------------------------------------
wait_delete_message(ExpectedNumberOfMsgs, Timeout) when Timeout > 0 ->
  Msgs = meck:num_calls(erlcloud_sqs, delete_message, [?AWS_RECEIPT_SQS_NAME,
                                                       ?RECEIPT_SQS_HANDLE]),
  case Msgs of
    ExpectedNumberOfMsgs -> { ok, 0 };
    _ -> timer:sleep(?SLEEP_TO_CHECK),
         wait_delete_message(ExpectedNumberOfMsgs, (Timeout - ?SLEEP_TO_CHECK))
  end;

wait_delete_message(_, _) ->
  {error, meck:num_calls(erlcloud_sqs, delete_message, [?AWS_RECEIPT_SQS_NAME,
                                                        ?RECEIPT_SQS_HANDLE])}.

%%--------------------------------------------------------------------
%% @doc This function will wait for all messages to be read from AWS ReceiptsSQS
%%      with the receipt information
%%
%% @param ExpectedNumberOfMsgs Expected number of messages
%% @param Timeout maximum timeout for receiving messages
%% @end

%%--------------------------------------------------------------------
wait_read_server_to_receive(ExpectedNumberOfMsgs, Timeout) when Timeout > 0 ->
  case try_get_state(miniclip) of
    #{msg_counter := ExpectedNumberOfMsgs} -> { ok, 0 };
    _ -> timer:sleep(?SLEEP_TO_CHECK),
         wait_read_server_to_receive(ExpectedNumberOfMsgs, (Timeout - ?SLEEP_TO_CHECK))
  end;

wait_read_server_to_receive(_, _) ->
  #{msg_counter := Counter} = try_get_state(miniclip),
  {error, Counter}.

%%--------------------------------------------------------------------
%% @doc This function will be called to mock reading messages from AWS SQS
%%
%% @param NumberOfMsgs Number of messages to be available to read
%% @param Message Available message
%% @param MessageMd5 Md5 of the message
%% @end
%%--------------------------------------------------------------------
aws_sqs_available(NumberOfMsgs, Message, MessageMd5) when NumberOfMsgs > 0 ->
  case get({aws_sqs_msgs}) of
    0 -> [{messages, [] }];
    undefined -> put({aws_sqs_msgs}, NumberOfMsgs - 1),
        [{messages, ?MSG_TEMPLATE(Message, MessageMd5, ?RECEIPT_SQS_HANDLE) }];
    N -> put({aws_sqs_msgs}, N - 1),
        [{messages, ?MSG_TEMPLATE(Message, MessageMd5, ?RECEIPT_SQS_HANDLE) }]
  end.

%%--------------------------------------------------------------------
%% @doc This function will wait until no active miniclip msg servers
%%      are active
%%
%% @param Timeout Timeout to wait
%% @end
%%--------------------------------------------------------------------
wait_for_no_active_miniclip_msg_server(Timeout) when Timeout > 0 ->
  [_,{active,ActiveMsgServers},_,_] =
                                  supervisor:count_children(miniclip_msg_sup),
  case ActiveMsgServers of
    0 -> { ok, 0 };
    _ -> timer:sleep(?SLEEP_TO_CHECK),
         wait_for_no_active_miniclip_msg_server(Timeout - ?SLEEP_TO_CHECK)
  end;

wait_for_no_active_miniclip_msg_server(_) ->
  [_,{active,ActiveMsgServers},_,_] =
                                  supervisor:count_children(miniclip_msg_sup),
  {error, ActiveMsgServers}.

%%--------------------------------------------------------------------
%% @doc This function creates the required number of miniclip_msg server
%%      with default body
%%
%% @param NumberOfServer Number of entities
%% @end
%%--------------------------------------------------------------------
create_default_miniclip_msg(NumberOfServer) ->
  lists:foreach(
    fun(_) ->
      BodyMap = ?AWS_MAP_BODY_ENCODE,
      miniclip_msg_sup:process_msg(BodyMap#{ ?MAP_RCPT_HANDLE =>
                                             ?RECEIPT_SQS_HANDLE})
    end,
    lists:seq(1,NumberOfServer)
  ).
%%--------------------------------------------------------------------
%% @doc This function try to get the state of a registered server
%%
%% @param Name Server name
%% @end
%%--------------------------------------------------------------------
try_get_state(Name)->
  try sys:get_state(Name) of
    S -> S
  catch
    _:{noproc,_} -> undefined
  end.


