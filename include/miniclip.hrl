%%%-------------------------------------------------------------------
%%% Created : 04 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc Definitions for the whole application
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(miniclip).
-define(miniclip, true).

%%%===================================================================
%%% Global Defines
%%%===================================================================

%% AWS Credential definitions
-define(AWS_ACCESS_KEY_ID,     "XXXXXXXX").
-define(AWS_SECRET_ACCESS_KEY, "XXXXXXXX").
-define(AWS_REGION,            "sa-east-1").
%% AWS SQS names
-define(AWS_RECEIPT_SQS_NAME,  "ReceiptsSQS").
-define(AWS_DEBUG_POST_QUEUE,  "ResultsSQS").

%% AWS SQS Definitions
-define(AWS_SQS_MAX_READ_MSGS,  10).
-define(AWS_SQS_INVALID_SERVER, {aws_error,{http_error,400,"Bad Request",_}} ).

%% AWS Errors for dynamoDB
%% https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Programming.Errors.html
-define(AWS_DDB2_CHECK_FAIL_MSG,     <<"ConditionalCheckFailedException">>).
-define(AWS_DDB2_MAX_THROUGHPUT_MSG, <<"ProvisionedThroughputExceededException">>).
-define(AWS_DDB2_LIMIT_OPERATIONS,   50). % Maximum parallel operation for dynamoDB

%% DDB2 defines (names, fields, etc)
-define(DDB2_NAME,     <<"TransactionIdTable">>).
-define(DDB2_F1,       <<"TransactionId">>).

%% See https://developer.apple.com/documentation/storekit/in-app_purchase/validating_receipts_with_the_app_store
-define(APPLE_PRODUCTION,   "https://buy.itunes.apple.com/verifyReceipt").
-define(APPLE_SAND_BOX,     "https://sandbox.itunes.apple.com/verifyReceipt").
-define(APPLE_CONTENT_TYPE, "application/json").

%% See https://developer.apple.com/documentation/appstorereceipts/status
-define(APPLE_STATUS_OK,           0).
-define(APPLE_STATUS_SANDBOX,      21007). % Redirect to sandbox
-define(APPLE_STATUS_ERR_SERVER_1, 21005). % Server error, try again
-define(APPLE_STATUS_ERR_SERVER_2, 21009). % Server error, try again
-define(APPLE_STATUS_ERR_EXPIRED,  21006). % Valid, but expired
-define(APPLE_STATUS_ERR_CORRUPTED,21002). % Receipt corrupted

%% Return messages for the receipt validation
-define(OK,    <<"OK">>).
-define(ERROR, <<"INVALID">>).

%% Http defines
-define(HTTP_OK, {"HTTP/1.1",200,"OK"}).

% Body Messages with all information needed to validate the receip
-define(MAP_RECEIPT,     <<"receipt">>).
-define(MAP_USER_ID,     <<"user_id">>).
-define(MAP_POST_QUEUE,  <<"post_queue">>).
-define(MAP_TRANS_ID,    <<"transaction_id">>).
-define(MAP_STATUS,      <<"status">>).
-define(MAP_RETRIES,     retries).
-define(MAP_VALIDATION,  validation).
-define(MAP_APPLE_RCPT,  apple_receipt).
-define(MAP_RCPT_HANDLE, receipt_handle).

-endif. %% miniclip
