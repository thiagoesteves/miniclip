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
-define(AWS_ACCESS_KEY_ID,     "AKIAIBEF4BTNHA6TREMA").
-define(AWS_SECRET_ACCESS_KEY, "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").
-define(AWS_REGION,            "sa-east-1").
%% AWS SQS names
-define(AWS_SQS_NAME,          "Miniclip").
-define(AWS_DEBUG_POST_QUEUE,  "ResultMiniclip").
%% AWS General Definitions
-define(AWS_MAX_READ_MSGS,      10).
%% AWS Errors
%% https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Programming.Errors.html
-define(AWS_MAX_THROUGHPUT_MSG, <<"ProvisionedThroughputExceededException">>).
-define(AWS_LIMIT_OPERATIONS,   48). %% 50 is the maximum

% Body Messages read and post
-define(MAP_RECEIPT,    <<"receipt">>).
-define(MAP_USER_ID,    <<"user_id">>).
-define(MAP_POST_QUEUE, <<"post_queue">>).
-define(MAP_TRANS_ID,   <<"transaction_id">>).
-define(MAP_STATUS,     <<"status">>).

%% Apple definitions
-define(APPLE_PRODUCTION,   "https://buy.itunes.apple.com/verifyReceipt").
-define(APPLE_SAND_BOX,     "https://sandbox.itunes.apple.com/verifyReceipt").
-define(APPLE_CONTENT_TYPE, "application/json").

-define(STATUS_OK,           0).
-define(STATUS_SANDBOX,      21007).
%% Server code that requires another atempt to validate
-define(STATUS_ERR_SERVER_1, 21005).
-define(STATUS_ERR_SERVER_2, 21009).

%% Return messages
-define(OK,    <<"OK">>).
-define(ERROR, <<"INVALID">>).

-endif. %% miniclip
