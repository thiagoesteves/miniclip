%%%-------------------------------------------------------------------
%%% Created : 04 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc
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
-define(AWS_SQS_NAME,          "Miniclip").
-define(AWS_DEBUG_POST_QUEUE,  "ResultMiniclip").

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

-define(STATUS_OK,      0).
-define(STATUS_SANDBOX, 21007).

%% Return messages
-define(OK,    <<"OK">>).
-define(ERROR, <<"INVALID">>).

-endif. %% miniclip
