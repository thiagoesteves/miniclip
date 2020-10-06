%%%-------------------------------------------------------------------
%%% Created : 03 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This is the debug module that helps to load messsages and to validate
%%%      at apple website
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(miniclip_dbg).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("miniclip.hrl").

-dialyzer({nowarn_function, send_msg/1}).

%%%===================================================================
%%% Function exports
%%%===================================================================

-export([apple_verification/0, load/1]).

%%%===================================================================
%%% Local Defines
%%%===================================================================

-define(MAX_USER_VAL, 100000520512).

%% AWS body message
-define(AWS_SQS_MSG_BODY, [ {body, Body},
                            {md5_of_body,_},
                            {message_id,_},
                            {receipt_handle,Handle},
                            {attributes,_},
                            {message_attributes,_} ] ).

-define(IN_APP_RECEIPT_OK_1,
 <<"ewoJInNpZ25hdHVyZSIgPSAiQWh0UkxxU0dlWHF3SEVNUmZkTk5hSWZBS2NSOHpSZ2tsd2ZpWkhLZWEvbkVJQVR5d
  E8xbWhDOWkyVVlQdmQ3WnVQR1B2Y3JMdnd2aVl0NkwrU1FLeTJqcmRtVFhUU2txUnZGOXFla1VzenRoemlBQ0EyZn
  RjQUhQUGQ4QTI4MFBmdzEydEdUenNzaHFPZUNqYkRIejlJbERydU5yWGRldVZZUXVVMU9HNDdFc0FBQURWekNDQTF
  Nd2dnSTdvQU1DQVFJQ0NHVVVrVTNaV0FTMU1BMEdDU3FHU0liM0RRRUJCUVVBTUg4eEN6QUpCZ05WQkFZVEFsVlRN
  Uk13RVFZRFZRUUtEQXBCY0hCc1pTQkpibU11TVNZd0pBWURWUVFMREIxQmNIQnNaU0JEWlhKMGFXWnBZMkYwYVc5d
  UlFRjFkR2h2Y21sMGVURXpNREVHQTFVRUF3d3FRWEJ3YkdVZ2FWUjFibVZ6SUZOMGIzSmxJRU5sY25ScFptbGpZWF
  JwYjI0Z1FYVjBhRzl5YVhSNU1CNFhEVEE1TURZeE5USXlNRFUxTmxvWERURTBNRFl4TkRJeU1EVTFObG93WkRFak1
  DRUdBMVVFQXd3YVVIVnlZMmhoYzJWU1pXTmxhWEIwUTJWeWRHbG1hV05oZEdVeEd6QVpCZ05WQkFzTUVrRndjR3hs
  SUdsVWRXNWxjeUJUZEc5eVpURVRNQkVHQTFVRUNnd0tRWEJ3YkdVZ1NXNWpMakVMTUFrR0ExVUVCaE1DVlZNd2daO
  HdEUVlKS29aSWh2Y05BUUVCQlFBRGdZMEFNSUdKQW9HQkFNclJqRjJjdDRJclNkaVRDaGFJMGc4cHd2L2NtSHM4cC
  9Sd1YvcnQvOTFYS1ZoTmw0WElCaW1LalFRTmZnSHNEczZ5anUrK0RyS0pFN3VLc3BoTWRkS1lmRkU1ckdYc0FkQkV
  qQndSSX
  hleFRldngzSExFRkdBdDFtb0t4NTA5ZGh4dGlJZERnSnYyWWFWczQ5QjB1SnZOZHk2U01xTk5MSHNETHpEUzlvWkh
  BZ01CQUFHamNqQndNQXdHQTFVZEV3RUIvd1FDTUFBd0h3WURWUjBqQkJnd0ZvQVVOaDNvNHAyQzBnRVl0VEpyRHRk
  REM1RllRem93RGdZRFZSMFBBUUgvQkFRREFnZUFNQjBHQTFVZERnUVdCQlNwZzRQeUdVakZQaEpYQ0JUTXphTittV
  jhrOVRBUUJnb3Foa2lHOTJOa0JnVUJCQUlGQURBTkJna3Foa2lHOXcwQkFRVUZBQU9DQVFFQUVhU2JQanRtTjRDL0
  lCM1FFcEszMlJ4YWNDRFhkVlhBZVZSZVM1RmFaeGMrdDg4cFFQOTNCaUF4dmRXLzNlVFNNR1k1RmJlQVlMM2V0cVA
  1Z204d3JGb2pYMGlreVZSU3RRKy9BUTBLRWp0cUIwN2tMczlRVWU4Y3pSOFVHZmRNMUV1bVYvVWd2RGQ0TndOWXhM
  UU1nNFdUUWZna1FRVnk4R1had1ZIZ2JFL1VDNlk3MDUzcEdYQms1MU5QTTN3b3hoZDNnU1JMdlhqK2xvSHNTdGNUR
  XFlOXBCRHBtRzUrc2s0dHcrR0szR01lRU41LytlMVFUOW5wL0tsMW5qK2FCdzdDMHhzeTBiRm5hQWQxY1NTNnhkb3
  J5L0NVdk02Z3RLc21uT09kcVRlc2JwMGJzOHNuNldxczBDOWRnY3hSSHVPTVoydG04bnBMVW03YXJnT1N6UT09Ijs
  KCSJwdXJjaGFzZS1pbmZvIiA9ICJld29KSW05eWFXZHBibUZzTFhCMWNtTm9ZWE5sTFdSaGRHVXRjSE4wSWlBOUlD
  SXlNREV5TFRBMExURTRJREExT2pBM09qRXpJRUZ0WlhKcFkyRXZURzl6WDBGdVoyVnNaWE1pT3dvSkltaHZjM1JsW
  kMxcFlYQXRkbVZ5YzJsd
  mJpSWdQU0FpTVM0d0xqRWlPd29KSW05eWFXZHBibUZzTFhSeVlXNXpZV04wYVc5dUxXbGtJaUE5SUNJeE1EQXdNRE
  F3TURReU9USTROVFkzSWpzS0NTSmlkbkp6SWlBOUlDSXhMakV1TVNJN0Nna2lkSEpoYm5OaFkzUnBiMjR0YVdRaUl
  EMGdJakV3TURBd01EQXdOREk1TWpnMU5qY2lPd29KSW5GMVlXNTBhWFI1SWlBOUlDSXhJanNLQ1NKdmNtbG5hVzVo
  YkMxd2RYSmphR0Z6WlMxa1lYUmxMVzF6SWlBOUlDSXhNek0wTnpVd09ETXpNREF3SWpzS0NTSndjbTlrZFdOMExXb
  GtJaUE5SUNKamIyMHViV2x1YVdOc2FYQXVZVzVwYldGc2MyaGxiSFJsY2k1amRYSnlaVzVqZVZCaFkyc3hJanNLQ1
  NKcGRHVnRMV2xrSWlBOUlDSTBOekF5TVRneE9EZ2lPd29KSW1KcFpDSWdQU0FpWTI5dExtMXBibWxqYkdsd0xtRnV
  hVzFoYkhOb1pXeDBaWElpT3dvSkluQjFjbU5vWVhObExXUmhkR1V0YlhNaUlEMGdJakV6TXpRM05UQTRNek13TURB
  aU93b0pJbkIxY21Ob1lYTmxMV1JoZEdVaUlEMGdJakl3TVRJdE1EUXRNVGdnTVRJNk1EYzZNVE1nUlhSakwwZE5WQ
  0k3Q2draWNIVnlZMmhoYzJVdFpHRjBaUzF3YzNRaUlEMGdJakl3TVRJdE1EUXRNVGdnTURVNk1EYzZNVE1nUVcxbG
  NtbGpZUzlNYjNOZlFXNW5aV3hsY3lJN0Nna2liM0pwWjJsdVlXd3RjSFZ5WTJoaGMyVXRaR0YwWlNJZ1BTQWlNakF
  4TWkwd05DMHhPQ0F4TWpvd056b3hNeUJGZEdNdlIwMVVJanNLZlE9PSI7CgkiZW52aXJvbm1lbnQiID0gIlNhbmRi
  b3giOwoJInBvZCIgPSAi MTAwIjsKCSJzaWduaW5nLXN0YXR1cyIgPSAiMCI7Cn0=">>).

-define(IN_APP_RECEIPT_OK_2,
  <<"eyJzaWduYXR1cmUiID0gIkFoaHEwbmFxaG01ci8wSUNuYk9hVThCeVBLNkRja2ZsanRCMDNnZUh4dk0ybEVjVk
  dqK2NVM1lnWGZ0RkZCZ2lFYkR1NGdoYXFVWFRqRzlpc25Zeit6VWFhTXRUZDJ6YnNLbFhIMitzYm1ZaC9tY2M2eWt
  3dVFkaFZsOWZKYkxNaFVXWHNTUlIxVlVjQlE4TkxjVGg5ZHNXOTVTREcxdG9DQk45cWM0L01CZlVBQUFEVnpDQ0Ex
  TXdnZ0k3b0FNQ0FRSUNDQnVwNCtQQWhtL0xNQTBHQ1NxR1NJYjNEUUVCQlFVQU1IOHhDekFKQmdOVkJBWVRBbFZUT
  VJNd0VRWURWUVFLREFwQmNIQnNaU0JKYm1NdU1TWXdKQVlEVlFRTERCMUJjSEJzWlNCRFpYSjBhV1pwWTJGMGFXOX
  VJRUYxZEdodmNtbDBlVEV6TURFR0ExVUVBd3dxUVhCd2JHVWdhVlIxYm1WeklGTjBiM0psSUVObGNuUnBabWxqWVh
  ScGIyNGdRWFYwYUc5eWFYUjVNQjRYRFRFME1EWXdOekF3TURJeU1Wb1hEVEUyTURVeE9ERTRNekV6TUZvd1pERWpN
  Q0VHQTFVRUF3d2FVSFZ5WTJoaGMyVlNaV05sYVhCMFEyVnlkR2xtYVdOaGRHVXhHekFaQmdOVkJBc01Fa0Z3Y0d4b
  ElHbFVkVzVsY3lCVGRHOXlaVEVUTUJFR0ExVUVDZ3dLUVhCd2JHVWdTVzVqTGpFTE1Ba0dBMVVFQmhNQ1ZWTXdnWj
  h3RFFZSktvWklodmNOQVFFQkJRQURnWTBBTUlHSkFvR0JBTW1URXVMZ2ppbUx3Ukp4eTFvRWYwZXNVTkRWRUllNnd
  Ec25uYWwxNGhOQnQxdjE5NVg2bjkzWU83Z2kzb3JQU3V4OUQ1NTRTa01wK1NheWc4NGxUYzM2MlV0bVlMcFduYjM0
  bnF5R3g5S0JWVHk1T0dWNGxqRTFPd0Mrb1RuUk0rUUxSQ21lTnhNYlBaaFM0N1QrZVp0REVoVkI5dXNrMytKTTJDb
  2dmd283QWdNQkFBR2pjakJ3TUIwR0ExVWREZ1FXQkJTSmFFZU51cTlEZjZaZk42OEZlK0kydTIyc3NEQU1CZ05WSF
  JNQkFmOEVBakFBTUI4R0ExVWRJd1FZTUJhQUZEWWQ2T0tkZ3RJQkdMVXlhdzdYUXd1UldFTTZNQTRHQTFVZER3RUI
  vd1FFQXdJSGdEQVFCZ29xaGtpRzkyTmtCZ1VCQkFJRkFEQU5CZ2txaGtpRzl3MEJBUVVGQUFPQ0FRRUFlYUpWMlU1
  MXJ4ZmNxQUFlNUMyL2ZFVzhLVWw0aU80bE11dGE3TjZYelAxcFpJejFOa2tDdElJd2V5Tmo1VVJZSEsrSGpSS1NVO
  VJMZ3VObDBua2Z4cU9iaU1ja3dSdWRLU3E2OU5JbnJaeUNENjZSNEs3N25iOWxNVEFCU1NZbHNLdDhvTnRsaGdSLz
  FralNTUlFjSGt0c0RjU2lRR0tNZGtTbHA0QXlYZjd2bkhQQmU0eUN3WVYyUHBTTjA0a2JvaUozcEJseHNHd1YvWmx
  MMjZNMnVlWUhLWUN1WGhkcUZ3eFZnbTUyaDNvZUpPT3Qvdlk0RWNRcTdlcUhtNm0wM1o5YjdQUnpZTTJLR1hIRG1P
  TWs3dkRwZU1WbExEUFNHWXoxK1Uzc0R4SnplYlNwYmFKbVQ3aW16VUtmZ2dFWTd4eGY0Y3pmSDB5ajV3TnpTR1RPd
  lE9PSI7ICJwdXJjaGFzZS1pbmZvIiA9ICJld29KSW05eWFXZHBibUZzTFhCMWNtTm9ZWE5sTFdSaGRHVXRjSE4wSW
  lBOUlDSXlNREUxTFRFeExUSXpJREE0T2pNeU9qSTNJRUZ0WlhKcFkyRXZURzl6WDBGdVoyVnNaWE1pT3dvSkluVnV
  hWEYxWlMxcFpHVnVkR2xtYVdWeUlpQTlJQ0psTkRjME1qVmtaamRtWWpaaFlqaGpNMk0zWlRVM016QmpNMkUzWldJ
  MVlqWTFOak00TWpOa0lqc0tDU0p2Y21sbmFXNWhiQzEwY21GdWMyRmpkR2x2YmkxcFpDSWdQU0FpTVRBd01EQXdNR
  EU0TVRRNU1qVTFOeUk3Q2draVluWnljeUlnUFNBaU15STdDZ2tpZEhKaGJuTmhZM1JwYjI0dGFXUWlJRDBnSWpFd0
  1EQXdNREF4T0RFME9USTFOVGNpT3dvSkluRjFZVzUwYVhSNUlpQTlJQ0l4SWpzS0NTSnZjbWxuYVc1aGJDMXdkWEp
  qYUdGelpTMWtZWFJsTFcxeklpQTlJQ0l4TkRRNE1qazJNelEzTVRNNElqc0tDU0oxYm1seGRXVXRkbVZ1Wkc5eUxX
  bGtaVzUwYVdacFpYSWlJRDBnSWpWRE1qYzFRalZCTFRORk5qWXROREF5TmkwNU5USXpMVGcwTlVJeU1qVTNOVFZHU
  XlJN0Nna2ljSEp2WkhWamRDMXBaQ0lnUFNBaWNIVnlZMmhoYzJWeUxtTnZibk4xYldGaWJHVkdaV0YwZFhKbElqc0
  tDU0pwZEdWdExXbGtJaUE5SUNJeE1EWXhOVFUzTkRnMElqc0tDU0ppYVdRaUlEMGdJbU52YlM1bGN5NVFkWEpqYUd
  GelpYSWlPd29KSW5CMWNtTm9ZWE5sTFdSaGRHVXRiWE1pSUQwZ0lqRTBORGd5T1RZek5EY3hNemdpT3dvSkluQjFj
  bU5vWVhObExXUmhkR1VpSUQwZ0lqSXdNVFV0TVRFdE1qTWdNVFk2TXpJNk1qY2dSWFJqTDBkTlZDSTdDZ2tpY0hWe
  VkyaGhjMlV0WkdGMFpTMXdjM1FpSUQwZ0lqSXdNVFV0TVRFdE1qTWdNRGc2TXpJNk1qY2dRVzFsY21sallTOU1iM0
  5mUVc1blpXeGxjeUk3Q2draWIzSnBaMmx1WVd3dGNIVnlZMmhoYzJVdFpHRjBaU0lnUFNBaU1qQXhOUzB4TVMweU1
  5QXhOam96TWpveU55QkZkR012UjAxVUlqc0tmUT09IjsiZW52aXJvbm1lbnQiID0gIlNhbmRib3giOyJwb2QiID0g
  IjEwMCI7InNpZ25pbmctc3RhdHVzIiA9ICIwIjt9">>).

%%%===================================================================
%%% API
%%%===================================================================
-spec apple_verification() -> ok.
apple_verification() ->
  Request = {?APPLE_SAND_BOX, [], ?APPLE_CONTENT_TYPE,
             jsone:encode( #{<<"receipt-data">> => ?IN_APP_RECEIPT_OK_1} ) },
  Res = httpc:request(post, Request, [], []),
  io:format("\n\r ~p", [Res]),
  ok.

-spec load(integer()) -> ok.
load(Messages) ->
  ReceiptList = [?IN_APP_RECEIPT_OK_1, ?IN_APP_RECEIPT_OK_2],
  List = lists:foldl(fun(_, X) -> X ++ ReceiptList end,
                     [],
                     lists:seq(1,Messages div 2)),
  [ send_msg(Receip) || Receip <- List],
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
-spec send_msg(string()) -> ok.
send_msg(Receipt) ->
  User = rand:uniform(?MAX_USER_VAL),
  Data = jsone:encode( #{<<"user_id">>    => User,
                         <<"receipt">>    => Receipt,
                         <<"post_queue">> => <<?AWS_DEBUG_POST_QUEUE>> }, [] ),
  [{message_id,_}, {md5_of_message_body,_}] =
                                 erlcloud_sqs:send_message(?AWS_RECEIPT_SQS_NAME, Data),
  ok.
