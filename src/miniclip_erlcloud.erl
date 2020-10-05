%%%-------------------------------------------------------------------
%%% Created : 06 Oct 2020 by Thiago Esteves <calori@gmail.com>
%%%
%%% @doc This wrapper will allow to hold the system when the maximum
%%%      throughput is achieved (Future options can be added here)
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(miniclip_erlcloud).

-author('Thiago Esteves').

%%%===================================================================
%%% Includes
%%%===================================================================

-include("miniclip.hrl").

%%%===================================================================
%%% Function exports
%%%===================================================================

%% Public API export
-export([ddb2_put_item/2,
         ddb2_q/3,
         ddb2_create_table/5]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

-define(THROUGHPUT_WAIT_TIME, 10000).

%%%===================================================================
%%% API
%%%===================================================================
-spec ddb2_put_item(erlcloud:table_name(),
                    erlcloud:in_item()) -> erlcloud:put_item_return().
ddb2_put_item(Name, Item) ->
  case erlcloud_ddb2:put_item(Name, Item) of
    {error,{?AWS_MAX_THROUGHPUT_MSG,_}} ->
        io:format("+"),
        timer:sleep(?THROUGHPUT_WAIT_TIME),
        ddb2_put_item(Name, Item);
    _ = Res ->
        Res
  end.

-spec ddb2_q(erlcloud:table_name(),
             erlcloud:conditions() | erlcloud:expression(),
             erlcloud:q_opts()) -> erlcloud:q_return().
ddb2_q(Name, Conditions, Options) ->
  case erlcloud_ddb2:q(Name, Conditions, Options) of
    {error,{?AWS_MAX_THROUGHPUT_MSG,_}} ->
        io:format("q"),
        timer:sleep(?THROUGHPUT_WAIT_TIME),
        ddb2_q(Name, Conditions, Options);
    _ = Res ->
        Res
  end.

-spec ddb2_create_table(erlcloud:table_name(), erlcloud:attr_defs(),
                        erlcloud:key_schema(), erlcloud:read_units(),
                        erlcloud:write_units())
                  -> erlcloud:create_table_return().
ddb2_create_table(Name, AttrDefs, Key, RUnits, WUnits) ->
  case erlcloud_ddb2:create_table(Name, AttrDefs, Key, RUnits, WUnits) of
    {error,{?AWS_MAX_THROUGHPUT_MSG,_}} ->
        io:format("c"),
        timer:sleep(?THROUGHPUT_WAIT_TIME),
        ddb2_create_table(Name, AttrDefs, Key, RUnits, WUnits);
    _ = Res ->
        Res
  end.

%%====================================================================
%% Internal functions
%%====================================================================

