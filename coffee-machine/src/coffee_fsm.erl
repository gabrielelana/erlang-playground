-module(coffee_fsm).
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_fsm).

-export([start_link/0, init/1]).
-export([stop/0, terminate/3]).
-export([americano/0, cappuccino/0, tea/0, espresso/0, cup_removed/0, pay/1, cancel/0]).
-export([selection/2, payment/2, remove/2, handle_event/3, handle_info/3, code_change/4, handle_sync_event/4]).


%% Client functions

start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_fsm:send_all_state_event(?MODULE, stop).

americano() ->
  gen_fsm:send_even(?MODULE, {selection, americano, 150}).

cappuccino() ->
  gen_fsm:send_even(?MODULE, {selection, cappuccino, 150}).

espresso() ->
  gen_fsm:send_even(?MODULE, {selection, espresso, 100}).

tea() ->
  gen_fsm:send_even(?MODULE, {selection, tea, 100}).

cup_removed() ->
  gen_fsm:send_event(?MODULE, cup_removed).

pay(Coin) ->
  gen_fsm:send_event(?MODULE, {pay, Coin}).

cancel() ->
  gen_fsm:send_event(?MODULE, cancel).


%% FSM callbacks

init([]) ->
  hw:reboot(),
  hw:display("Make Your Selection", []),
  process_flag(trap_exit, true), %% TODO: make some test on the termination/cleanup phase
  {ok, selection, []}.

selection({selection, What, Price}, _LoopData) ->
  hw:display("Please pay: ~w", [Price]),
  {next_state, payment, {What, Price, 0}};
selection({pay, Coin}, LoopData) ->
  hw:return_change(Coin),
  {next_state, selection, LoopData};
selection(_OtherMessage, LoopData) ->
  {next_state, selection, LoopData}.

payment({pay, Coin}, {What, Price, Paid}) when Paid + Coin < Price ->
  NewPaid = Paid + Coin,
  ToPay = Price - NewPaid,
  hw:display("Please pay: ~w", [ToPay]),
  {next_state, payment, {What, Price, Paid + Coin}};
payment({pay, Coin}, {_What, Price, Paid}) when Paid + Coin >= Price ->
  hw:display("Preparing Drink.", []),
  hw:return_change((Paid + Coin) - Price),
  hw:drop_cup(),
  hw:display("Remove Cup.", []),
  {next_state, remove, null};
payment(cancel, {_What, _Price, _Paid}) ->
  hw:display("Make Your Selection", []),
  {next_state, selection, null};
payment(_OtherMessage, {What, Price, Paid}) ->
  {next_state, payment, {What, Price, Paid}}.

remove(cup_removed, _LoopData) ->
  hw:display("Make Your Selection", []),
  {next_state, selection, null};
remove({pay, Coin}, _LoopData) ->
  hw:return_change(Coin),
  {next_state, remove, null};
remove(_OtherMessage, _LoopData) ->
  {next_state, remove, null}.

handle_event(stop, _State, LoopData) ->
  {stop, normal, LoopData}.

terminate(_Reason, payment, {_What, _Price, _Paid}) ->
    %% TODO: test with meck that we invoke HW for the remainder if
    %%       we are in PAYMENT state
    ok;
terminate(_Reason, _State, _LoopData) ->
    ok.

handle_info(_Info, _State, _LoopData) ->
    ok.

code_change(_OldVsn, _State, _LoopData, _Extra) ->
    ok.

handle_sync_event(_Event, _From, _State, _LoopData) ->
    ok.
