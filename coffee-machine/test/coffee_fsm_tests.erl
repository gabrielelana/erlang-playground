-module(coffee_fsm_tests).

-include_lib("eunit/include/eunit.hrl").


mocking(Mod, F) ->
  mocking(Mod, [], F).

mocking(Mod, Options, F) ->
  meck:new(Mod, Options),
  F(),
  ?assert(meck:validate(Mod)),
  meck:unload(Mod).

spy_on(Mod, F, Arity) ->
  meck:expect(Mod, F, Arity, nil),
  {Mod, F, Arity}.

was_called_once_with(SpiedOn, Args) ->
  was_called_with(SpiedOn, [Args]).

was_called_with({Mod, F, Arity}, ArgsPerCall) ->
  lists:foldl(
    fun(Args, CountOfCall) ->
      lists:foldl(
        fun(Arg, CountOfArg) ->
          ?assertEqual(Arg, meck:capture(CountOfCall, Mod, F, Arity, CountOfArg)),
          CountOfArg + 1
        end,
        1, Args
      ),
      CountOfCall + 1
    end,
    1, ArgsPerCall
  ).

was_never_called({Mod, F, Arity}) ->
  ?assertEqual(0, meck:num_calls(Mod, F, Arity));
was_never_called(Mod) ->
  ?assertEqual(0, length(meck:history(Mod))).

when_in_selection_state_and_user_makes_a_selection_next_state_is_payment_test() ->
  What = something,
  Price = 100,
  mocking(hw,
    fun() ->
      Display = spy_on(hw, display, 2),
      ?assertMatch(
         {next_state, payment, {What, Price, 0}},
         coffee_fsm:selection({selection, What, Price}, [])
      ),
      was_called_once_with(Display, ["Please pay: ~w", [Price]])
    end
  ).

when_in_selection_state_and_user_insert_coin_we_remain_in_selection_state_test() ->
  Coin = 100,
  mocking(hw,
    fun() ->
      Change = spy_on(hw, return_change, 1),
      ?assertMatch(
         {next_state, selection, []},
         coffee_fsm:selection({pay, Coin}, [])
      ),
      was_called_once_with(Change, [Coin])
    end
  ).

when_in_selection_state_other_events_are_ignored_test() ->
  mocking(hw, [passthrough],
    fun() ->
      ?assertMatch({next_state, selection, []}, coffee_fsm:selection(cancel, [])),
      ?assertMatch({next_state, selection, []}, coffee_fsm:selection(cup_removed, [])),
      was_never_called(hw)
    end
  ).


% when_in_payment_state_user_inserts_not_enough_coins_we_are_still_in_payment_state_test() ->
%     What = something,
%     Price = 100,
%     Payed = 0,
%     Inserted = 10,
%     ExpectedToBeInserted = Payed + Inserted,
%     meck:new(hw),
%     meck:expect(
%       hw,
%       display,
%       fun(Msg, Args) ->
%               ?assertEqual("Please pay: ~w", Msg),
%               ?assertEqual([90], Args)
%       end
%     ),
%     ?assertMatch(
%        {next_state, payment, {What, Price, ExpectedToBeInserted}},
%        ?MODULE:payment({pay, Inserted}, {What, Price, Payed})
%     ),
%     ?assert(meck:validate(hw)),
%     ?assertEqual(1, meck:num_calls(hw, display, 2)),
%     meck:unload(hw).

% when_in_payment_state_user_inserts_enough_coins_we_go_into_the_remove_state_test() ->
%     What = something,
%     Price = 100,
%     Payed = 0,
%     Inserted = 200,
%     meck:new(hw),
%     meck:expect(
%       hw,
%       display,
%       fun(Msg, _Args) ->
%               %% not a good assertion. But I can't check order of calls when
%               %% defining an expectation. We can check better in the history
%               %% of the mock after the calls have been made
%               ?assert((Msg =:= "Preparing Drink.") or (Msg =:= "Remove Cup."))
%       end
%     ),
%     meck:expect(
%       hw,
%       return_change,
%       fun(Coin) ->
%               ?assertEqual(100, Coin)
%       end
%     ),
%     meck:expect(hw, drop_cup, 0, ok),
%     ?assertMatch(
%        {next_state, remove, null},
%        ?MODULE:payment({pay, Inserted}, {What, Price, Payed})
%     ),
%     %% History = meck:history(hw),
%     %% lists:foreach(fun(Elem) -> io:format("~p~n", [Elem]) end, History),
%     %%[{<0.1931.0>,{hw,display,["Preparing Drink.",[]]},ok},
%     %% {<0.1931.0>,{hw,return_change,"d"},ok},
%     %% {<0.1931.0>,{hw,drop_cup,[]},ok},
%     %% {<0.1931.0>,{hw,display,[[...]|...]},ok}]])
%     %% History seems a good place for finding informations about the
%     %% order of the calls. At the moment this aspect is not tested
%     ?assert(meck:validate(hw)),
%     ?assertEqual(2, meck:num_calls(hw, display, 2)),
%     ?assertEqual(1, meck:num_calls(hw, return_change, 1)),
%     ?assertEqual(1, meck:num_calls(hw, drop_cup, 0)),
%     %% let's try capture/5 (this is better bacause we can understand the sequence of the calls)
%     ?assertEqual("Preparing Drink.", meck:capture(first, hw, display, '_', 1)),
%     ?assertEqual("Remove Cup.", meck:capture(last, hw, display, '_', 1)),
%     %% but we have to check by calling Args and this is not the easiest way.
%     %% for a perfect validation of the sequence it would be great to use history
%     %% beacuse we have to display "remove drink" only after the cup has been dropped
%     meck:unload(hw).

% when_in_payment_state_user_push_cancel_machine_returns_to_selection_state_test() ->
%     meck:new(hw),
%     meck:expect(
%       hw,
%       display,
%       fun(Msg, Args) ->
%               ?assertEqual("Make Your Selection", Msg),
%               ?assertEqual([], Args)
%       end
%     ),
%     ?assertMatch(
%        {next_state, selection, null},
%        ?MODULE:payment(cancel, {something_to_drink, 100, 0})
%     ),
%     ?assertEqual(1, meck:num_calls(hw, display, 2)),
%     meck:unload(hw).

% when_in_payment_state_other_events_are_ignored_test() ->
%     What = something_to_drink,
%     Price = 100,
%     Inserted = 10,
%     meck:new(hw, [passthrough]),
%     ?assertMatch(
%        {next_state, payment, {What, Price, Inserted}},
%        ?MODULE:payment(cup_removed, {What, Price, Inserted})
%     ),
%     ?assertMatch(
%        {next_state, payment, {What, Price, Inserted}},
%        ?MODULE:payment({selection, {something_other, 300, 0}}, {What, Price, Inserted})
%     ),
%     ?assertEqual(0, meck:num_calls(hw, display, '_')),
%     ?assertEqual(0, meck:num_calls(hw, return_change, '_')),
%     ?assertEqual(0, meck:num_calls(hw, drop_cup, '_')),
%     ?assertEqual(0, meck:num_calls(hw, reboot, '_')),
%     ?assertEqual(0, meck:num_calls(hw, prepare, '_')),
%     meck:unload(hw).

% when_in_remove_state_after_cup_removed_event_machine_returns_in_selection_test() ->
%     meck:new(hw),
%     meck:expect(hw, display, fun(Msg, _Args) -> ?assertEqual("Make Your Selection", Msg) end),
%     ?assertMatch(
%        {next_state, selection, null},
%        ?MODULE:remove(cup_removed, null)
%     ),
%     ?assertEqual(1, meck:num_calls(hw, display, 2)),
%     meck:unload(hw).

% when_in_remove_state_user_inserts_coin_machine_is_still_in_remove_state_test() ->
%     meck:new(hw),
%     meck:expect(hw, return_change, fun(Coin) -> ?assertEqual(10, Coin) end),
%     ?assertMatch(
%        {next_state, remove, null},
%        ?MODULE:remove({pay, 10}, null)
%     ),
%     ?assertEqual(1, meck:num_calls(hw, return_change, 1)),
%     meck:unload(hw).

% when_in_remove_state_other_events_are_ignored_test() ->
%     meck:new(hw, [passthrough]),
%     ?assertMatch(
%        {next_state, remove, null},
%        ?MODULE:remove({selection, {something_to_drink, 100, 0}}, null)
%     ),
%     ?assertMatch(
%        {next_state, remove, null},
%        ?MODULE:remove(cancel, null)
%     ),
%     ?assertEqual(0, meck:num_calls(hw, display, '_')),
%     ?assertEqual(0, meck:num_calls(hw, return_change, '_')),
%     ?assertEqual(0, meck:num_calls(hw, drop_cup, '_')),
%     ?assertEqual(0, meck:num_calls(hw, reboot, '_')),
%     ?assertEqual(0, meck:num_calls(hw, prepare, '_')),
%     meck:unload(hw).

% %%when_fsm_is_going_down_change_is_returned_test() ->
% %%    meck:new(hw, [passthrough]),
% %%    meck:expect(hw, return_change, fun(Coin) -> ?assertEqual(50, Coin) end),
% %%    ?MODULE:start_link(),
% %%    ?MODULE:americano(),
% %%    ?MODULE:pay(50),
% %%    ?MODULE:stop(),
% %%    ?assertEqual(1, meck:num_calls(hw, return_change, 1)),
% %%    meck:unload(hw).
