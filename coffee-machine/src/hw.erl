-module(hw).

-compile(export_all).

display(Str, Arg) ->
    io:format("Display: " ++ Str ++ "~n", Arg).

return_change(Payment) ->
    io:format("Machine: returned ~w in change~n", [Payment]).

drop_cup() ->
    io:format("Machine: dropped cup~n", []).

prepare(Type) ->
    io:format("Machine: preparing ~p~n", [Type]).

reboot() ->
    io:format("Machine: rebooted hardware~n", []).
