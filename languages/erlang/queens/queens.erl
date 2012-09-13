-module(queens).
-export([solve/0]).

solve() ->
    solve(1, []).

print_board([]) -> io:format("~n", []);
print_board([{_X, Y}|T]) ->
    Line = [string:copies(". ", Y - 1), "@ ", string:copies(". ", 8 - Y)],
    io:format("~s~n", [Line]),
    print_board(T).

solve(X, Taken) when X > 8 ->
    io:format("~p~n", [Taken]),
    print_board(Taken);

solve(X, Taken) ->
    L = [{X, Y} || Y <- lists:seq(1, 8), not under_attack({X, Y}, Taken)],
    row(L, Taken).

row([], _) -> [];
row([{X, Y}|L], Taken) ->
    solve(X + 1, [{X, Y} | Taken]),
    row(L, Taken).

under_attack(_, []) -> false;
under_attack({X, Y}, [{Xt, Yt}|L]) ->
    Y == Yt orelse abs(Y - Yt) == abs(X - Xt) orelse under_attack({X, Y}, L).
