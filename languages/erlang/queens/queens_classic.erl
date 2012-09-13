-module(queens_classic).
-export([solve/0]).

solve() ->
    solve(lists:seq(1, 8), lists:seq(1, 15 + 15), 1, []).

print_board([]) -> io:format("~n", []);
print_board([H|T]) ->
    Line = [string:copies(". ", H - 1), "@ ", string:copies(". ", 8 - H)],
    io:format("~s~n", [Line]),
    print_board(T).

solve(_, _, Cols, Result) when Cols > 8 -> 
    io:format("~p~n", [Result]),
    print_board(Result);

solve(Rows, Diags, Col, Result) ->
    lists:foreach(fun(Row) ->
                     D1 = Row + Col,
                     D2 = Row - Col + 8 + 15,
                     T = lists:member(Row, Rows) andalso
                         lists:member(D1, Diags) andalso
                         lists:member(D2, Diags),
                     if T ->
                         Rows1 = Rows -- [Row],
                         Diags1 = Diags -- [D1, D2],
                         solve(Rows1, Diags1, Col + 1, [Row | Result]);
                        true -> void
                     end
                  end, Rows).
