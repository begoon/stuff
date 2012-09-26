#!/usr/bin/env escript

%% http://acmp.ru/index.asp?main=task&id_task=176

main(_) ->
  N = 50, K = 25,
  io:format("~w\n", [rbs(2*N, 0, 1, K)]).

rbs(0, 0, 0, _) -> 1;
rbs(0, _, _, _) -> 0;
rbs(_, K, _, _) when K < 0 -> 0;

rbs(N, K, 0, MAX_K) ->
  if 
    K < MAX_K-1 -> rbs_memo(N-1, K-1, 0, MAX_K) + rbs_memo(N-1, K+1, 0, MAX_K);
    K < MAX_K   -> rbs_memo(N-1, K-1, 0, MAX_K);
    K =:= MAX_K -> 0
  end;

rbs(N, K, 1, MAX_K) ->
  if
    K < MAX_K   -> rbs_memo(N-1, K-1, 1, MAX_K) + rbs_memo(N-1, K+1, 1, MAX_K);
    K =:= MAX_K -> rbs_memo(N-1, K-1, 0, MAX_K) + rbs_memo(N-1, K-1, 1, MAX_K)
  end.

rbs_memo(N, K, T, MAX_K) ->
  Name = rbs,
  case ets:info(Name) of
    undefined ->
      ets:new(Name, [public, named_table]);
    _ -> true
  end,
  Key = {N, K, T, MAX_K},
  case ets:lookup(Name, Key) of
    [] ->
      Val = rbs(N, K, T, MAX_K),
      ets:insert(Name, {Key, Val}),
      Val;
    [{_, Val}] -> Val;
    Else -> Else
  end.
