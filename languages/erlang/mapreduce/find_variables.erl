-module(find_variables).
-export([main/0, find_variables_in_file/2, process_found_variables/3]).

-define(PATH, "/Users/alexander/Perforce/temenos/depot/TAFC/PATCH/").
-define(MASK, "").

main() ->
    io:format("Creating list of files...~n", []),
    Files = filelib:fold_files(?PATH, "\\..*(cpp)", true, 
                               fun(N, A) -> [N | A] end, []),
    io:format("Found ~b file(s)~n", [length(Files)]),
    F1 = fun find_variables_in_file/2,
    F2 = fun process_found_variables/3,
    benchmark(fun() -> 
        L = phofs:mapreduce(F1, F2, [], Files),
        io:format("Found ~b variable(s)~n", [length(L)])
    end, "MapReduce").

benchmark(Worker, Title) ->
    {T, _} = timer:tc(fun() -> Worker() end),
    io:format("~s: ~f sec(s)~n", [Title, T/1000000]).

-define(REGEXP, "(JBASEgetenv|getenv|ContextGetVariable)"
                "\s*\\(\s*\"([^\"]+)\"\s*\\)").

find_variables_in_file(Pid, FileName) ->
    case file:open(FileName, [read]) of 
        {ok, File} ->
            {_, RE} = {0, 0}, %re:compile(?REGEXP),
            CallBack = fun(Var) -> Pid ! {Var, 1} end,
            find_variable_in_file(File, RE, CallBack),
            file:close(File);
        {error, Reason} ->
            io:format("Unable to process '~s', ~p~n", [FileName, Reason]),
            exit(1)
    end.

process_found_variables(Key, Vals, A) ->
    [{Key, length(Vals)} | A].

find_variable_in_file(File, RE, CallBack) ->
    case io:get_line(File, "") of
       eof -> void;
       Line -> 
         scan_line_in_file(Line, RE, CallBack),
         find_variable_in_file(File, RE, CallBack)
    end.

scan_line_in_file(Line, RE, CallBack) ->
    void.

scan_line_in_file0(Line, RE, CallBack) ->
    case re:run(Line, RE) of
        {match, Captured} -> 
            [_, _, {NameP, NameL}] = Captured,
            Name = string:substr(Line, NameP + 1, NameL),
            CallBack(Name);
        nomatch -> void
    end.
