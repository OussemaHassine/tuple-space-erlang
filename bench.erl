-module(bench).
-export([
    bench_out/2,
    bench_rd/2,
    bench_in/2,
    bench_failure_recovery/1
]).

%% =====================================================
%% Measure average OUT time
%% =====================================================
bench_out(TS, N) ->
    Start = erlang:monotonic_time(microsecond),
    lists:foreach(
        fun(I) ->
            ts:out(TS, {I, value})
        end,
        lists:seq(1, N)
    ),
    End = erlang:monotonic_time(microsecond),
    Avg = (End - Start) / N,
    {avg_out_microseconds, Avg}.

%% =====================================================
%% Measure average RD time (non-blocking case)
%% =====================================================
bench_rd(TS, N) ->
    %% Ensure tuples exist
    lists:foreach(
        fun(I) ->
            ts:out(TS, {I, value})
        end,
        lists:seq(1, N)
    ),

    MatchFun = fun({_X, _}) -> true end,

    Start = erlang:monotonic_time(microsecond),
    lists:foreach(
        fun(_) ->
            ts:rd(TS, MatchFun)
        end,
        lists:seq(1, N)
    ),
    End = erlang:monotonic_time(microsecond),
    Avg = (End - Start) / N,
    {avg_rd_microseconds, Avg}.

%% =====================================================
%% Measure average IN time (removal)
%% =====================================================
bench_in(TS, N) ->
    %% Ensure tuples exist
    lists:foreach(
        fun(I) ->
            ts:out(TS, {I, value})
        end,
        lists:seq(1, N)
    ),

    MatchFun = fun({_X, _}) -> true end,

    Start = erlang:monotonic_time(microsecond),
    lists:foreach(
        fun(_) ->
            ts:in(TS, MatchFun)
        end,
        lists:seq(1, N)
    ),
    End = erlang:monotonic_time(microsecond),
    Avg = (End - Start) / N,
    {avg_in_microseconds, Avg}.

%% =====================================================
%% Measure recovery time after node failure
%% =====================================================
bench_failure_recovery(N) ->
    TS = ts:new(recovery_test),

    %% Writer process
    Writer =
        spawn(fun() ->
            lists:foreach(
                fun(I) ->
                    ts:out(TS, {I, value}),
                    timer:sleep(1)
                end,
                lists:seq(1, N)
            )
        end),

    %% Kill the writer
    timer:sleep(10),
    Start = erlang:monotonic_time(microsecond),
    exit(Writer, kill),

    %% Check when TS still answers
    MatchFun = fun({_X, _}) -> true end,
    ts:rd(TS, MatchFun),
    End = erlang:monotonic_time(microsecond),

    {recovery_time_microseconds, End - Start}.
