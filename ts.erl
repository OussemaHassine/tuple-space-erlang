-module(ts).

-export([
    new/1,
    out/2,
    rd/2, rd/3,
    in/2, in/3,
    addNode/2, removeNode/2, nodes/1
]).

%%% =====================================================
%%% Public API
%%% =====================================================

%% Create a new Tuple Space with a name
new(Name) ->
    spawn(fun() ->
        loop(#{name => Name, tuples => [], nodes => []})
    end).

%% OUT: add a tuple
out(TS, Tuple) ->
    TS ! {out, Tuple},
    ok.

%% RD: blocking read (no removal)
rd(TS, MatchFun) ->
    TS ! {rd, MatchFun, self()},
    receive
        {ok, Tuple} ->
            Tuple
    end.

%% RD with timeout
rd(TS, MatchFun, Timeout) ->
    TS ! {rd, MatchFun, self()},
    receive
        {ok, Tuple} ->
            {ok, Tuple}
    after Timeout ->
        {err, timeout}
    end.

%% IN: blocking read + remove
in(TS, MatchFun) ->
    TS ! {in, MatchFun, self()},
    receive
        {ok, Tuple} ->
            Tuple
    end.

%% IN with timeout
in(TS, MatchFun, Timeout) ->
    TS ! {in, MatchFun, self()},
    receive
        {ok, Tuple} ->
            {ok, Tuple}
    after Timeout ->
        {err, timeout}
    end.

%% Node management
addNode(TS, Node) ->
    TS ! {add_node, Node},
    ok.

removeNode(TS, Node) ->
    TS ! {remove_node, Node},
    ok.

nodes(TS) ->
    TS ! {nodes, self()},
    receive
        {nodes, Ns} ->
            Ns
    end.

%%% =====================================================
%%% Internal Functions
%%% =====================================================

%% Main server loop
loop(State = #{tuples := Tuples, nodes := Nodes}) ->
    receive
        %% OUT
        {out, Tuple} ->
            loop(State#{tuples := [Tuple | Tuples]});

        %% RD (no removal)
        {rd, MatchFun, From} ->
            case find_match(MatchFun, Tuples) of
                {ok, T} ->
                    From ! {ok, T},
                    loop(State);
                not_found ->
                    loop(State)
            end;

        %% IN (remove tuple)
        {in, MatchFun, From} ->
            case find_and_remove(MatchFun, Tuples) of
                {ok, T, NewTuples} ->
                    From ! {ok, T},
                    loop(State#{tuples := NewTuples});
                not_found ->
                    loop(State)
            end;

        %% Node management
        {add_node, Node} ->
            loop(State#{nodes := lists:usort([Node | Nodes])});

        {remove_node, Node} ->
            loop(State#{nodes := lists:delete(Node, Nodes)});

        {nodes, From} ->
            From ! {nodes, Nodes},
            loop(State)
    end.

%% Find first matching tuple (rd)
find_match(_MatchFun, []) ->
    not_found;
find_match(MatchFun, [T | Ts]) ->
    case MatchFun(T) of
        true  -> {ok, T};
        false -> find_match(MatchFun, Ts)
    end.

%% Find and remove matching tuple (in)
find_and_remove(_MatchFun, []) ->
    not_found;
find_and_remove(MatchFun, [T | Ts]) ->
    case MatchFun(T) of
        true  -> {ok, T, Ts};
        false ->
            case find_and_remove(MatchFun, Ts) of
                {ok, Found, Rest} ->
                    {ok, Found, [T | Rest]};
                not_found ->
                    not_found
            end
    end.
