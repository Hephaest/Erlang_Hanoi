Table of Contents
=================
   * [Hanoi](#hanoi)
   * [Rules](#rules)
   * [Basic Algorithm](#basic-algorithm)
   * [Erlang Program For Hanoi](#erlang-program-for-hanoi)

# Hanoi
[![LICENSE](https://img.shields.io/cocoapods/l/AFNetworking.svg)](https://github.com/Hephaest/Hanoi/blob/master/LICENSE)
[![Dependencies](https://img.shields.io/badge/Dependencies-up%20to%20date-green.svg)](https://github.com/Hephaest/Hanoi/tree/master/src)

English | [中文](README_CN.md)

Last updated on `2019/07/09`

# Rules
We have **rules** to specify the 3-tower Hanoi issue:
1. Move all disks from tower 1 to tower 3.
2. A disk may only reside on top of a larger disk.
3. To move any disk, we must first move all the smaller disks off the top of it.

# Basic Algorithm
According to the rules, the algorithm can be written as follows:

**Step 1**: Move above N-1 disks from initial peg to auxiliary peg.

**Step 2**: Move itself from initial peg to end peg.

**Step 3**: Move above N-1 disks from auxiliary peg to end peg.<br>
(Think about this process as **recursion**)

# Erlang Program For Hanoi
In Erlang, we can store the disks of each tower as [{tower1, [5,4,3,2,1]},{tower2,[]},{tower3,[]}].

The only thing the user needs to do is give a total number of disks. This number will be converted into ascending order of the list.

This function is shown as follows:
```Erlang
produce(1) -> [1]; % If the current number is 1, than return [1] is fine.
produce(N) ->
    produce(N-1) ++ [N]. % Put the rest of the numbers ahead of the current number.
```
For instance, if the number is 5, the result after `produce()` function is [1,2,3,4,5]. This is not what we want.<br>
Somebody may suggests that we can change the statement `produce(N-1) ++ [N]` into `[N] ++ produce(N-1)`. Yep, this can work.
However, in fact, each time we my move the top disk of the tower, if the top disk is 5 (after reversion) that will be wrong.
Hence, the `reverse()` function is an alternative method which will be invoked if necessary.

This function is shown as follows:
```Erlang
reverse_tower([H|T],M)->
    reverse_tower(T,[H|M]); % Add a new head to the accumulator and the list will automatically be reversed.
reverse_tower([],M)-> M. % If the element of the list is empty then the reverse operation is done.
reverse_tower(T)->
    reverse_tower(T,[]). % Initialize the accumulator.
```
After conversion, we can start solving this issue by the above algorithm.

The algorithm has been implemented by the following function:
```Erlang
%% ==================================================================================================================
%% This function is used to return a representation of the towers and the disk which is required to be moved.
%% ==================================================================================================================
check_status(T, Start, End)->
    % Use _ variable as a wildcard for pattern matching.
    [{_,Tower1List},{_,Tower2List},{_,Tower3List}] = T,
    Status = if Start == tower1, End == tower2 -> % Add a new head to the tower 2 and discard a top disk from tower 1.
                Number = hd(Tower1List), [{tower1,tl(Tower1List)},{tower2,[Number|Tower2List]},{tower3,Tower3List}];
              Start == tower1, End == tower3 -> % Add a new head to the tower 3 and discard a top disk from tower 1.
                Number = hd(Tower1List), [{tower1,tl(Tower1List)},{tower2,Tower2List},{tower3,[Number|Tower3List]}];
              Start == tower2, End == tower1 -> % Add a new head to the tower 1 and discard a top disk from tower 2.
                Number = hd(Tower2List), [{tower1,[Number|Tower1List]},{tower2,tl(Tower2List)},{tower3,Tower3List}];
              Start == tower2, End == tower3 -> % Add a new head to the tower 3 and discard a top disk from tower 2.
                Number = hd(Tower2List), [{tower1,Tower1List},{tower2,tl(Tower2List)},{tower3,[Number|Tower3List]}];
              Start == tower3, End == tower1 -> % Add a new head to the tower 1 and discard a top disk from tower 3.
                Number = hd(Tower3List), [{tower1,[Number|Tower1List]},{tower2, Tower2List},{tower3,tl(Tower3List)}];
              % Add a new head to the tower 2 and discard a top disk from tower 3.
              true -> Number = hd(Tower3List), [{tower1,Tower1List},{tower2, [Number|Tower2List]},{tower3,tl(Tower3List)}]
            end,
    {Number, Status}. % Return a tuple combined with a number and a new presentation of the three towers.

%% ==================================================================================================================
%% This function is used to move the disk from one tower to another.
%% After finishing, refresh the representation of the game.
%% ==================================================================================================================
move(T, Start, End)->
    % Get the current disk which is required to move and then generate the new representation of the three towers.
    {Number, NewStatus} = tool:check_status(T, Start, End),
    io:format("-------------------------------------------~nMove No.~p disk: ~p -------> ~p ~n", [Number, Start, End]),
    % Display new representation of the three towers.
    display_towers(NewStatus),
    % Return the new representation so that solve function could track the current state.
    NewStatus.

%% ==================================================================================================================
%% This function is used to solve the puzzle by tail recursion.
%% ==================================================================================================================
solve(1, Init, Aux, Dest, T)-> move(T, Init, Dest); % The smallest disk could be easily moved to destination tower.
solve(N, Init, Aux, Dest, T) when N > 1->
    % Move above N-1 disks from initial tower to auxiliary tower.
    ResetStatus = solve(N-1, Init, Dest, Aux, T),
    % Move itself from initial tower to destination tower.
    ResetStatusAgain = move(ResetStatus, Init, Dest),
    % Move above N-1 disks from auxiliary tower to destination tower.
    solve(N-1, Aux, Init, Dest, ResetStatusAgain).
```
