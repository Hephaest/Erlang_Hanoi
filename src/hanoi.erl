%% @author Miao Cai
%% Created : 11. April 2019 7:23 PM
%% @doc This module is used to display and solve the Hanoi solution step by step.

-module(hanoi).
-author("Miao Cai").

%% ==================================================================================================================
%% API functions
%% ==================================================================================================================
-export([create_towers/1, display_towers/1, move/3, solve/5]).

%% ==================================================================================================================
%% This function is used to return a representation of the three towers in the start state and
%% solve the puzzle. After finishing, print the end state.
%% ==================================================================================================================
create_towers(N)->
    % Sort the numbers from smallest to largest in the list by the tool module.
    OriginalStatus = [{tower1, tool:produce(N)},{tower2,[]},{tower3,[]}],
    % Sort the numbers from small to big in the list by the tool module.
    display_towers(OriginalStatus),
    % Start solving the puzzle.
    solve(N, tower1, tower2, tower3, OriginalStatus),
    % Print end state.
    io:format("-------------------Finish------------------~n").

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
%% This function is used to display a representation of the game state.
%% Then print it to the screen.
%% ==================================================================================================================
display_towers(T)->
    % Retrieve the lists from the representation of the game.
    [{_,Tower1List},{_,Tower2List},{_,Tower3List}] = T,
    io:format("-------------------Update------------------~n"),
    % Sort the lists' orders from largest number to smallest by the tool module.
    io:format("~s~s ~w~n",[tower1, ":", tool:reverse_tower(Tower1List)]),
    io:format("~s~s ~w~n",[tower2, ":", tool:reverse_tower(Tower2List)]),
    io:format("~s~s ~w~n",[tower3, ":", tool:reverse_tower(Tower3List)]).

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

