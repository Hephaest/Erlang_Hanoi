%% @author Hephaest
%% @doc This module is used to provide fundamental methods to handle list without using other existing modules.
%% Created : 13. Apr 2019 3:34 AM
-module(tool).
-author("Hephaest").

%% ==================================================================================================================
%% API functions
%% ==================================================================================================================
-export([produce/1, reverse_tower/1,reverse_tower/2, check_status/3]).

%% ==================================================================================================================
%% This function is used to return a representation of list from smallest to largest by recursion algorithm.
%% ==================================================================================================================
produce(1) -> [1]; % If the current number is 1, than return [1] is fine.
produce(N) ->
    produce(N-1) ++ [N]. % Put the rest of the numbers ahead of the current number.

%% ==================================================================================================================
%% This function is used to reverse a list from largest to smallest by recursion algorithm.
%% ==================================================================================================================
reverse_tower([H|T],M)->
    reverse_tower(T,[H|M]); % Add a new head to the accumulator and the list will automatically be reversed.
reverse_tower([],M)-> M. % If the element of the list is empty then the reverse operation is done.
reverse_tower(T)->
    reverse_tower(T,[]). % Initialize the accumulator.

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

