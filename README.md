# Hanoi
[![LICENSE](https://img.shields.io/cocoapods/l/AFNetworking.svg)](https://github.com/Hephaest/Hanoi/blob/master/LICENSE)
[![Dependencies](https://img.shields.io/badge/Dependencies-up%20to%20date-green.svg)](https://github.com/Hephaest/Hanoi/tree/master/src)

# Rules
We have a **pattern** to solving the 3-tower Hanoi:
1. Move all disks from tower 1 to tower 3
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
For instance, if the number is 5
After conversion, 
