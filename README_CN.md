目录
=================
   * [汉诺塔](#汉诺塔)
   * [基本规则](#基本规则)
   * [基本算法](#基本算法)
   * [汉诺塔的 Erlang 源码](#汉诺塔的-Erlang-源码)

# 汉诺塔
[![LICENSE](https://img.shields.io/cocoapods/l/AFNetworking.svg)](https://github.com/Hephaest/Hanoi/blob/master/LICENSE)
[![Dependencies](https://img.shields.io/badge/Dependencies-up%20to%20date-green.svg)](https://github.com/Hephaest/Hanoi/tree/master/src)

[English](README.md) | 中文

最后一次更新于 `2019/07/09`

# 基本规则
对于只有三个塔的汉诺塔问题我们有个基本**规则**：
1. 将所有圆盘从塔1转移到塔3。
2. 小圆盘只能放在大圆盘上面。
3. 如果想移动某个特定的圆盘，必须先把其上的所有圆盘移走。

# 基本算法
根据以上规则，汉诺塔的算法可以写成以下几个步骤：

**第一步**: 将 N-1 个圆盘从初始塔移动到中间塔。

**第二步**: 再将该圆盘从初始塔移动到目的塔。

**第三步**: 再将剩下的 N-1 个圆盘从中间塔移动到目的塔。<br>
(对每个圆盘都进行上述操作那么这个思路可以看作是**递归**)

# 汉诺塔的 Erlang 源码
在 Erlang 中，我们可以将不同塔中的圆盘表达成 [{tower1, [5,4,3,2,1]},{tower2,[]},{tower3,[]}] 的形式。

用户唯一需要做的事情就是传递初始的圆盘总数。这个数字将被列表转换成升序的数字形式。

此函数如下所示：
```Erlang
produce(1) -> [1]; % 如果当前数字为1，就直接返回1，递归结束。
produce(N) ->
    produce(N-1) ++ [N]. % 将为递归的数值放于当前数值之前。
```
举个例子，如果总数为5，经过 `produce()` 函数运行后的结果为[1,2,3,4,5]。但这不是我们想要的结果。<br>
有人可能会提议把 `produce(N-1) ++ [N]` 语句掉换成 `[N] ++ produce(N-1)` 不就行了吗。是的，如果按当前的要求看效果已经达到了。
然而实际上，我们每次都会移动塔顶最上方的圆盘，它的值一定是列表中最小的。如果在这里使用降序排列的话我们获取到的数值就会是5而不是1。
因此，`reverse()` 函数应该作为一个额外调用的方法来写。

此函数如下所示：
```Erlang
reverse_tower([H|T],M)->
    reverse_tower(T,[H|M]); % 将新头圆盘添加到列表中，列表将自动反转。
reverse_tower([],M)-> M. % 如果列表为空说明所有圆盘已排好序。
reverse_tower(T)->
    reverse_tower(T,[]). % 初始列表。
```
在列表转换之后，我们可以开始通过上述算法解决问题。

该算法通过以下函数实现：
```Erlang
%% ==================================================================================================================
%% 此函数用于返回不同塔的状态和需要移动的圆盘。
%% ==================================================================================================================
check_status(T, Start, End)->
    % 使用 _ 变量在模式匹配中作为通配符。
    [{_,Tower1List},{_,Tower2List},{_,Tower3List}] = T,
    Status = if Start == tower1, End == tower2 -> % 添加新的圆盘到塔2并丢掉塔1最上方的圆盘。
                Number = hd(Tower1List), [{tower1,tl(Tower1List)},{tower2,[Number|Tower2List]},{tower3,Tower3List}];
              Start == tower1, End == tower3 -> % 添加新的圆盘到塔3并丢掉塔1最上方的圆盘。
                Number = hd(Tower1List), [{tower1,tl(Tower1List)},{tower2,Tower2List},{tower3,[Number|Tower3List]}];
              Start == tower2, End == tower1 -> % 添加新的圆盘到塔1并丢掉塔2最上方的圆盘。
                Number = hd(Tower2List), [{tower1,[Number|Tower1List]},{tower2,tl(Tower2List)},{tower3,Tower3List}];
              Start == tower2, End == tower3 -> % 添加新的圆盘到塔3并丢掉塔2最上方的圆盘。
                Number = hd(Tower2List), [{tower1,Tower1List},{tower2,tl(Tower2List)},{tower3,[Number|Tower3List]}];
              Start == tower3, End == tower1 -> % 添加新的圆盘到塔1并丢掉塔3最上方的圆盘。
                Number = hd(Tower3List), [{tower1,[Number|Tower1List]},{tower2, Tower2List},{tower3,tl(Tower3List)}];
              % 添加新的圆盘到塔2并丢掉塔3最上方的圆盘。
              true -> Number = hd(Tower3List), [{tower1,Tower1List},{tower2, [Number|Tower2List]},{tower3,tl(Tower3List)}]
            end,
    {Number, Status}. % 返回一个包含当前移动圆盘的值和新的状态标识的元组。

%% ==================================================================================================================
%% 该函数用于将圆盘从起始塔转移到目的塔。
%% 完成移动后，更新状态的表示。
%% ==================================================================================================================
move(T, Start, End)->
    % 获得被移动圆盘的值和新的状态表示。
    {Number, NewStatus} = tool:check_status(T, Start, End),
    io:format("-------------------------------------------~nMove No.~p disk: ~p -------> ~p ~n", [Number, Start, End]),
    % 打印出新的状态表示。
    display_towers(NewStatus),
    % 返回新的状态。
    NewStatus.

%% ==================================================================================================================
%% 该函数使用尾递归解决圆盘转移问题。
%% ==================================================================================================================
solve(1, Init, Aux, Dest, T)-> move(T, Init, Dest); % 最上方的圆盘可以直接移动。
solve(N, Init, Aux, Dest, T) when N > 1->
    % 将 N-1 个圆盘从初始塔移动到中间塔。
    ResetStatus = solve(N-1, Init, Dest, Aux, T),
    % 再将该圆盘从初始塔移动到目的塔。
    ResetStatusAgain = move(ResetStatus, Init, Dest),
    % 再将剩下的 N-1 个圆盘从中间塔移动到目的塔
    solve(N-1, Aux, Init, Dest, ResetStatusAgain).
```
