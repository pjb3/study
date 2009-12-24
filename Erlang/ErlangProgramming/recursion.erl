-module(recursion).
-compile(export_all).

bump([]) -> [];
bump([Head|Tail]) -> [ Head + 1 | bump(Tail) ].

avg([]) -> 0;
avg(List) -> sum(List) / len(List).

sum(List) -> sum_acc(List,0).

sum_acc([],Sum) -> Sum;
sum_acc([Head|Tail],Sum) -> sum_acc(Tail, Head + Sum).

len([]) -> 0;
len([_|Tail]) -> 1 + len(Tail).

evens([]) -> [];
evens([Head|Tail]) when Head rem 2 == 0 -> [ Head | evens(Tail)];
evens([_|Tail]) -> evens(Tail).

member(_, []) -> false;
member(Head, [Head|_]) -> true;
member(E, [_|Tail]) -> member(E, Tail).