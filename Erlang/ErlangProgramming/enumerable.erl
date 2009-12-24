-module(enumerable).
-compile(export_all).

all([], Pred) when is_function(Pred, 1) -> true;
all([Head|Tail], Pred) ->
  case Pred(Head) of
    true  -> all(Pred, Tail);
    false -> false
  end.

any([], Pred) when is_function(Pred, 1) -> false;
any([Head|Tail], Pred) ->  
  case Pred(Head) of
    true  -> true;
    false -> any(Tail, Pred)
  end.

collect(List, Func) -> map(List, Func).

detect(List, Func) -> find(List, Func).

each([], Func) when is_function(Func, 1) -> ok;
each([Head|Tail], Func) -> 
  Func(Head), 
  each(Tail, Func).

each_with_index(List, Func) -> each_with_index(List, Func, 0).

each_with_index([Head|Tail], Func, Index) ->
  Func(Head, Index),
  each_with_index(Tail, Func, Index + 1);
each_with_index([], Func, Index) when is_function(Func, 2) and is_number(Index) -> ok.

entries(List) -> List.

find([], Pred) when is_function(Pred, 1) -> undefined;
find([Head|Tail], Pred) ->
  case Pred(Head) of
    true  -> Head;
    false -> find(Tail, Pred)
  end.

find_all([], Pred) when is_function(Pred, 1) -> [];
find_all([Head|Tail], Func) ->
  case Func(Head) of
    true  -> find_all(Tail, Func);
    false -> [ Head | find_all(Tail, Func) ]
  end.

include([], _) -> false;
include([Head|Tail], Elem) ->
  case Head == Elem of
    true  -> true;
    false -> include(Tail, Elem)
  end.

inject([], _) -> [];
inject([Head|Tail], Func) -> inject(Tail, Func, Head).

inject([], Func, Initial) when is_function(Func, 2) -> Initial;
inject([Head|Tail], Func, Initial) -> inject(Tail, Func, Func(Initial, Head)).

map([], Func) when is_function(Func, 1) -> [];
map([Head|Tail], Func) -> [ Func(Head) | map(Tail, Func) ].

select(List, Func) -> find_all(List, Func).


  