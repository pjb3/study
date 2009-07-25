-module(trivial_process).
-export([start/0]).

start() -> 
  spawn(fun() -> loop() end).

loop() ->
  receive
    Any ->
      io:format("~nI got the message: ~p~n",[Any]),
      loop()
  end.