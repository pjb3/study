-module(user).
-compile(export_all).

create(MyName) ->
  Pid = spawn(fun() -> loop(MyName) end),
  room:add_user(Pid,MyName), Pid.

loop(MyName) ->
  receive
    { accept, SenderName, Message } ->
      io:format("~n~s receives from ~s: ~s ~n", [MyName, SenderName, Message]),
      loop(MyName);

    { say, Message } ->
      io:format("~n~s says ~s ~n",[MyName, Message]),
      room ! { self(), broadcast, MyName, Message },
      loop(MyName)

  end.