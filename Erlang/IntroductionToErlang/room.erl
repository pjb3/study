-module(room).
-compile(export_all).

start() ->
  register(room,spawn(fun() -> loop([]) end)).

add_user(Pid, MyName) ->
  room ! {Pid, add, MyName}.

loop(Chatters) ->
  receive
    {From, broadcast, SenderName, Message} ->
      [ Member ! { accept, SenderName, Message } || Member <- Chatters,
                                                    Member =/= From ],
      loop(Chatters);

    {From, add, MyName } ->
      lists:foreach(fun(Member) ->
         Member ! { accept, "Server", MyName ++ " has joined" } end, Chatters ),
      loop([From|Chatters])
  end.