-ifdef(VERBOSE).
-define(PRINT(Format, Terms), io:format(Format,Terms)).
-else.
-define(PRINT(Format, Terms), ok).
-endif.

-define(U(X), unicode:characters_to_binary(X)).


-record(ircmsg, {prefix = <<>>, 
                 command = <<>>, 
                 arguments = [], 
                 tail = <<>>}).

-record(serverconfig, {name="unnamed",
                       hostname,
                       port=6667,
                       nick="dingd1ng",
                       channels=[],
                       modules=[],
                       dbpid
                      }).

-record(channelconfig, {name="#channel",
                        modules=[]}).

