% Macro for invoking a method on a given receiver
-define(invoke(Receiver, Method, Arguments, Block), reia_dispatch:call(Receiver, Method, Arguments, Block)).