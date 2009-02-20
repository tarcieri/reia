%
% Exception: Methods for the Exception pseudo-class
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Exception').
-export([funcall/3]).

funcall({exception, {_Class, Reason}}, inspect, []) ->
  reia_string:from_list(io_lib:format("~p", [Reason]));
funcall(Constant, to_s, []) ->
  funcall(Constant, inspect, []).