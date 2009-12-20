%
% Binary: Methods of the Binary builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Binary').
-export([call/4]).
-include("../core/reia_types.hrl").

call(Binary, to_s, _Args, _Block) ->
  List = binary_to_list(Binary),
  case io_lib:char_list(List) of
    true  -> "$[\"" ++ List ++ "\"]";
    false -> "$[" ++ reia_dispatch:call(List, join, {#reia_string{members=[","]}}, nil) ++ "]"
  end.