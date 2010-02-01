%
% Binary: Methods of the Binary builtin
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module('Binary').
-export([call/4]).
-include("../core/reia_types.hrl").
-include("../core/reia_invoke.hrl").

call(Binary, to_string, _Args, _Block) ->
  #reia_string{elements=[Binary]};

call(Binary, to_s, _Args, _Block) ->
  #reia_string{elements=[Binary]};
  
call(Binary, inspect, _Args, _Block) ->
  List = binary_to_list(Binary),
  Elements = ?invoke(?invoke(List, join, {#reia_string{elements=[","]}}, nil), to_list, {}, nil),
  ?invoke("<[" ++ Elements ++ "]>", to_string, {}, nil).