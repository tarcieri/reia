%
% reia_funrefs: Lambdas which reference Reia functions and methods
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_funrefs).
-export([forms/3]).

forms(Line, Receiver, Name) ->
  {'case', Line,
    Receiver,
    [{clause, Line,
      [{tuple, Line, [{atom, Line, constant}, {var, Line, '__module_name'}]}], [],
      [module(Line, {var, Line, '__module_name'}, Name)]
    }]
  }.

module(Line, Module, Name) ->
  {call, Line,
   {remote, Line, {atom, Line, erlang}, {atom, Line, make_fun}},
   [Module, {atom, Line, Name}, {integer, Line, 2}]
  }.