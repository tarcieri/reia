%
% reia_funrefs: Funs which reference Reia functions and methods
% Copyright (C)2009 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

-module(reia_funrefs).
-export([forms/3]).

forms(Line, Receiver, Name) ->
  {'case', Line,
    Receiver,
    [
      {clause, Line,
        [{tuple, Line, [{atom, Line, constant}, {var, Line, '__module_name'}]}], [],
        [module(Line, {var, Line, '__module_name'}, Name)]
      },
      {clause, Line,
        [{tuple, Line, [{atom, Line, 'object'}, {tuple, Line, [{var, Line, '__pid'}, {var, Line, '_'}]}]}], [],
        [object(Line, {var, Line, '__pid'}, Name)]
      }
    ]
  }.

module(Line, Module, Name) ->
  {call, Line,
    {remote, Line, {atom, Line, erlang}, {atom, Line, make_fun}},
    [Module, {atom, Line, Name}, {integer, Line, 2}]
  }.
  
object(Line, Pid, Name) ->
  {'fun', Line, {clauses, [
    {clause, Line, [{var, Line, '__args'}, {var, Line, '__block'}], [],
      [{call, Line,
        {remote, Line, {atom, Line, reia_class}, {atom, Line, call}},
        [Pid, {tuple, Line, [
          {atom, Line, Name}, 
          {call, Line, {atom, Line, tuple_to_list}, [{var, Line, '__args'}]}, 
          {var, Line, '__block'}
        ]}]
      }]
    }
  ]}}.