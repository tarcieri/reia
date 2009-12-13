-define(reia_dispatch(Receiver, Line, Method, Args, Block),
  {call, Line,
    {remote, Line, {atom, Line, reia_dispatch}, {atom, Line, call}},
    [
      transform(Receiver),
      {atom, Line, Method},
      {tuple, Line, [transform(Arg) || Arg <- Args]},
      transform(Block)
    ]
  }
).