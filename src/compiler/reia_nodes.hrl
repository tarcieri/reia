% Modules
-record(module,     {line, name, functions}).

% Functions
-record(function,   {line, name, arguments, block, body}).

% Terminals
-record(integer,    {line, value}).
-record(float,      {line, value}).
-record(identifier, {line, name}).
-record(atom,       {line, name}).
-record(true,       {line}).
-record(false,      {line}).
-record(nil,        {line}).

% Function calls
-record(remote_call, {line, receiver, name, arguments, block}).
-record(native_call, {line, module, function, arguments}).

% Operators
-record(unary_op,  {line, type, val}).
-record(binary_op, {line, type, val1, val2}).

% Other Expressions
-record(match, {line, left, right}).
-record(cons,  {line, expr, tail}).
-record(empty, {line}).
-record(tuple, {line, elements}).
-record(map,   {line, elements}).