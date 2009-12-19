% Modules
-record(module,     {line, name, functions}).

% Functions
-record(function,   {line, name, arguments, block={nil, 1}, body}).

% Terminals
-record(integer,    {line, value}).
-record(float,      {line, value}).
-record(identifier, {line, name}).
-record(string,     {line, characters}).
-record(regexp,     {line, pattern}).
-record(atom,       {line, name}).
-record(range,      {line, from, to}).
-record(true,       {line}).
-record(false,      {line}).
-record(nil,        {line}).

% Function calls
-record(remote_call, {line, receiver, name, arguments, block}).
-record(native_call, {line, module, function, arguments}).

% Operators
-record(unary_op,  {line, type, val}).
-record(binary_op, {line, type, left, right}).

% Other Expressions
-record(match, {line, left, right}).
-record(cons,  {line, expr, tail}).
-record(empty, {line}).
-record(tuple, {line, elements}).
-record(dict,  {line, elements}).
-record(block, {line, exprs}).