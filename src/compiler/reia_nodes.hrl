% Terminals
-record(integer, {line, value}).
-record(float,   {line, value}).

% Operators
-record(unary_op,  {line, type, val}).
-record(binary_op, {line, type, val1, val2}).

% Other Expressions
-record(cons,  {line, expr, tail}).
-record(tuple, {line, elements}).