% Modules
-record(module,     {line, name, functions}).

% Functions
-record(function,   {line, name, arguments, block={nil, 1}, body}).

% Terminals
-record(integer,    {line, value}).
-record(float,      {line, value}).
-record(identifier, {line, name}).
-record(string,     {line, characters}).
-record(dstring,    {line, elements}).
-record(regexp,     {line, pattern}).
-record(atom,       {line, name}).
-record(true,       {line}).
-record(false,      {line}).
-record(nil,        {line}).

% Function calls
-record(remote_call, {line, receiver, name, arguments, block}).
-record(native_call, {line, module, function, arguments}).

% Operators
-record(unary_op,    {line, type, val}).
-record(binary_op,   {line, type, left, right}).
-record(ternary_op,  {line, left, middle, right}).

% Other Expressions
-record(match,       {line, left, right}).
-record(cons,        {line, expr, tail}).
-record(empty,       {line}).
-record(tuple,       {line, elements}).
-record(binary,      {line, elements}).
-record(bin_element, {line, expression, size=default, type_list=default}).
-record(range,       {line, from, to}).
-record(dict,        {line, elements}).
-record(block,       {line, exprs}).
-record(clause,      {line, patterns, exprs}).
-record('case',      {line, expr, clauses}).
