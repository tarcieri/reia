% Modules
-record(module,      {line, name, functions}).

% Functions
-record(function,    {line, name, args, block={nil, 1}, body}).

% Terminals
-record(integer,     {line, value}).
-record(float,       {line, value}).
-record(identifier,  {line, name}).
-record(bound_var,   {line, name}).
-record(string,      {line, characters}).
-record(dstring,     {line, elements}).
-record(regexp,      {line, pattern}).
-record(module_name, {line, name}).
-record(atom,        {line, name}).
-record(true,        {line}).
-record(false,       {line}).
-record(nil,         {line}).

% Function calls
-record(local_call,  {line, name, args=[], block={nil, 1}}).
-record(remote_call, {line, receiver, name, args=[], block={nil, 1}}).
-record(native_call, {line, module, function, args=[]}).

% Operators
-record(unary_op,    {line, type, expr}).
-record(binary_op,   {line, type, left, right}).

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
-record('if',        {line, clauses}).
