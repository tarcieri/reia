%
% reia_nodes: Nodes of the Reia parse tree
% Copyright (C)2009-10 Tony Arcieri
% 
% Redistribution is permitted under the MIT license.  See LICENSE for details.
%

% Terminals
-record(integer,     {line, value}).
-record(float,       {line, value}).
-record(var,         {line, name}).
-record(ivar,        {line, name}).
-record(bound_var,   {line, name}).
-record(string,      {line, characters}).
-record(dstring,     {line, elements}).
-record(regexp,      {line, pattern}).
-record(module_name, {line, name}).
-record(atom,        {line, name}).
-record(true,        {line}).
-record(false,       {line}).
-record(nil,         {line=1}).
-record(self,        {line}).

% Modules and Classes
-record(module,      {line, name, functions}).
-record(class,       {line, name, superclass='Object', methods}).

% Functions
-record(function,    {line, name, args=[], block=#var{line=1,name='_'}, body}).

% Function calls
-record(class_inst,  {line, class, args=[], block=#nil{}}).
-record(local_call,  {line, name,  args=[], block=#nil{}}).
-record(remote_call, {line, receiver, name,   args=[], block=#nil{}}).
-record(native_call, {line, module, function, args=[]}).
-record(var_call,    {line, receiver, args=[], block=#nil{}}).

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
-record(lambda,      {line, args=[], body}).
-record(block,       {line, exprs}).
-record(lc,          {line, expr, generators}).
-record(generate,    {line, pattern, source}).
-record(clause,      {line, patterns, exprs}).
-record('case',      {line, expr, clauses}).
-record('if',        {line, clauses}).
-record('receive',   {line, clauses, timeout=void}).
-record('after',     {line, length, exprs}).
-record(throw,       {line, type='RuntimeError', message}).
-record('try',       {line, body, clauses}).
-record('catch',     {line, pattern, body}).