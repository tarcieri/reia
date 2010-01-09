% Global options for the Reia compiler.  Passed to every single transform.
% Transforms can use them for what they will.
-record(compile_options, {
  % Original Reia parse tree, to be embedded as the code attribute of final modules
  code = undefined,
  
  % Input code is treated as being in the following scope
  scope = toplevel,
  
  % Reia compiler passes to run
  passes = [comparisons, conditionals, rebinding, ssa, r2e],
  
  % Automatically enable HiPE (i.e. JIT) support if available
  autohipe = false,
  
  % Wrap toplevel code in its own module
  toplevel_wrapper = true,
  
  % Options to pass along to the Erlang compiler
  erlc_options = [debug_info, export_all, verbose, report_errors]
}).