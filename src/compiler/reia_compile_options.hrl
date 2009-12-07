% Global options for the Reia compiler.  Passed to every single transform.
% Transforms can use them for what they will.
-record(compile_options, {
  code          = undefined,
  scope         = toplevel,
  passes        = [ssa,r2e],
  autohipe      = false,
  erlc_options  = [debug_info, export_all, verbose, report_errors]
}).