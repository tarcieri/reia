% Global options for the Reia compiler.  Passed to every single transform.
% Transforms can use them for what they will.
-record(compile_options, {
  scope         = toplevel,
  passes        = [r2e],
  toplevel_args = [],
  erlc_options  = [debug_info, export_all, verbose, report_errors]
}).