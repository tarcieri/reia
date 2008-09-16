#
# Eval: Methods for evaluating Reia expressions or abstract syntax
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
module Eval
  def string(str)
    case reia_parse::string(str.to_list())
      (~ok, forms):
        (~value, value, _binding) = exprs(forms, new_binding())
        value
      error:
        throw error
    
  def exprs(forms, binding)
    erl_forms = reia_compiler::compile(forms, [(~ssa, binding), ~r2e, ~dynamic])
    (~value, result, binding) = erl_eval::exprs(erl_forms, binding, (~value, fun(name, arguments) { local(name, arguments) }))
    binding = binding.map { |(var, value)| (var.to_s().sub(/^~/, "").sub(/_[0-9]+$/, "").uncapitalize().to_atom(), value) }
    (~value, result, binding)
  
  def new_binding
    erl_eval::new_bindings()
  
  # Thunk local functions to Kernel
  # FIXME: this is a rather inelegant solution and should be DRYed out
  def local(name, args)
    case name
      ~puts:
        args.each { |arg| Local.puts(arg) }
        nil
      ~print:
        Local.print(args[0])
      ~eval:
        Local.eval(args[0])
      ~load:
        Local.load(args[0])
      