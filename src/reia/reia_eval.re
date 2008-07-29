#
# reia_eval: Methods for evaluating Reia expressions or abstract syntax
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
module ReiaEval
  def string(str)
    case reia_parse::string(str.to_list())
      (~ok, forms):
        (~value, value, _binding) = exprs(forms, new_binding())
        value
      error:
        error
    
  def exprs(forms, binding)
    erl_forms = reia_compiler::compile(forms)
    erl_eval::exprs(erl_forms, binding, (~value, fun(name, arguments) { local(name, arguments) }))
  
  def new_binding
    erl_eval::new_bindings()
  
  # Thunk local functions to ReiaKernel
  # FIXME: this is a rather inelegant solution and should be DRYed out
  def local(name, args)
    case name
      ~puts:
        args.each { |arg| reia_kernel::puts(arg) }
        nil
      ~print:
        reia_kernel::print(args[0])
      ~eval:
        reia_kernel::eval(args[0])
      ~load:
        reia_kernel::load(args[0])
      