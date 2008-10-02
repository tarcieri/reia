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
    # Compile Reia forms to Erlang forms
    erl_forms = reia_compiler::compile(forms, [(~ssa, binding), ~r2e, ~dynamic])
    
    # Convert Reia variable names to Erlang SSA names
    binding = binding.map { |(var, value)| (var.to_s().capitalize().sub(/$/, "_0").to_atom(), value) }
    
    # Set up a fun which is called for local functions
    local_callback = fun(name, arguments) { local(name, arguments) }
    
    # Evaluate the expressions using erl_eval
    (~value, result, binding) = erl_eval::exprs(erl_forms, binding, (~value, local_callback))
    
    # Convert Erlang SSA variable names back to Reia names
    binding = binding.map { |(var, value)| (var.to_s().sub(/_[0-9]+$/, "").uncapitalize().to_atom(), value) }
    
    # Keep only the latest variable name
    binding = binding.reduce({}) { |(var, value), hash| hash.insert(var, value) }.to_list()
    
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
      