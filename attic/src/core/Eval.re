#
# Eval: Methods for evaluating Reia expressions or abstract syntax
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Eval
  def string(str)
    case reia_parse::string(str.to_list())
    when (:ok, forms)
      (:value, value, _binding) = exprs(forms, new_binding())
      value
    when error
      throw error
    end
  end
    
  def exprs(forms, binding)
    # Compile Reia forms to Erlang forms
    erl_forms = reia_compiler::compile(forms, passes(binding))
    
    # Convert Reia variable names to Erlang SSA names
    binding = binding.map { |(var, value)| (var.to_s().sub(/^/, "_").sub(/$/, "_0").to_atom(), value) }
        
    # Evaluate the expressions using erl_eval
    (:value, result, binding) = eval_shim::exprs(erl_forms, binding)
    
    # Convert Erlang SSA variable names back to Reia names
    binding = binding.map { |(var, value)| (var.to_s().sub(/^_/, "").sub(/_[0-9]+$/, "").to_atom(), value) }
    
    # Keep only the latest variable name
    binding = binding.reduce({}) { |(var, value), hash| hash.insert(var, value) }.to_list()
    
    (:value, result, binding)
  end
    
  def passes(binding)
    reia_compiler::default_passes().map do |pass|
      if pass == :ssa
        (:ssa, binding)
      else
        pass
      end
    end
  end
  
  def new_binding
    erl_eval::new_bindings()
  end
end