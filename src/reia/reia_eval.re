module ReiaEval
  def string(str)
    case reia_parse::string(str.to_list())
      (~ok, forms):
        exprs(forms, reia_eval::new_binding())
      error:
        error
    
  def exprs(forms, binding)
    erl_forms = reia_compiler::compile(forms)
    erl_eval::exprs(erl_forms, binding)
  
  def new_binding
    erl_eval::new_bindings()