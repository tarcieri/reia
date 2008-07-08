module ReiaEval
  def string(str)
    case reia_parse::string(str.to_list())
      (~ok, forms):
        exprs(forms, new_binding())
      error:
        error
    
  def exprs(forms, binding)
    erl_forms = reia_compiler::compile(forms)
    erl_eval::exprs(erl_forms, binding, (~value, fun(name, arguments) { local(name, arguments) }))
  
  def new_binding
    erl_eval::new_bindings()
    
  def local(name, args)
    case name
      ~puts:
        args.each { |arg| reia_kernel::puts(arg) }
        nil