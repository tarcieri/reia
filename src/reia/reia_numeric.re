module ReiaNumeric
  def pow(base, exponent)
    result = math::pow(base, exponent)
    if erlang::is_integer(base)
      erlang::round(result)
    else
      result
        
  def funcall(number, ~to_s, [])
    if erlang::is_integer(number)
      erlang::integer_to_list(number).to_string() 
    else
      [list] = io_lib::format("~f".to_list(), [number])
      list.to_string()