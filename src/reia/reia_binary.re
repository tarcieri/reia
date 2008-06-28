module ReiaBinary
  def funcall(binary, ~to_list, [])
    erlang::binary_to_list(binary)
    
  def funcall(binary, ~to_string, [])
    binary.to_list().to_string()
    
  def funcall(binary, ~to_s, [])
    list = binary.to_list()
    if io_lib::char_list(list)
      ['<<"', list.to_string(), '">>'].join()
    else
      ["<<", list.join(","), ">>"].join()
      
