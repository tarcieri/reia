module ReiaBinary
  def funcall(binary, ~to_s, [])
    list = erlang::binary_to_list(binary)
    if io_lib::char_list(list)
      ['<<"', list.to_string(), '">>'].join()
    else
      ["<<", list.join(","), ">>"].join()