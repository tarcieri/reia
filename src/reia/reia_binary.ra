module ReiaBinary
  def funcall(binary, ~to_s, [])
    list = erlang::binary_to_list(binary)
    case io_lib::char_list(list)
      true:
        ['<<"', list.to_string(), '">>'].join()
      false:
        ["<<", list.join(","), ">>"].join()