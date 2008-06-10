module ReiaTuple
  # Tuple#to_list
  #   Convert a tuple to a list
  def funcall(tuple, ~to_list, [])
    erlang::tuple_to_list(tuple)
  
  # Tuple#[]
  #   Retrieve an element from a Tuple
  def funcall(tuple, ~'[]', [index])
    tuple.to_list()[index]
  
  # Tuple#to_s
  #   Generate a string representation of a Tuple  
  def funcall(tuple, ~to_s, [])
    ["(", tuple.to_list().join(","), ")"].join()