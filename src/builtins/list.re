#
# list.re: Methods of the List builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# Lists are singly linked chains of values which are easy to prepend to
class List
  # Lists are lists!
  def class; List; end
  
  # Retrieve the element at the given 0-based (or negative) index
  def [](index)
    length = erl.length(self)
    
    index += length if index < 0
    index += 1
    
    erl.lists.nth(index, self) if index >= 1 and index <= length
  end
  
  # Slowly set an element at a given 0-based (or negative) index
  def []=(index, value)
    if index < 0
      replace(erl.length(self) + index, value)
    else
      replace(index, value)
    end
  end
  
  # Number of elements in a list
  def size
    erl.length(self)
  end
  
  # Reverse the order of a list
  def reverse
    erl.lists.reverse(self)
  end
  
  # Join a list into a string, casting all elements to strings
  def join(separator)
    elements = case self
    when []
      []
    when _
      join_list([], self, separator.to_list())
    end
        
    elements.to_string()
  end
  def join
    join("")
  end
  
  # Iterate over a list, successively calling the given block and returning
  # the receiver when done
  def each(&block)
    erl.lists.foreach(block, self)
    self
  end

  # Iterate over a list, building a new list of values returned from the
  # given block when called with each element in the list
  def map(&block)
    erl.lists.map(block, self)
  end
  
  # Flatten a deeply nested list
  def flatten
    erl.lists.flatten(self)
  end
  
  # Generate a string representative of a list's contents
  def inspect
    to_s()
  end

  # Cast to a tuple
  def to_tuple
    erl.list_to_tuple(self)
  end

  # Cast a list of 2-tuples to a dict
  def to_dict
    erl.dict.from_list(self)
  end
  
  # Cast to a list, returning the identity
  def to_list
    self
  end

  # Interpret a list of integers as characters and convert them to a string
  def to_string
    elements = flatten().map do |element|
      case element
      when (:reia_string, parts)
        parts
      when _
        element
      end
    end
    
    (:reia_string, elements)
  end
  
  # Coerce to a string
  def to_s
    "[#{map { |e| e.inspect() }.join(',')}]"
  end
  
  # Is the list empty?
  def empty?
    case self
    when []
      true
    when _
      false
    end
  end

  # FIXME: implement private
  #########
  #private#
  #########
  
  def replace(index, value)
    replace(self, 0, index, value)
  end
  
  def replace([], _, _, _)
    throw(ArgumentError, "no such element to replace")
  end
  def replace([head, *tail], n, index, value)
    if n == index
      [value, *tail]
    else
      [head, *replace(tail, n + 1, index, value)]
    end
  end

  def join_list(result, [elem], _)
    erl.lists.reverse([convert_element(elem), *result])
  end
  def join_list(result, [elem, *rest], separator)
    join_list([separator, convert_element(elem), *result], rest, separator)
  end
  
  def convert_element(elem)
    elem.to_s().to_list()
  end
end