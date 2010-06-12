#
# list.re: Methods of the List builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class List
  def [](index)
    length = erl.length(self)
    
    index += length if index < 0
    index += 1
    
    erl.lists.nth(index, self) if index >= 1 and index <= length
  end
  
  def []=(index, value)
    if index < 0
      replace(erl.length(self) + index, value)
    else
      replace(index, value)
    end
  end
  
  def size
    erl.length(self)
  end

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
  
  def to_s
    "[#{map { |e| e.inspect() }.join(',')}]"
  end

  def inspect
    to_s()
  end
  
  def reverse
    erl.lists.reverse(self)
  end
  
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
  
  def each(&block)
    erl.lists.foreach(block, self)
    self
  end

  def map(&block)
    erl.lists.map(block, self)
  end
  
  def flatten
    erl.lists.flatten(self)
  end

  def to_tuple
    erl.list_to_tuple(self)
  end

  def to_dict
    erl.dict.from_list(self)
  end
      
  def to_list
    self
  end

  def replace(index, value)
    replace(self, 0, index, value)
  end
  
  # FIXME: throw syntax not implemented
  #def replace([], _, _, _)
  #  throw 'bad argument'
  #end
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