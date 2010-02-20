#
# list.re: Methods of the List builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module List
  def call(self, :'[]', (index,), block)
    length = erl.length(self)
    
    index += length if index < 0
    index += 1
    
    erl.lists.nth(index, self) if index >= 1 and index <= length
  end
  
  def call(self, :'[]=', (index, value), block)
    if index < 0
      replace(self, erl.length(self) + index, value)
    else
      replace(self, index, value)
    end
  end
  
  def call(self, :size, args, block)
    erl.length(self)
  end

  def call(self, :to_string, args, block)
    elements = self.flatten().map do |element|
      case element
      when (:reia_string, parts)
        parts
      when _
        element
      end
    end
    
    (:reia_string, elements)
  end
  
  def call(self, :to_s, args, block)
    "[#{self.map { |e| e.inspect() }.join(',')}]"
  end

  def call(self, :inspect, args, block)
    call(self, :to_s, args, block)
  end
  
  def call(self, :reverse, args, block)
    erl.lists.reverse(self)
  end
  
  def call(self, :join, (separator,), block)
    elements = case self
    when []
      []
    when _
      join_list([], self, separator.to_list())
    end
        
    elements.to_string()
  end
  def call(self, :join, (), block)
    call(self, :join, ("",), block)
  end
  
  def call(self, :each, args, block)
    erl.lists.foreach(block, self)
    self
  end

  def call(self, :map, args, block)
    erl.lists.map(block, self)
  end
  
  def call(self, :flatten, args, block)
    erl.lists.flatten(self)
  end

  def call(self, :to_tuple, args, block)
    erl.list_to_tuple(self)
  end

  def call(self, :to_dict, args, block)
    erl.dict.from_list(self)
  end
      
  def call(self, :to_list, args, block)
    self
  end

  def replace(self, index, value)
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