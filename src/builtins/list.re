#
# list.re: Methods of the List builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module List
  def call(fake_self, :'[]', (index,), block)
    length = erl.length(fake_self)
    
    index += length if index < 0
    index += 1
    
    erl.lists.nth(index, fake_self) if index >= 1 and index <= length
  end
  
  def call(fake_self, :'[]=', (index, value), block)
    if index < 0
      replace(fake_self, erl.length(fake_self) + index, value)
    else
      replace(fake_self, index, value)
    end
  end
  
  def call(fake_self, :size, args, block)
    erl.length(fake_self)
  end

  def call(fake_self, :to_string, args, block)
    elements = fake_self.flatten().map do |element|
      case element
      when (:reia_string, parts)
        parts
      when _
        element
      end
    end
    
    (:reia_string, elements)
  end
  
  def call(fake_self, :to_s, args, block)
    "[#{fake_self.map { |e| e.inspect() }.join(',')}]"
  end

  def call(fake_self, :inspect, args, block)
    call(fake_self, :to_s, args, block)
  end
  
  def call(fake_self, :reverse, args, block)
    erl.lists.reverse(fake_self)
  end
  
  def call(fake_self, :join, (separator,), block)
    elements = case fake_self
    when []
      []
    when _
      join_list([], fake_self, separator.to_list())
    end
        
    elements.to_string()
  end
  def call(fake_self, :join, (), block)
    call(fake_self, :join, ("",), block)
  end
  
  def call(fake_self, :each, args, block)
    erl.lists.foreach(block, fake_self)
    fake_self
  end

  def call(fake_self, :map, args, block)
    erl.lists.map(block, fake_self)
  end
  
  def call(fake_self, :flatten, args, block)
    erl.lists.flatten(fake_self)
  end

  def call(fake_self, :to_tuple, args, block)
    erl.list_to_tuple(fake_self)
  end

  def call(fake_self, :to_dict, args, block)
    erl.dict.from_list(fake_self)
  end
      
  def call(fake_self, :to_list, args, block)
    fake_self
  end

  def replace(fake_self, index, value)
    replace(fake_self, 0, index, value)
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
