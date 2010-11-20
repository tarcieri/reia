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

  # Returns the first element in the list
  def first
    erl.lists.nth(1, self)
  end

  # Returns the first n-elements in the list
  def first(n)
    erl.lists.sublist(self, n)
  end

  # Returns the last element in the list
  def last
    erl.lists.nth(size(), self)
  end

  # returns the last n-elements in the list
  def last(n)
    index = [0, (size() - n)].max()
    erl.lists.nthtail(index,self)
  end

  # Returns the first element of list that compares less than or equal
  # to all other elements of list.
  def min
    erl.lists.min(self)
  end

  # Returns the first element of list that compares greater than or equal
  # to all other elements of list.
  def max
    erl.lists.max(self)
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

  # Returns a new list built by concatenating the two lists together to 
  # produce a third list.
  def append(other_list)
    erl.lists.append(self, other_list)
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

  # Takes a conditional block and returns true if all elements meet the
  # condition, false otherwise
  def all?(&block)
    erl.lists.all(block, self)
  end

  # Takes a conditional block and returns true if any element meets the
  # condition, false otherwise
  def any?(&block)
    erl.lists.any(block, self)
  end

  # Takes a conditional block and returns a new list with only the elements
  # for which the block evaluates to true
  def select(&block)
    erl.lists.filter(block, self)
  end

  # Takes a conditional block and partitions the list into two lists,
  # where the first list includes the elements which meet the condition 
  # and the second list contains the elements which do not meet the 
  # condition.
  def partition(&block)
    erl.lists.partition(block, self)
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
  
  # Cast to a binary
  def to_binary
    erl.list_to_binary(self)
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
