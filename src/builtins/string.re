#
# String: Methods of the String builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class String
  def class; String; end
  
  def to_string
    self
  end
  
  def to_s
    self
  end
  
  def to_binary
    (:reia_string, elements) = self
    erl.iolist_to_binary(elements)
  end
  
  def to_list
    erl.binary_to_list(to_binary())
  end

  
  def to_atom
    erl.list_to_atom(to_list())
  end
  
  def to_module
    to_atom().to_module()
  end
    
  def inspect
    "\"#{self}\""
  end
  
  def print
    erl.io.put_chars(to_list())
    self
  end
  
  def puts
    "#{self}\n".print()
  end
  
  def size
    to_binary().size()
  end
  
  def length
    size()
  end
  
  def chop
    [last, *rest] = to_list().reverse()
    rest.reverse().to_string()
  end
  
  def capitalize
    [first, *rest] = to_list()
    [erl.string.to_upper([first]), *rest].to_string()
  end
  
  def sub(pattern, replacement)
    case pattern
    when (:reia_regexp, regex)
      nil # FIXME: urgh, this shouldn't be necessary
    when (:reia_string, _)
      # FIXME: this shouldn't use a regex, but I'm lazy
      regex = pattern.to_binary()
    when _ # FIXME: this should really be else clause :/
      throw(ArgumentError, "invalid pattern: #{pattern}")
    end
    
    list = to_list()
    case erl.re.run(self.to_binary(), regex)
    when (:match, [(start, length)])
      head = erl.lists.sublist(list, 1, start).to_string()
      tail = erl.lists.sublist(list, start + length + 1, erl.length(list)).to_string()
      "#{head}#{replacement}#{tail}"
    when :nomatch
      self
    end
  end
    
  def parse(format)
    "#{format.to_s().capitalize()}Parser".to_module().parse(self)
  end
  
  def parse
    parse(:reia)
  end
end