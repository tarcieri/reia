#
# String: Methods of the String builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module String
  def call(self, :to_binary, args, block)
    (:reia_string, elements) = self
    erl.iolist_to_binary(elements)
  end
  
  def call(self, :to_list, args, block)
    erl.binary_to_list(self.to_binary())
  end

  
  def call(self, :to_atom, args, block)
    erl.list_to_atom(self.to_list())
  end
  
  def call(self, :to_module, args, block)
    (:reia_module, self.to_atom())
  end
  
  def call(self, :to_s, args, block)
    self
  end
  
  def call(self, :inspect, args, block)
    "\"#{self.to_s()}\""
  end
  
  def call(self, :print, args, block)
    erl.io.format(self.to_list())
  end
  
  def call(self, :size, args, block)
    self.to_binary().size()
  end
  
  def call(self, :length, args, block)
    self.size()
  end
  
  def call(self, :capitalize, args, block)
    [first, *rest] = self.to_list()
    [erl.string.to_upper([first]), *rest].to_string()
  end
  
  def call(self, :sub, (pattern, replacement), block)
    case pattern
    when (:reia_regexp, regex)
      nil # FIXME: urgh, this shouldn't be necessary
    when (:reia_string, _)
      # FIXME: this shouldn't use a regex, but I'm lazy
      regex = pattern.to_binary()
    #when _ # FIXME: this should really be else clause :/
      # FIXME: throw grammar not implemented :(
      #throw "invalid type for pattern"
    end
    
    list = self.to_list()
    case erl.re.run(self.to_binary(), regex)
    when (:match, [(start, length)])
      head = erl.lists.sublist(list, 1, start).to_string()
      tail = erl.lists.sublist(list, start + length + 1, erl.length(list)).to_string()
      "#{head}#{replacement}#{tail}"
    when :nomatch
      self
    end
  end
end