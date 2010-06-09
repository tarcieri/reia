#
# String: Methods of the String builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module String
  def call(fake_self, :to_binary, args, block)
    (:reia_string, elements) = fake_self
    erl.iolist_to_binary(elements)
  end
  
  def call(fake_self, :to_list, args, block)
    erl.binary_to_list(fake_self.to_binary())
  end

  
  def call(fake_self, :to_atom, args, block)
    erl.list_to_atom(fake_self.to_list())
  end
  
  def call(fake_self, :to_module, args, block)
    (:reia_module, fake_self.to_atom())
  end
  
  def call(fake_self, :to_s, args, block)
    fake_self
  end
  
  def call(fake_self, :inspect, args, block)
    "\"#{fake_self.to_s()}\""
  end
  
  def call(fake_self, :print, args, block)
    erl.io.format(fake_self.to_list())
  end
  
  def call(fake_self, :size, args, block)
    fake_self.to_binary().size()
  end
  
  def call(fake_self, :length, args, block)
    fake_self.size()
  end
  
  def call(fake_self, :capitalize, args, block)
    [first, *rest] = fake_self.to_list()
    [erl.string.to_upper([first]), *rest].to_string()
  end
  
  def call(fake_self, :sub, (pattern, replacement), block)
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
    
    list = fake_self.to_list()
    case erl.re.run(fake_self.to_binary(), regex)
    when (:match, [(start, length)])
      head = erl.lists.sublist(list, 1, start).to_string()
      tail = erl.lists.sublist(list, start + length + 1, erl.length(list)).to_string()
      "#{head}#{replacement}#{tail}"
    when :nomatch
      fake_self
    end
  end
end
