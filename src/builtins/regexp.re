#
# regexp.re: Methods of the Regexp builtin
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Regexp
  def class; Regexp; end
  
  def to_binary
    (:reia_regexp, pattern) = self
    pattern
  end
  
  def to_s
    (:reia_regexp, pattern) = self
    "%r/#{pattern}/"
  end
    
  def inspect
    to_s()
  end
  
  def match(string)
    case erl.re.run(string.to_binary(), to_binary(), [(:capture, :all, :binary)])
    when (:match, results)
      strings = [result.to_string() for result in results]
      if strings.size() > 1
        strings
      else
        strings[0]
      end
    when :nomatch
    end
  end
end
