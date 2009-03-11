#
# Regex: Methods for the Regexp pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Regex
  def funcall(regexp, :to_list, [])
    (:regexp, bin) = regexp.uninternalize()
    erlang::binary_to_list(bin)
  end
    
  def funcall(regexp, :to_string, [])
    regexp.to_list().to_string()
  end
    
  def funcall(regexp, :to_s, [])
    "/#{regexp.to_string()}/"
  end
    
  def funcall(regexp, :inspect, [])
    funcall(regexp, :to_s, [])
  end
  
  def funcall(regexp, :match, [string])
    case re::run(string.to_binary(), regexp.to_list(), [(:capture, :all, :binary)])
    when (:match, results)
      if results.size() > 1
        results.map { |result| result.to_string() }
      else
        results[0].to_string()
      end
    when :nomatch
      nil
    end
  end
end