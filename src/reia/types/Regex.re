#
# Regex: Methods for the Regexp pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Regex
  def funcall(regexp, ~to_list, [])
    (~regexp, bin) = regexp.to_internal()
    erlang::binary_to_list(bin)
    
  def funcall(regexp, ~to_string, [])
    regexp.to_list().to_string()
    
  def funcall(regexp, ~to_s, [])
    ["/", regexp.to_string(), "/"].join()
  
  def funcall(regexp, ~match, [string])
    case re::run(string.to_binary(), regexp.to_list(), [(~capture, ~all, ~binary)])
      (~match, results):
        results.map { |result| result.to_string() }
      ~nomatch:
        nil
