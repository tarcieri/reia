#
# Regex: Methods for the Regexp pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Regex
  def funcall(regexp, ~to_list, [])
    (~regexp, bin) = regexp.uninternalize()
    erlang::binary_to_list(bin)
    
  def funcall(regexp, ~to_string, [])
    regexp.to_list().to_string()
    
  def funcall(regexp, ~to_s, [])
    ["/", regexp.to_string(), "/"].join()
    
  def funcall(regexp, ~inspect, [])
    funcall(regexp, ~to_s, [])
  
  def funcall(regexp, ~match, [string])
    str = string.to_list()
    case regexp::match(str, regexp.to_list())
      (~match, start, length):
        string::sub_string(str, start, start + length - 1).to_string()
      ~nomatch:
        nil

  def funcall(regexp, ~matches, [string])
    str = string.to_list()
    case regexp::matches(str, regexp.to_list())
      (~match, results):
        results.map { |(start, length)| string::sub_string(str, start, start + length - 1).to_string() }
      ~nomatch:
        []

  def funcall(regexp, ~sub, [string, replacement])
    case regexp::sub(string.to_list(), regexp.to_list(), replacement.to_list())
      (~ok, result, _):
        result.to_string()
      ~error:
        nil

  def funcall(regexp, ~gsub, [string, replacement])
    case regexp::gsub(string.to_list(), regexp.to_list(), replacement.to_list())
      (~ok, result, _):
        result.to_string()
      ~error:
        nil

  def funcall(regexp, ~split, [string])
    case regexp::split(string.to_list(), regexp.to_list())
      (~ok, results):
        results.map { |result| result.to_string() }
      (~error):
        []
