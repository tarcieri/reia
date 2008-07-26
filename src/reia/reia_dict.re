#
# reia_dict: Methods for the Dict pseudo-class
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
module ReiaDict
  # Dict#[]
  #   Retrieve an element from a Dict
  def funcall(term, ~'[]', [key])
    (~dict, dict) = term.to_internal()
    case dict::find(key, dict)
      (~ok, value):
        value
      ~error:
        nil

  # Dict#to_list
  #   Convert a dict to a list of its key/value pairs
  def funcall(term, ~to_list, [])
    (~dict, dict) = term.to_internal()
    dict::to_list(dict)
    
  # Dict#to_s
  #   Convert a dict to a string representation
  def funcall(dict, ~to_s, [])
    members = dict.to_list().map { |(k, v)| [k, v].join(":") }
    ["{", members.join(","), "}"].join()