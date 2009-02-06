#
# Object: Common ancestor of all classes
# Copyright (C)2008-09 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Object
  def initialize
    nil
    
  def class
    Object
    
  def to_s
    ["#<", class(), ">"].join()
    
  def inspect
    to_s()
    
  def _(method, args)
    throw (~error, (method, "undefined"))
    
  def handle_message(message)
    nil