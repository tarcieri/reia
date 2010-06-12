#
# exceptions.re: Classes representing various standard exceptions
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Exception
  def class; Exception; end
  
  def initialize(file, line, message)
    (@file, @line, @message) = (file, line, message)
  end
    
  def file; @file; end
  def line; @line; end
  def message; @message; end
end

class StandardError < Exception; end
class NativeError   < Excpetion; end

class NoMethodError < StandardError; end
class ArgumentError < StandardError; end
class RuntimeError  < StandardError; end
class NoMatchError  < StandardError; end