#
# exceptions.re: Classes representing various standard exceptions
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Exception  
  def initialize(file, line, message)
    (@file, @line, @message) = (file, line, message)
  end
    
  def file; @file; end
  def line; @line; end
  def message; @message; end
  
  def to_s
    trace = backtrace().map do |(mod, func, _)|
      "\tfrom #{mod}: in '#{func}'"
    end
    
    trace[0] = "#{trace[0]} (line #{@line})" if @line
    trace.join!("\n")
    
    "#{class()}: #{message()}\n#{trace}"
  end
  
  def backtrace
    # FIXME: this uses a process-local side effect
    # Would probably be best (albeit slow) to capture this AOT
    erl.get_stacktrace()
  end
end

class StandardError < Exception; end
class NativeError   < Excpetion; end

class SyntaxError   < StandardError; end
class NameError     < StandardError; end
class NoMethodError < StandardError; end
class ArgumentError < StandardError; end
class RuntimeError  < StandardError; end
class NoMatchError  < StandardError; end