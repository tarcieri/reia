#
# exceptions.re: Classes representing various standard exceptions
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class Exception  
  def initialize(line, message)
    (@line, @message) = (line, message.to_string())
  end
  
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
    # Would probably be better (albeit slower) to capture this AOT, lest
    # another exception gets thrown in the meantime and we lose the backtrace
    erl.get_stacktrace()
  end
end

#
# Exception types
#

# Standard Reia exceptions
class StandardError < Exception; end

# Errors arising from within the Erlang environment
class NativeError   < Exception; end

#
# Core language errors
#

class SyntaxError   < StandardError; end
class NameError     < StandardError; end
class NoMethodError < StandardError; end
class ArgumentError < StandardError; end
class RuntimeError  < StandardError; end
class NoMatchError  < StandardError; end
class LoadError     < StandardError; end

#
# I/O errors
#

class FileNotFound < StandardError; end
