#
# TCPServer: Generic TCP server interface
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class TCPServer
  def initialize(addr, port)
    (@addr, @port) = (addr, port)
    @options = [:binary, (:packet, 0), (:active, false), (:reuseaddr, true)]
    
    case erl.gen_tcp.listen(@port, @options)
    when (:ok, sock)
      @sock = sock
    when (:error, reason)
      throw("Couldn't create TCPServer: #{reason}")
    end
  end
  
  def accept
    case erl.gen_tcp.accept(@sock)
    when (:ok, port)
      port
    when (:error, error)
      throw("TCPServer error: #{error.inspect()}")
    end
  end
end