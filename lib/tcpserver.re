#
# TCPServer: Generic TCP server interface
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

class TCPServer
  def initialize(@addr, @port, options)    
    # FIXME: Needs better dict literal grammar! And keyword args ;(
    opts = {:mode=>:binary, :active=>:false, :packet=>:raw, :reuseaddr=>:true}
    opts.merge!(options)
    
    option_list = [
      opts[:mode],
      (:active, opts[:active]),
      (:packet, opts[:packet]),
      (:reuseaddr, opts[:reuseaddr])
    ]
        
    case erl.gen_tcp.listen(@port, option_list)
    when (:ok, sock)
      @sock = sock
    when (:error, reason)
      throw("Couldn't create TCPServer: #{reason}")
    end
  end
  
  def accept
    case erl.gen_tcp.accept(@sock)
    when (:ok, port)
      TCPSocket(port)
    when (:error, error)
      throw("TCPServer error: #{error.inspect()}")
    end
  end
  
  def stop
    erl.gen_tcp.close(@sock)
  end
end