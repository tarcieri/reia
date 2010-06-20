#
# TCPSocket: Reia interface for TCP sockets
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

# General socket-related errors
class SocketError < StandardError; end

# DSN-related errors
class ResolveError < SocketError; end

# This interface is by no means finalized or complete.  It will be subject to
# a number of changes.  Please keep this in mind when using it.
class TCPSocket
  # Open a TCP connection the given host and port
  #
  # Example:
  #  sock = TCPSocket("www.google.com", 80)
  #
  def initialize(host, port)
    case erl.gen_tcp.connect(host.to_list(), port, [:binary, (:active, false)])
    when (:ok, socket)
      @socket = socket
    when (:error, :nxdomain)
      throw(ResolveError, "cannot resolve '#{host}'")
    when error
      throw(error.inspect())
    end
  end
  
  # Read the specified amount of data from the socket.  A read of length zero
  # will read all data available.
  def read(length)
    case erl.gen_tcp.recv(@socket, length)
    when (:ok, packet)
      packet
    when (:error, :closed)
    when (:error, reason)
      throw(reason.inspect())
    end
  end
  
  # Shortcut for read(0)
  def read(); read(0); end
  
  # Write the given data to the socket
  def write(data)
    case erl.gen_tcp.send(@socket, data.to_binary())
    when :ok
      true
    when (:error, reason)
      throw(reason.inspect())
    end
  end
  
  # Close the socket
  def close
    case erl.gen_tcp.close(@socket)
    when :ok
      true
    when (:error, reason)
      throw(reason.inspect())
    end
  end
end