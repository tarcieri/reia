

class TCPSocket
  def initialize(host, port)
    case gen_tcp::connect(host.to_list(), port, [~binary, (~active, false)])
      (~ok, socket):
        @socket = socket
        ~ok
      error:
        throw error
        
  def read(length)
    case gen_tcp::recv(@socket, length)
      (~ok, packet):
        packet
      (~error, reason):
        throw reason
        
  def write(data)
    case gen_tcp::send(@socket, data)
      ~ok:
        true
      (~error, reason):
        throw reason
  
  def close
    case gen_tcp::close(@socket)
      ~ok:
        true
      (~error, reason):
        throw reason