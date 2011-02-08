#
# lolcat.re: irccat for Reia
# Copyright (C)2010 Tony Arcieri
#
# Redistribution is permitted under the MIT license. See LICENSE for details.
#

host = "irc.freenode.net"
port = 6667
nick = "lolcat"
channel = "#reia"

listen_addr = "0.0.0.0"
listen_port = 3210

class Lolcat
  def initialize(@server, @port, @nick, @channel)
    @sock = TCPSocket(@server, @port, {:packet => :line, :active => true})
  end
  
  # attr_reader ftw
  def server; @server; end
  def port; @port; end
  def nick; @nick; end
  def channel; @channel; end
  
  def register
    @sock.write("USER #{@nick} * * #{@nick}\n")
    @sock.write("NICK #{@nick}\n")
    @sock.write("JOIN #{@channel}\n")
  end
  
  def run
    receive
    when (:tcp, _, line)
      process_line(line.to_s().chop())
    when (:line, line)
      send_message("PRIVMSG #{@channel} :#{line}")
    when msg
      # Ignore unrecognized messages
    end
    
    run()
  end
    
  def process_line(line)
    line.puts()
    
    if %r/^PING /.match(line)
      handle_ping(line)
    elseif %r/PRIVMSG (#[a-z0-0]+) :ohai/.match(line)
      send_message("PRIVMSG #{@channel} :OHAI!!")
    end
  end
  
  def handle_ping(line)
    # The colors, Duke! The colors!
    [_, message] = %r/PING :(.*?)[\r\n]*$/.match(line)
    
    # Lulzy debug message
    "OHAI THAR SERVAR #{message}! PONG!!!".puts()
    
    # Oh yeah here's where we actually do the important thing
    send_message("PONG :#{message}")
  end
  
  def send_message(msg)
    @sock.write("#{msg}\r\n")
  end
end

class Server
  def initialize(@pid, @addr, @port)
    @server = TCPServer(@addr, @port, {:packet => :line})
    "*** TCP server listening on #{@addr}:#{@port}".puts()
  end
  
  def run
    connection = ConnectionHandler(@pid, @server.accept())
    Process.spawn { connection.run() }
    run()
  end
end

class ConnectionHandler
  def initialize(@pid, @sock); end
  
  def run()
    line = @sock.read()
    if line
      line = line.to_s().chop()
      @pid ! (:line, line)
      run()
    end
  end
end

cat = Lolcat(host, port, nick, channel)
cat.register()

server = Server(Process.pid(), listen_addr, listen_port)
Process.spawn_link { server.run() }

cat.run()
