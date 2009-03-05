# Process ring example
#
# Create a ring of processes of the given size and send a message around it
# the given number of times
#
# From the toplevel Reia directory, run: bin/ire 
# Then type: load("examples/ring.re")
#
# Send messages around the ring with:
# >> Ring.around(503, 1000)
#

module Ring
  def around(procs, times)
    pid = start(procs, erlang::self())
    pid ! times
    parent_loop(pid)
  end
    
  def parent_loop(pid)
    receive
    when 0
      nil
    when n
      pid ! n - 1
      parent_loop(pid)
    end
  end
      
  def start(0, parent)
    parent
  end
  def start(n, parent)
    start(n - 1, Process.spawn(fun() { child_loop(parent) }))
  end
  
  def child_loop(parent)
    receive
    when msg
      parent ! msg
      child_loop(parent)
    end
  end
end