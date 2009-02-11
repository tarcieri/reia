module Ring
  def around(procs, times)
    pid = start(procs, erlang::self())
    pid ! times
    parent_loop(pid)
    
  def parent_loop(pid)
    receive
    when 0
      nil
    when n
      pid ! n - 1
      parent_loop(pid)
      
  def start(0, parent)
    parent
  def start(n, parent)
    start(n - 1, Process.spawn(fun() { child_loop(parent) }))
  
  def child_loop(parent)
    receive
    when msg
      parent ! msg
      child_loop(parent)