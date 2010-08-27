module RecursionBenchmark
  def loop(0)
    :ok
  end
  
  def loop(n)
    k = n - 1
    loop(k)
  end
  
  def run
    loop(1000000)
  end
end
