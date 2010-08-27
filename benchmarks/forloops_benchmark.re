module ForloopsBenchmark
  def run
    range = 1..1000000
    #list = range.to_list() # Note: weird thing happens when you uncoment this, it affects the speed of the NEXT test drastically
    [ n for n in range ]
  end
end