module DoloopsBenchmark
  def run
    range = 1..1000000
    list = range.to_list()
    list.map do |n|
      n
    end
  end
end