# run benchmarks in Reia and Erlang

started_at = erl.now()
System.load("test/test_helper.re")

benchmarks = ["recursion", "sleep", "forloops", "doloops"]
bench_repeat = 10

"Running benchmark tests #{bench_repeat} times each".puts()
bench = benchmarks.map do |name|
  System.load("benchmarks/#{name}_benchmark.re")
  
  erl_module = "benchmarks/ebin/#{name}_benchmark"
  case erl.code.load_abs(erl_module.to_list())
  when (:error, error)
    throw("Couldn't load #{erl_module}: #{error}")
  end
  
  mod = "#{name.capitalize()}Benchmark".to_module()
  reia_bench_start = erl.now()
  (1..bench_repeat).to_list().map do |n|
    mod.run()
  end
  reia_bench_end = erl.now()
  
  erl_bench_start = erl.now()
  (1..bench_repeat).to_list().map do |n|
    erl.apply("#{name}_benchmark".to_atom(), :run, [])
  end
  erl_bench_end = erl.now()
  
  reia_duration = TestHelper.duration(reia_bench_start, reia_bench_end)
  erl_duration = TestHelper.duration(erl_bench_start, erl_bench_end)
  ratio = reia_duration / erl_duration
  "#{name} Reia:#{reia_duration}s Erlang:#{erl_duration}s that's #{ratio}x slower".puts()
end

finished_at = erl.now()

duration = TestHelper.duration(started_at, finished_at)
"\nFinished in #{duration} seconds\n".puts()
"#{bench.size()} benchmarks".puts()
