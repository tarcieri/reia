#
# runner: Runner for Reia's test suite
# Copyright (C)2008-10 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

Main.load("test/test_helper.re")

tests = [
  ("builtins", ["list", "tuple", "string", "regex", "numeric", "map", "atom"]),
  ("core",     ["blocks", "branching", "funrefs"])
]

results = tests.map do |(group, modules)|
  modules.map do |name|
    try
      Main.load("test/#{group}/#{name}.re")

      mod = "#{name.capitalize()}Test".to_module()
      mod.run()
    catch ex
      Main.print("E")
      (:error, "#{group}/#{name}.re", ex)
    end
  end
end.flatten()

Main.puts("\n")

failures = [error for error = (:error, _, _, _, _) in results]
failures.each do |(:error, group, description, expected, actual)| 
  Main.puts("'#{group} #{description}' FAILED")
  Main.puts("expected #{expected.inspect()}, actual #{actual.inspect()}\n")
end

errors = [error for error = (:error, _, _) in results]
errors.each do |(:error, test, ex)|
  case ex
  when (:exception,(:throw, msg))
    Main.puts("#{test} ERROR: #{msg.to_string()}")
  when _
    Main.puts("#{test} ERROR: #{ex}\n")
  end
end

Main.puts("#{results.size()} assertions, #{failures.size()} failures, #{errors.size()} errors")
System.halt(1) if failures.size() > 0 or errors.size() > 0
