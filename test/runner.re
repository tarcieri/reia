#
# runner: Runner for Reia's test suite
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

load("test/test_helper.re")

tests = [
  "builtins/list",
  "builtins/tuple",
  "builtins/string",
  "builtins/regex",
  "builtins/numeric",
  "builtins/hash",
  "builtins/atom",
  "core/object",
  "core/blocks",
  "lib/file",
  "lib/dir"
]

results = tests.map do |test|
  try
    load("test/#{test}.re")
  
    name = test.split(/\//)[1]
    mod = "#{name.capitalize()}Test".to_constant()
    mod.run()
  catch ex
    print("E")
    (:error, "#{test}.re", ex)
  end
end.flatten()

puts("\n")

failures = [error|error = (:error, _, _, _, _) in results]
failures.each do |(:error, group, description, expected, actual)| 
  puts("'#{group} #{description}' FAILED")
  puts("expected #{expected.inspect()}, actual #{actual.inspect(    )}\n")
end

errors = [error|error = (:error, _, _) in results]
errors.each do |(:error, test, ex)|
  puts("#{test} ERROR: #{ex}\n")
end

puts("#{results.size()} assertions, #{failures.size()} failures, #{errors.size()} errors")
System.halt(1) if failures.size() > 0 or errors.size() > 0