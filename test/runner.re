#
# runner: Runner for Reia's test suite
# Copyright (C)2008 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

try
  load("test/test_helper.re")

  # List test
  load("test/builtins/list.re")
  r1 = ListTest.run()

  # Tuple test
  load("test/builtins/tuple.re")
  r2 = TupleTest.run()

  # String test
  load("test/builtins/string.re")
  r3 = StringTest.run()

  # Regex test
  load("test/builtins/regex.re")
  r4 = RegexTest.run()

  # Numeric test
  load("test/builtins/numeric.re")
  r5 = NumericTest.run()

  # Hash test
  load("test/builtins/hash.re")
  r6 = HashTest.run()

  # Atom test
  load("test/builtins/atom.re")
  r7 = AtomTest.run()

  # Object test
  load("test/core/object.re")
  r8 = ObjectTest.run()

  # File test
  load("test/lib/file.re")
  r9 = FileTest.run()

  # File test
  load("test/lib/dir.re")
  r10 = DirTest.run()

  puts("\n")

  results = [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10].flatten()
  failures = [error|error = (:error, _, _, _, _) in results]
  failures.each do |(:error, group, description, expected, actual)| 
    puts("'#{group} #{description}' FAILED")
    puts("expected #{expected.inspect()}, actual #{actual.inspect()}\n")
  end
  
  puts("#{results.size()} assertions, #{failures.size()} failures")
  System.halt(1) if failures.size() > 0
catch ex
  puts("Exception: #{ex}")
  System.halt(1)
end