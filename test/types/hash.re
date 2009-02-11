#
# HashTest: Tests for Reia's hash type
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module HashTest
  def run
    [index_test(), compare_test(), remove_test(), size_test(), keys_test(), has_test(), has_test2()]
    
  def index_test
    TestHelper.expect(Hash, "index recieves proper value", fun do
      ({~foo: "bar"}[~foo], "bar")
    )

  def compare_test
    TestHelper.expect(Hash, "comparison is equal", fun do
      ({~foo: "bar"}, {~foo: "bar"})
    )

  def remove_test
    TestHelper.expect(Hash, "removes a value", fun do
      ({~foo: "bar", ~zoo: "horse"}.remove(~zoo), {~foo: "bar"})
    )

  def size_test
    TestHelper.expect(Hash, "calculates size", fun do
      ({~foo: "bar", ~zoo: "horse"}.size(), 2)
    )

  def keys_test
    TestHelper.expect(Hash, "collects keys into a list", fun do
      ({~foo: "bar", ~zoo: "horse", ~bar: "foo"}.keys(), [~bar, ~zoo, ~foo])
    )

  def has_test
    TestHelper.expect(Hash, "checks if a key is presented", fun do
      ({~foo: "bar", ~zoo: "horse"}.has(~foo), true)
    )

  def has_test2
    TestHelper.expect(Hash, "checks if a key is missing", fun do
      ({~foo: "bar", ~zoo: "horse"}.has(~bar), false)
    )
