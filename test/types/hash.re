#
# HashTest: Tests for Reia's hash type
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module HashTest
  def run
    [index_test(), compare_test()]
    
  def index_test
    TestHelper.expect(Hash, "index recieves proper value", fun do
      ({~foo: "bar"}[~foo], "bar")
    )

  def compare_test
    TestHelper.expect(Hash, "comparison is equal", fun do
      ({~foo: "bar"}, {~foo: "bar"})
    )
