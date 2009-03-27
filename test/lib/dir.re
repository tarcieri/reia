#
# DirTest: Tests for Reia's file lib
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

load("lib/dir.re")

module DirTest
  def run
    [
      create_test(),   
      list_test(), 
      delete_test()
    ]
  end
  
  def create_test
    TestHelper.expect(Dir, "creates directories") do
      (:ok, Dir.create("testdir"))
    end
  end
  
  def list_test
    TestHelper.expect(Dir, "lists directory contents") do
      (0, Dir.list("testdir").size())
    end
  end
  
  def delete_test
    TestHelper.expect(Dir, "removes directories") do
      (:ok, Dir.delete("testdir"))
    end
  end
end