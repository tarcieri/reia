#
# FileTest: Tests for Reia's file lib
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
load("lib/file.re")

module FileTest
  def run
    [
      iodevice_test(), 
      read_test(), 
      write_test(), 
      copy_test(), 
      move_test(), 
      delete_test(),
      exists_test()
    ]
  end
  
  def iodevice_test
    fn = "foo.txt"
    f = File.open(fn, [:write])
    f.write("bar")
    f.close()

    x = File.open(fn)
    TestHelper.expect(File, "file is written to and read from in the IODevice class") do
      result = (<<"bar">>, x.read(3))
      x.close()
      result
    end
  end
  
  def read_test
    TestHelper.expect(File, "read (module version) reads correctly") do
      (<<"bar">>, File.read("foo.txt"))
    end
  end
  
  def write_test
    TestHelper.expect(File, "write (module version) writes correctly") do
      (:ok, File.write("foo.txt", "baz"))
    end
  end
  
  def copy_test
    TestHelper.expect(File, "copies files") do
      (:ok, File.copy("foo.txt", "bar.txt"))
    end
  end
  
  def move_test
    TestHelper.expect(File, "moves files") do
      (:ok, File.move("foo.txt", "bar.txt"))
    end
  end
  
  def delete_test
    TestHelper.expect(File, "removes file") do
      (:ok, File.delete("bar.txt"))
    end
  end
  
  def exists_test
    TestHelper.expect(File, "knows if files exist") do
      should_be_there = File.exists?("/")
      should_not_be_there = File.exists?("/hopefullynobodycreatedthis")
      ((true, false), (should_be_there, should_not_be_there))
    end    
  end
end