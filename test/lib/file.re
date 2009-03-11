#
# FileTest: Tests for Reia's file lib
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
load("lib/file.re")

module FileTest
  def run
    [iodevice_test(), 
    fm_read_test(), 
    fm_write_test(), 
    fm_copy_test(), 
    fm_move_test(), 
    fm_delete_test()]
  end
  
  def iodevice_test
    fn = "foo.txt"
    f = File.open(fn, [:write])
    f.write("bar")
    f.close()

    x = File.open(fn)
    TestHelper.expect(File, "file is written to and read from in the IODevice class", fun do
      result = (<<"bar">>, x.read(3))
      x.close()
      result
    end)
  end
  
  def fm_read_test
    TestHelper.expect(File, "File.read (module version) reads correctly", fun do
      (<<"bar">>, File.read("foo.txt"))
    end)
  end
  
  def fm_write_test
    TestHelper.expect(File, "File.write (module version) writes correctly", fun do
      (:ok, File.write("foo.txt", "baz"))
    end)
  end
  
  def fm_copy_test
    TestHelper.expect(File, "File.copy copies file", fun do
      (:ok, File.copy("foo.txt", "bar.txt"))
    end)
  end
  
  def fm_move_test
    TestHelper.expect(File, "File.move moves file", fun do
      (:ok, File.move("foo.txt", "bar.txt"))
    end)
  end
  
  def fm_delete_test
    TestHelper.expect(File, "File.delete removes file", fun do
      (:ok, File.delete("bar.txt"))
    end)
  end
end