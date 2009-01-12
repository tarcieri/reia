#
# FileTest: Tests for Reia's file lib
# Copyright (C)2008 Jared Kuolt
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#
load("lib/file.re")

module FileTest
  def run
    Local.puts("File")
    [iodevice_test(), 
    fm_read_test(), 
    fm_write_test(), 
    fm_copy_test(), 
    fm_rename_test(), 
    fm_delete_test(), 
    fm_mkdir_test(),   
    fm_lsdir_test(), 
    fm_deldir_test()]

  def iodevice_test
    fn = "foo.txt"
    f = File.open(fn, [~write])
    f.write("bar")
    f.close()

    x = File.open(fn)
    TestHelper.expect("file is written to and read from in the IODevice class", fun do
      result = ("bar", x.read(3))
      x.close()
      result
    )

  def fm_read_test
    TestHelper.expect("File.read (module version) reads correctly", fun do
      ("bar", File.read("foo.txt"))
    )

  def fm_write_test
    TestHelper.expect("File.write (module version) writes correctly", fun do
      (~ok, File.write("foo.txt", "baz"))
    )

  def fm_copy_test
    TestHelper.expect("File.copy copies file", fun do
      (~ok, File.copy("foo.txt", "bar.txt"))
    )

  def fm_rename_test
    TestHelper.expect("File.rename moves file", fun do
      (~ok, File.rename("foo.txt", "bar.txt"))
    )

  def fm_delete_test
    TestHelper.expect("File.delete removes file", fun do
      (~ok, File.delete("bar.txt"))
    )

  def fm_mkdir_test
    TestHelper.expect("File.make_dir creates directory", fun do
      (~ok, File.make_dir("testdir"))
    )

  def fm_lsdir_test
    TestHelper.expect("File.list_dir lists directory contents", fun do
      ([], File.list_dir("testdir"))
    )

  def fm_deldir_test
    TestHelper.expect("File.del_dir removes directory", fun do
      (~ok, File.del_dir("testdir"))
    )


