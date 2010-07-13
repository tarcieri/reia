#
# code.re: Interface to the Reia code server
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Code
  def paths
    [path.to_string() for path in CodeServer.call(:paths)]
  end
  
  def unshift_path(path)
    res = CodeServer.call(:unshift_path, path.to_s().to_list())
    [path.to_string() for path in res]
  end
  
  def push_path(path)
    res = CodeServer.call(:push_path, path.to_s().to_list())
    [path.to_string() for path in res]
  end
  
  def set_paths(paths)
    res = CodeServer.call(:set_paths, [path.to_s().to_list() for path in paths])
    [path.to_string() for path in res]
  end
end