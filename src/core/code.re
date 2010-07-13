#
# code.re: Interface to the Reia code server
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module Code
  def paths
    CodeServer.call(:paths)
  end
end