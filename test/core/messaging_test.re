#
# MessagingTest: Tests for Reia's asynchronous messaging
# Copyright (C)2010 Tony Arcieri
# 
# Redistribution is permitted under the MIT license.  See LICENSE for details.
#

module MessagingTest
  def run
    [messaging_test()]
  end

  def messaging_test
    # FIXME: this test could use some work
    TestHelper.expect("Messaging", "works") do
      # Send ourselves a message
      Process.pid() ! (:theanswer, 42)
      
      receive
      when (:theanswer, answer)
        (42, answer)
      end
    end
  end
end