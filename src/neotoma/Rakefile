require 'rake/clean'

task :default => %w(check_erl_version build test)

# Build rules
task :build   => %w(neotoma)
task :neotoma => %w(parser compile)
task :parser  => %w(src/neotoma_parse.erl)

# include file
file "priv/peg_includes.erl" => ["src/neotoma_peg.erl"] do
	sh "cat src/neotoma_peg.erl | grep -v \"^%\" | grep -v \"^-\" > priv/peg_includes.erl"
  end

# Parser
if File.exist?("ebin/neotoma_parse.beam")
  file "src/neotoma_parse.erl" => ["src/neotoma_parse.peg", "priv/peg_includes.erl"] do
    erl_eval 'neotoma:file("src/neotoma_parse.peg")', 'ebin/'
  end
else
  sh "cp src/neotoma_parse.erl.safe  src/neotoma_parse.erl"
end

task :compile => %w(ebin src/neotoma.app priv/peg_includes.erl) do
    sh "cd src;erl -pa ../ebin -make"
  end

task :test_beams => %w(ebin_tests) do
    sh "cd tests;erl -pa ../ebin -make"
  end

# create directories
task :ebin          do mkdir "ebin" end
task :priv          do mkdir "priv" end
task :ebin_tests    do mkdir "ebin_tests" end

def mkdir(dir)
  File.exist?(dir) || sh("mkdir #{dir}")
end


# Test suite
task :test => %w(build test_beams) do
  erl_eval 'test_suite:test()', 'ebin', 'ebin_tests', 'ebin_tests/examples'
end

# Cleaning
CLEAN.include %w(ebin ebin_tests src/neotoma_parse.erl priv/peg_includes.erl)

#---- generic erlang version tests ------

# Returns the installed Erlang version
def erlang_version
  version = `erl -version 2>&1`.strip.match(/\d\.\d(\.\d)?$/)
  unless version
   STDERR.puts "Error retrieving Erlang version.  Do you have it installed?" 
   exit 1
  end
  
  version[0]
end

# Evaluate the given Erlang statement
def erl_eval(cmd, *pa)
  pa_str = pa.empty? ? "" : "-pa #{pa.join(' ')}"
  sh "erl -noshell #{pa_str} -eval '#{cmd}' -s erlang halt"
end

# Ensure the version of Erlang installed is recent enough
task :check_erl_version do
  print "Checking Erlang version... "
  version = erlang_version
  
  if version >= "5.6.3"
    puts "#{version} (ok)"
  else
    puts "#{version} (too old)"
    puts "Sorry, the version of Erlang you have installed is too old to run Reia"
    puts "Reia requires a minimum Erlang version of R12B-3 (5.6.3)"
    puts "Please see http://wiki.reia-lang.org/wiki/Building#Prerequisites"
    exit 1
  end
end


