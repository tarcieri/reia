require 'rake/clean'

task :default => %w(check_erl_version check_previous_install build)

#
# Prerequisites
#

# Returns the installed Erlang version
def erlang_version
  version = `erl -version 2>&1`.strip.match(/\d\.\d\.\d$/)
  unless version
   STDERR.puts "Error retrieving Erlang version.  Do you have it installed?" 
   exit 1
  end
  
  version[0]
end

# Retrieve the directory Erlang libraries are stored in
def erl_lib_dir
  `erl -noshell -eval "io:format(code:lib_dir())" -s init stop`
end

# Directory to install Reia into
def reia_install_dir
  File.join(erl_lib_dir, 'reia', '')
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

# Ensure Reia was not previously installed
task :check_previous_install do
  if File.exists?(reia_install_dir)
    puts "*** WARNING: Previous installation of Reia detected"
    puts "*** If you experience problems during the build process please try"
    puts "*** running 'rake uninstall' before proceeding"
  end
end

# Generate an output path for the given input file
def output_file(input_file, dir = 'ebin/')
  dir + File.basename(input_file).sub(/\.\w+$/, '.beam')
end

ERL_SRC = FileList.new('src/{compiler,core}/**/*.erl')
ERL_SRC.each do |input|
  unless output_file(input) == "ebin/reia_parse.beam"
    file output_file(input) => input do
      sh "erlc +debug_info -o ebin #{input}"
    end
  end
end

# Build rules
task :build   => %w(scanner reia)
task :reia    => ERL_SRC.map { |input_file| output_file(input_file) }
task :scanner => %w(src/leex/leex.beam src/compiler/reia_scan.erl)

# Scanner
file "src/leex/leex.beam" => "src/leex/leex.erl" do
  sh "erlc -W0 -o src/leex src/leex/leex.erl"
end

file "src/compiler/reia_scan.erl" => %w[src/leex/leex.beam src/compiler/reia_scan.xrl] do
  cmd = 'leex:file("src/compiler/reia_scan.xrl")'
  sh "erl -noshell -pa src/leex -eval '#{cmd}' -s init stop"
end
