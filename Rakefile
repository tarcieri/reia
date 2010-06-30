require 'rake/clean'

task :default => %w(check_erl_version check_previous_install build test)

# Returns the installed Erlang version
def erlang_version
  version = `erl -version 2>&1`.strip.match(/\d\.\d\.\d$/)
  unless version
   STDERR.puts "Error retrieving Erlang version.  Do you have it installed?" 
   exit 1
  end
  
  version[0]
end

# Evaluate the given Erlang statement
def erl_eval(cmd, *pa)
  pa_str = pa.empty? ? "" : "-pa #{pa.join(' ')}"
  sh "erl -noshell #{pa_str} -eval '#{cmd}' -s init stop"
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
def output_file(input_file, dir = 'ebin/', ext = '.beam')
  dir + File.basename(input_file).sub(/\.\w+$/, ext)
end

GENERATED_SRC = %w(src/compiler/reia_scan.erl src/compiler/reia_parse.erl)
ERL_SRC = (GENERATED_SRC + FileList.new('src/{compiler,core,builtins,json}/**/*.erl')).uniq
ERL_DEST = ERL_SRC.map { |input| output_file(input) }

QUIET_SRC = %w(src/compiler/reia_parse.erl)

ERL_SRC.each do |input|
  file output_file(input) => input do
    opts = ""
    opts << "+debug_info" unless QUIET_SRC.include? input
    sh "erlc #{opts} -o ebin #{input}"
  end
end

REIA_SRC  = FileList.new('src/builtins/**/*.re')
REIA_DEST = REIA_SRC.map { |input| output_file(input, 'ebin/', '.reb') }

REIA_SRC.each do |input|
  output = output_file(input, 'ebin/', '.reb')
  file output => input do
    sh "bin/reiac -o #{output} #{input}"
  end
end

# Build rules
task :build   => %w(scanner parser reia)
task :reia    => ERL_DEST + REIA_DEST
task :scanner => %w(src/leex/leex.beam src/compiler/reia_scan.erl)
task :parser  => %w(src/compiler/reia_parse.erl)

# Scanner
file "src/leex/leex.beam" => "src/leex/leex.erl" do
  sh "erlc -W0 -o src/leex src/leex/leex.erl"
end

file "src/compiler/reia_scan.erl" => %w(src/leex/leex.beam src/compiler/reia_scan.xrl) do
  erl_eval 'leex:file("src/compiler/reia_scan.xrl")', 'src/leex'
end

# Parser
file "src/compiler/reia_parse.erl" => %w(src/compiler/reia_parse.yrl) do
  erl_eval 'yecc:file("src/compiler/reia_parse.yrl", [verbose])'
end

# Test suite
task :test => :build do
  sh "bin/reia test/runner.re"
end

# Cleaning
CLEAN.include %w(src/compiler/reia_scan.erl src/compiler/reia_parse.erl)
CLEAN.include %w(ebin/* **/*.reb)