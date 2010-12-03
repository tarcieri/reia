require 'rake/clean'

task :default => %w(check_erl_version check_previous_install build)

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

# Neotoma (PEG for Erlang)
NEOTOMA_FILES = %w(neotoma neotoma_parse neotoma_peg)
task :neotoma => NEOTOMA_FILES.map { |f| "src/neotoma/ebin/#{f}.beam" }

NEOTOMA_FILES.each do |f|
  input = "src/neotoma/src/#{f}.erl"
  file "src/neotoma/ebin/#{f}.beam" => input do
    sh "erlc -o src/neotoma/ebin #{input}"
  end
end

# Parser
file "src/compiler/reia_parse.erl" => "src/compiler/reia_parse.peg" do
  erl_eval 'neotoma:file("src/compiler/reia_parse.peg")', 'src/neotoma/ebin'
end

# Generate an output path for the given input file
def output_file(input_file, dir = 'ebin/')
  dir + File.basename(input_file).sub(/\.\w+$/, '.beam')
end

GENERATED_SRC = %w(src/compiler/reia_parse.erl)
<<<<<<< HEAD
ERL_SRC = (GENERATED_SRC + FileList.new('src/{compiler,core}/**/*.erl')).uniq
=======
PARSER_TEST_SRC = %w(test/parser/parser_test.erl)
ERL_SRC = (GENERATED_SRC + PARSER_TEST_SRC + FileList.new('src/{compiler,core,builtins,json}/**/*.erl')).uniq
ERL_DEST = ERL_SRC.map { |input| output_file(input) }

QUIET_SRC = %w(src/compiler/reia_parse.erl)
>>>>>>> Added unit tests for parser

ERL_SRC.each do |input|
  file output_file(input) => input do
    sh "erlc +debug_info -o ebin #{input}"
  end
end

# Build rules
task :build   => %w(parser reia)
task :parser  => %w(neotoma src/compiler/reia_parse.erl)
<<<<<<< HEAD
task :reia    => ERL_SRC.map { |input_file| output_file(input_file) }

# Cleaning
CLEAN.include %w(ebin/* src/neotoma/ebin/* src/compiler/reia_parse.erl)
=======
task :reia    => ERL_DEST + REIA_DEST

# Test suite
task :test => :build do
  erl_eval 'eunit:test(parser_test)', 'ebin'
  sh "bin/reia test/runner.re"
end

# Benchmarks
BENCHMARK_SRC = FileList.new('benchmarks/**/*.erl')
BENCHMARK_DEST = BENCHMARK_SRC.map { |input| output_file(input, 'benchmarks/ebin/') }

BENCHMARK_SRC.each do |input|
  file output_file(input, 'benchmarks/ebin/') => input do
    sh "erlc +debug_info -o benchmarks/ebin #{input}"
  end
end

task :benchmark => BENCHMARK_DEST do
  sh "bin/reia benchmarks/runner.re"
end

# Cleaning
CLEAN.include %w(ebin/* src/neotoma/ebin/* src/compiler/reia_parse.erl)
CLEAN.include %w(**/*.beam **/*.reb)
CLEAN.include "erl_crash.dump"

#
# Installing
#

# Retrieve the directory Erlang libraries are stored in
def erl_lib_dir
  $erl_lib_dir ||= `erl -noshell -eval "io:format(code:lib_dir())" -s erlang halt`
end


# Directory to install Reia into
def reia_install_dir
  File.join(erl_lib_dir, 'reia', '')
end

# Munge Reia launcher scripts before installing
def munge_script(src, dest)
  str = File.read(src)
  
  # Remove REIA_HOME declaration
  str.gsub!(/^export REIA_HOME=.*$/, '')
  
  # Remove EXTRA_PATHS declaration
  str.gsub!(/^EXTRA_PATHS=.*$/, '')
  
  # Remove $EXTRA_PATHS variables
  str.gsub!(/\$EXTRA_PATHS/, '')
  
  # Strip all the extraneous newlines
  str.gsub!(/\n\n+/m, "\n\n")
  
  File.open(dest, "w", 0755) { |file| file << str }
end

directory BIN_INSTALL_DIR

task :install => [:check_erl_version, :build, BIN_INSTALL_DIR] do
  reia_dir = reia_install_dir
  STDERR.puts "*** Installing Reia into: #{reia_dir}"
  
  rm_r reia_dir if File.exist?(reia_dir)
  mkdir reia_dir
  cp_r "ebin", reia_dir
  
  %w[ire reia reiac].each do |script|
    src = File.expand_path("../bin/#{script}", __FILE__)
    dst = "#{BIN_INSTALL_DIR}/#{script}"
    
    STDERR.puts "Creating #{dst}"
    munge_script src, dst
  end
end

task :uninstall do
  reia_dir = reia_install_dir

  rm_r reia_dir if File.directory?(reia_dir)
  %w[ire reia reiac].each { |script| rm_f "#{BIN_INSTALL_DIR}/#{script}" }
end
>>>>>>> Added unit tests for parser
