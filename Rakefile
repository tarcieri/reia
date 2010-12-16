require 'rake/clean'

# Path on the local filesystem to install reia/ire scripts to
BIN_INSTALL_DIR = ENV['REIA_BIN_DIR'] || "/usr/local/bin"

task :default => %w(check_erl_version build test)

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

REIA_SRC  = FileList.new('src/{builtins,core}/**/*.re')
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
CLEAN.include %w(src/compiler/reia_scan.erl src/compiler/reia_parse.erl)
CLEAN.include %w(**/*.beam **/*.reb)
CLEAN.include "erl_crash.dump"

#
# Installing
#

# Retrieve the directory Erlang libraries are stored in
def erl_lib_dir
  ENV['ERL_LIB_DIR'] ||= `erl -noshell -eval "io:format(code:lib_dir())" -s erlang halt`
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
