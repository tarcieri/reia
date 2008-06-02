task :default => :build 
task :build => %w[reia copy_ebin]

rule ".beam" => ".erl" do |t|
  sh "erlc +debug_info -o #{File.dirname(t.name)} #{t.source}"
end

rule ".beam" => ".ra" do |t|
  sh "bin/reiac -o #{t.name} #{t.source}"
end

SOURCES = FileList.new('src/reia/*') do |fl|
  fl.include %w[*.erl *.ra *.xrl *.yrl]
end

BEAMS = SOURCES.sub(/\.\w+$/, '.beam')

task :reia => BEAMS

# Compile leex
file "src/leex/leex.beam" => "src/leex/leex.erl" do
  sh "erlc +nowarn_unused_vars -o src/leex src/leex/leex.erl"
end

# Compile reia_scan using leex
file "src/reia/reia_scan.erl" => %w[src/reia/reia_scan.xrl src/leex/leex.beam] do
  sh "erl -eval 'leex:file(\"src/reia/reia_scan.xrl\")' -pa src/leex -noshell -s init stop"
end

file "src/reia/reia_scan.beam" => "src/reia/reia_scan.erl" do
  sh "erlc +debug_info +nowarn_unused_vars -o src/reia src/reia/reia_scan.erl"
end

# Compile reia_parse using yecc
file "src/reia/reia_parse.erl" => "src/reia/reia_parse.yrl" do
  sh "bin/yecc src/reia/reia_parse.yrl"
end

# Create the ebin directory if it doesn't exist
directory "ebin"

# Copy all output BEAM files into the ebin directory
task "copy_ebin" => "ebin" do
  FileList["src/reia/*.beam"].each do |file|
    cp file, "ebin"
  end
end

task :clean do
  rm_f "src/reia/reia_scan.erl"
  rm_f "src/reia/reia_parse.erl"
  
  BEAMS.each { |beam| rm_f beam }
end