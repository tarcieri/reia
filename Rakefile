task :default => :build 
task :build => %w[reia copy_ebin test]

rule ".beam" => ".erl" do |t|
  sh "bin/erlc -o #{File.dirname(t.name)} #{t.source}"
end

rule ".beam" => ".re" do |t|
  sh "bin/reiac -o #{t.name} #{t.source}"
end

SMEX_BEAM = FileList['src/smart_exceptions/*.erl'].sub(/.erl$/, '.beam')

task :smart_exceptions => SMEX_BEAM

SMEX_BEAM.each do |beam|
  src = beam.sub(/.beam$/, '.erl')
  file beam => src do |t|
    sh "erlc -W0 -o #{File.dirname(t.name)} #{src}"
  end
end

SOURCES = FileList.new('src/reia/*') do |fl|
  fl.include %w[*.erl *.re *.xrl *.yrl]
end

BEAMS = SOURCES.sub(/\.\w+$/, '.beam')

task :reia => [:smart_exceptions] + BEAMS

# Compile leex
file "src/leex/leex.beam" => "src/leex/leex.erl" do
  sh "erlc -W0 -o src/leex src/leex/leex.erl"
end

# Compile reia_scan using leex
file "src/reia/reia_scan.erl" => %w[src/reia/reia_scan.xrl src/leex/leex.beam] do
  sh "bin/leex src/reia/reia_scan.xrl"
end

file "src/reia/reia_scan.beam" => "src/reia/reia_scan.erl" do
  sh "erlc +debug_info +nowarn_unused_vars -o src/reia src/reia/reia_scan.erl"
end

# Compile reia_parse using yecc
file "src/reia/reia_parse.erl" => "src/reia/reia_parse.yrl" do
  sh "bin/yecc src/reia/reia_parse.yrl"
end

file "src/reia/reia_parse.beam" => "src/reia/reia_parse.erl" do
  sh "erlc +debug_info -o src/reia src/reia/reia_parse.erl"
end

# Create the ebin directory if it doesn't exist
directory "ebin"

# Copy all output BEAM files into the ebin directory
task "copy_ebin" => "ebin" do
  FileList["src/reia/*.beam"].each do |file|
    cp file, "ebin"
  end
end

task :test do
  sh "bin/reia test/runner.re"
end

task :clean do
  FileList['src/smart_exceptions/*.beam'].each { |beam| rm_f beam }
  
  rm_f "src/leex/leex.beam"
  rm_f "src/reia/reia_scan.erl"
  rm_f "src/reia/reia_parse.erl"
  
  BEAMS.each { |beam| rm_f beam }
end
