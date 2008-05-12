task :default => :build
task :build => %w[compiler]

rule ".beam" => ".erl" do |t|
  sh "erlc +nowarn_unused_vars -o #{File.dirname(t.name)} #{t.source}"
end

task :compiler => %w[src/reia/compiler/reia_scan.beam]

# Compile the Reia reia_scan from the generated Erlang code
file "src/reia/compiler/reia_scan.beam" => "src/reia/compiler/reia_scan.erl"

# Compile leex
file "src/leex/leex.beam" => "src/leex/leex.erl"

# Compile the Reia reia_scan to Erlang code using leex
file "src/reia/compiler/reia_scan.erl" => %w[src/reia/compiler/reia_scan.xrl src/leex/leex.beam] do
  sh "erl -eval 'leex:file(\"src/reia/compiler/reia_scan.xrl\")' -pa src/leex -noshell -s init stop"
end

task :clean do
  rm_f 'src/leex/leex.beam'
  rm_f 'src/reia/compiler/reia_scan.erl'
  rm_f 'src/reia/compiler/reia_scan.beam'
end
