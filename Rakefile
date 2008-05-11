task :default => :build
task :build => %w[leex]

rule ".beam" => ".erl" do |t|
  sh "erlc +nowarn_unused_vars -o #{File.dirname(t.name)} #{t.source}"
end

task :leex => %w[src/leex/leex.beam src/reia/scanner.erl]

file "src/leex/leex.beam" => "src/leex/leex.erl"
file "src/reia/scanner.erl" => %w[src/reia/scanner.xrl src/leex/leex.beam] do
  sh "erl -eval 'leex:file(\"src/reia/scanner.xrl\")' -pa src/leex -noshell -s init stop"
end

task :clean do
  rm_f 'src/leex/leex.beam'
  rm_f 'src/reia/scanner.erl'
  rm_rf 'ebin'
end
