
require_relative '../lib/battle-cats-rolls/unpacker'

unpacker = BattleCatsRolls::Unpacker.for_list

Dir["#{ARGV.first || 'data'}/**/*.list"].each do |file|
  puts "#{file}:"
  puts unpacker.decrypt(File.binread(file))
  puts "---"
end
