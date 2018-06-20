
require_relative '../lib/battle-cats-rolls/unpacker'

require 'digest/md5'

key = Digest::MD5.hexdigest('pack')[0, 16]
unpacker = BattleCatsRolls::Unpacker.new(key)

Dir["#{ARGV.first || 'data'}/**/*.list"].each do |file|
  puts "#{file}:"
  puts unpacker.decrypt(File.binread(file))
  puts "---"
end
