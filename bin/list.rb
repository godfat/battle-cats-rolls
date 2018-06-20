
require_relative '../lib/battle-cats-rolls/unpacker'

require 'digest/md5'

def decrypt key, data
  cipher = OpenSSL::Cipher.new('aes-128-ecb')
  cipher.decrypt
  cipher.key = key
  cipher.update(data) + cipher.final
end

key = Digest::MD5.hexdigest('pack')[0, 16]
unpacker = BattleCatsRolls::Unpacker.new

Dir["#{ARGV.first || 'data'}/**/*.list"].each do |file|
  puts "#{file}:"
  puts unpacker.decrypt(key, File.binread(file))
  puts "---"
end
