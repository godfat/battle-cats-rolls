
require 'openssl'
require 'digest/md5'

def decrypt key, data
  cipher = OpenSSL::Cipher.new('aes-128-ecb')
  cipher.decrypt
  cipher.key = key
  cipher.update(data) + cipher.final
end

key = Digest::MD5.hexdigest('pack')[0, 16]

Dir["#{ARGV.first || 'data'}/**/*.list"].each do |file|
  puts "#{file}:"
  puts decrypt(key, File.binread(file))
  puts "---"
end
