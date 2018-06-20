
require 'openssl'
require 'digest/md5'
require 'fileutils'

class Unpacker < Struct.new(:bad_data)
  def initialize
    super(false)
  end

  def decrypt key, data
    if bad_data
      data
    else
      cipher = OpenSSL::Cipher.new('aes-128-ecb')
      cipher.decrypt
      cipher.key = key
      cipher.update(data) + cipher.final
    end
  rescue OpenSSL::Cipher::CipherError => e
    warn "#{e.class}:#{e}, turning off decryption"
    self.bad_data = true
    data
  end
end

list_key = Digest::MD5.hexdigest('pack')[0, 16]
pack_key = Digest::MD5.hexdigest('battlecats')[0, 16]
list_file = ARGV.first || 'data/app/DataLocal.list'
pack_file = list_file[0...list_file.rindex('.')] + '.pack'
pack_data = File.binread(pack_file)
dir = File.basename(pack_file)
unpacker = Unpacker.new

FileUtils.mkdir_p(dir)

puts "Extracting #{pack_file}"

# Drop first line for number of files
list = unpacker.decrypt(list_key, File.binread(list_file)).lines.drop(1)
list.each do |row|
  name, offset, size = row.split(',')

  # next unless name == 'GatyaDataSetR1.csv'

  data = unpacker.decrypt(pack_key, pack_data[offset.to_i, size.to_i])
  File.binwrite("#{dir}/#{name}", data)
end
