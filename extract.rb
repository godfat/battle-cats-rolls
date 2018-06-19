
require 'openssl'
require 'digest/md5'
require 'fileutils'

def decrypt key, data
  cipher = OpenSSL::Cipher.new('aes-128-ecb')
  cipher.decrypt
  cipher.key = key
  cipher.update(data) + cipher.final
end

list_key = Digest::MD5.hexdigest('pack')[0, 16]
pack_key = Digest::MD5.hexdigest('battlecats')[0, 16]
list_file = 'data/app/DataLocal.list'
pack_file = list_file[0...list_file.rindex('.')] + '.pack'
pack_data = File.binread(pack_file)
dir = File.basename(list_file)

FileUtils.mkdir_p(dir)

# Drop first line for number of files
list = decrypt(list_key, File.binread(list_file)).lines.drop(1)
list.each do |row|
  name, offset, size = row.split(',')

  next unless name == 'GatyaDataSetR1.csv'

  data = decrypt(pack_key, pack_data[offset.to_i, size.to_i])
  File.binwrite("#{dir}/#{name}", data)
end
