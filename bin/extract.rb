
require_relative '../lib/battle-cats-rolls/unpacker'

require 'digest/md5'
require 'fileutils'

list_key = Digest::MD5.hexdigest('pack')[0, 16]
pack_key = Digest::MD5.hexdigest('battlecats')[0, 16]
list_file = ARGV.first || 'data/7.1.0/app/DataLocal.list'
pack_file = list_file[0...list_file.rindex('.')] + '.pack'
pack_data = File.binread(pack_file)
dir = "extract/7.1.0/#{File.basename(pack_file)}"
unpacker = BattleCatsRolls::Unpacker.new(pack_key)

FileUtils.mkdir_p(dir)

puts "Extracting #{pack_file}"

# Drop first line for number of files
list = BattleCatsRolls::Unpacker.new(list_key).
  decrypt(File.binread(list_file)).lines.drop(1)

list.each do |row|
  name, offset, size = row.split(',')

  data = unpacker.decrypt(pack_data[offset.to_i, size.to_i])
  File.binwrite("#{dir}/#{name}", data)
end
