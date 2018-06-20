
require_relative '../lib/battle-cats-rolls/extractor'
require_relative '../lib/battle-cats-rolls/unpacker'

require 'fileutils'

extractor = BattleCatsRolls::Extractor.new(
  ARGV.first || 'data/7.1.0/app/DataLocal.list')

unpacker = BattleCatsRolls::Unpacker.for_pack

dir = "extract/7.1.0/#{File.basename(extractor.pack_path)}"
FileUtils.mkdir_p(dir)

puts "Extracting #{extractor.pack_path}"

# Drop first line for number of files
list = BattleCatsRolls::Unpacker.for_list.
  decrypt(extractor.list_data).lines.drop(1)

list.each do |row|
  name, offset, size = row.split(',')

  data = unpacker.decrypt(extractor.pack_data[offset.to_i, size.to_i])
  File.binwrite("#{dir}/#{name}", data)
end
