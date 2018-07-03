
require_relative '../lib/battle-cats-rolls/pack_reader'

require 'fileutils'

reader = BattleCatsRolls::PackReader.new(
  ARGV.first || 'data/7.1.0/app/DataLocal.list')

dir = "extract/7.1.0/#{reader.name}.pack"
FileUtils.mkdir_p(dir)

puts "Extracting #{reader.pack_path}"

reader.each do |filename, data|
  File.binwrite("#{dir}/#{filename}", data.call)
end
