
require_relative '../lib/battle-cats-rolls/pack_reader'

require 'fileutils'

reader = BattleCatsRolls::PackReader.new('data/7.1.0/app/ImageLocal.list')

dir = "extract/7.1.0/#{reader.name}.pack"
asset = "lib/battle-cats-rolls/asset/image"
FileUtils.mkdir_p(dir)
FileUtils.mkdir_p(asset)

puts "Extracting #{reader.pack_path}"

mapicon, data = reader.find do |filename, _|
  filename == 'mapicon.png'
end

path = "#{dir}/#{mapicon}"

File.binwrite(path, data.call)

puts "Cropping #{path}"

# Install ImageMagick for this
p system('convert', '-crop', '60x60+60+0', path, "#{asset}/treasure.png")
