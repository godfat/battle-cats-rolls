
require_relative '../lib/battle-cats-rolls/pack_provider'
require_relative '../lib/battle-cats-rolls/builder'

pack_provider = BattleCatsRolls::PackProvider.new('data/7.1.0/app')
pack_builder = BattleCatsRolls::Builder.new(pack_provider)

p pack_builder.gacha[293].map(&pack_builder.cat_names.method(:[]))

pack_builder.dump('build/7.1.0')

require_relative '../lib/battle-cats-rolls/extract_provider'

extract_provider = BattleCatsRolls::ExtractProvider.new('extract/7.1.0')
extract_builder = BattleCatsRolls::Builder.new(extract_provider)

p pack_builder == extract_builder

require_relative '../lib/battle-cats-rolls/tsv_reader'

reader = BattleCatsRolls::TsvReader.read('data/events/20180707.tsv')
current = BattleCatsRolls::TsvReader.current

p reader.gacha
p reader == current
