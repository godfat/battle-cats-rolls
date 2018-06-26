
require_relative '../lib/battle-cats-rolls/pack_provider'
require_relative '../lib/battle-cats-rolls/cats_builder'

pack = BattleCatsRolls::PackProvider.new('data/7.1.0/app')
cats_pack = BattleCatsRolls::CatsBuilder.new(pack)

p cats_pack.gacha[293].map(&cats_pack.cat_names.method(:[]))

require_relative '../lib/battle-cats-rolls/extract_provider'

extract = BattleCatsRolls::ExtractProvider.new('extract/7.1.0')
cats_extract = BattleCatsRolls::CatsBuilder.new(extract)

p cats_pack == cats_extract

require_relative '../lib/battle-cats-rolls/tsv_reader'

events = BattleCatsRolls::TsvReader.read('data/events/20180707.tsv')

p events.gacha
p events == BattleCatsRolls::TsvReader.current

require_relative '../lib/battle-cats-rolls/crystal_ball'

ball = BattleCatsRolls::CrystalBall.from_pack_and_events(cats_pack, events)

ball.dump('build/7.1.0')
