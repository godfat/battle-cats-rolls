
require_relative '../lib/battle-cats-rolls/pack_provider'
require_relative '../lib/battle-cats-rolls/cats_builder'

pack = BattleCatsRolls::PackProvider.new('data/7.1.0/app')
cats_pack = BattleCatsRolls::CatsBuilder.new(pack)

require_relative '../lib/battle-cats-rolls/extract_provider'

extract = BattleCatsRolls::ExtractProvider.new('extract/7.1.0')
cats_extract = BattleCatsRolls::CatsBuilder.new(extract)

p cats_pack == cats_extract

require_relative '../lib/battle-cats-rolls/events_reader'
require_relative '../lib/battle-cats-rolls/crystal_ball'

events = BattleCatsRolls::EventsReader.read('data/events')
ball = BattleCatsRolls::CrystalBall.from_pack_and_events(cats_pack, events)

ball.dump('build/7.1.0')
