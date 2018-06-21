
require_relative '../lib/battle-cats-rolls/pack_provider'
require_relative '../lib/battle-cats-rolls/builder'

provider = BattleCatsRolls::PackProvider.new('data/7.1.0/app')
builder = BattleCatsRolls::Builder.new(provider)

p builder.gacha[293].map(&builder.cat_names.method(:[]))
