
require_relative '../lib/battle-cats-rolls/builder'

builder = BattleCatsRolls::Builder.new('data/7.1.0/app')

p builder.data_local.dig(:gacha, 293).map(&builder.cat_names.method(:[]))
