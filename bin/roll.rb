
require_relative '../lib/battle-cats-rolls/crystal_ball'
require_relative '../lib/battle-cats-rolls/gacha_pool'
require_relative '../lib/battle-cats-rolls/gacha'

ball = BattleCatsRolls::CrystalBall.load('build/7.1.0')
pool = BattleCatsRolls::GachaPool.new(ball, 273)
gacha = BattleCatsRolls::Gacha.new(pool, ARGV.first.to_i)

puts "Rolling #{gacha.name}"

10.times do
  puts gacha.roll
end
