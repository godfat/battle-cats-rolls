
require_relative '../lib/battle-cats-rolls/crystal_ball'
require_relative '../lib/battle-cats-rolls/gacha'

ball = BattleCatsRolls::CrystalBall.load('build/7.1.0')

[302].each do |event_id|
  gacha = BattleCatsRolls::Gacha.new(ball, event_id, ARGV.first.to_i)

  puts "Rolling (#{gacha.ubers.size} ubers) #{gacha.name}"

  10.times do
    puts gacha.roll
  end

  puts
end
