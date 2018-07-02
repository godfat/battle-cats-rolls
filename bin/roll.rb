
require_relative '../lib/battle-cats-rolls/crystal_ball'
require_relative '../lib/battle-cats-rolls/gacha'

ball = BattleCatsRolls::CrystalBall.load('build/7.1.0')

[273, 302].each do |event_id|
  gacha = BattleCatsRolls::Gacha.new(ball, event_id, ARGV.first.to_i)

  puts "Rolling (#{gacha.ubers.size} ubers) #{gacha.name}"

  1.upto(100) do |i|
    a, b = gacha.roll_both!
    printf "%3d: %30s %30s\n", i, a, b
  end

  puts
end
