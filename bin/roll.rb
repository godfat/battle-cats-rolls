
require_relative '../lib/battle-cats-rolls/crystal_ball'
require_relative '../lib/battle-cats-rolls/gacha'

ball = BattleCatsRolls::CrystalBall.load('build/7.1.0')

%w[2018-07-05_273 2018-07-14_302].each do |event_name|
  gacha = BattleCatsRolls::Gacha.new(ball, event_name, ARGV.first.to_i)

  puts "Rolling (#{gacha.uber_slots.size} ubers) #{gacha.name}"

  1.upto(100) do |i|
    a, b = gacha.roll_both!
    printf "%3d: %35s %35s\n", i, a.to_ansi, b.to_ansi
  end

  puts
end
