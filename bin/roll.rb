
require_relative '../lib/battle-cats-rolls/crystal_ball'
require_relative '../lib/battle-cats-rolls/gacha'

ball = BattleCatsRolls::CrystalBall.load('build/7.1.0')

[273, 292].each do |event_id|
  gacha = BattleCatsRolls::Gacha.
    from_ball_and_event_id(ball, event_id, ARGV.first.to_i)

  puts "Rolling #{gacha.name}"

  10.times do
    puts gacha.roll
  end

  puts
end
