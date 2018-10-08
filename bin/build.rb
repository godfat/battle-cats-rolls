
require_relative '../lib/battle-cats-rolls/runner'

if ARGV.empty?
  %w[en tw jp].each do |lang|
    puts "Building data for #{lang}..."
    BattleCatsRolls::Runner.build(lang)
  end
else
  BattleCatsRolls::Runner.build(*ARGV)
end
