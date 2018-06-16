
def count_rarity data, rarity
  data.count{|_, row|row['rarity'] == rarity}
end

def find_slot data, rarity, slot
  sorted_data = data.sort_by{|_, row|row['order']}
  sorted_data.select{|(_, row)|row['rarity'] == rarity}.dig(slot, 0)
end

def roll seed, sr, ssr
  score = seed % 10000 / 100
  seed = next_seed(seed)

  [seed,
     case score
     when 0...(100 - sr - ssr)
       0
     when sr...(100 - ssr)
       1
     else
       2
     end]
end

def next_seed seed
  seed = shift(seed, :<<, 13)
  seed = shift(seed, :>>, 17)
  seed = shift(seed, :<<, 15)
end

def shift seed, direction, bits
  seed ^= seed.public_send(direction, bits)
  (seed + 0x80000000) % (0x100000000) - (0x80000000)
end

require 'json'

SR_RATE = 26
SSR_RATE = 9
GUARANTEED_DRAWS = 11

data = JSON.parse(File.read('epicfest.json'))
seed = ARGV.first.to_i
seed_rarity, rarity = roll(seed, SR_RATE, SSR_RATE)
slot = seed_rarity.abs % count_rarity(data, rarity)
puts find_slot(data, rarity, slot)

guaranteed_seed = ((GUARANTEED_DRAWS - 1) * 2).times.inject(seed) do |current|
  next_seed(current)
end
puts find_slot(data, 2, guaranteed_seed.abs % count_rarity(data, 2))
