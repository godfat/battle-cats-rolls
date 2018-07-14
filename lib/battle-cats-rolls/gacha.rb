# frozen_string_literal: true

require 'forwardable'

require_relative 'cat'
require_relative 'gacha_pool'

module BattleCatsRolls
  class Gacha < Struct.new(:pool, :seed)
    Base = 10000
    Rare = 2
    SR   = 3
    Uber = 4

    extend Forwardable

    def_delegators :pool, *%w[id start_on end_on name rare sr ssr]

    def initialize crystal_ball, event_name, seed
      super(GachaPool.new(crystal_ball, event_name), seed)
    end

    def roll_both_with_sequence! sequence
      roll_both!.each do |cat|
        cat.sequence = sequence
      end
    end

    def roll_both!
      a_int = roll_int!
      b_int = roll_int
      a_cat = roll_cat!(a_int)
      b_cat = roll_cat(b_int)

      [a_cat, b_cat]
    end

    def roll!
      roll_cat!(roll_int!)
    end

    def fill_guaranteed cats
      guaranteed_rolls = pool.guaranteed_rolls

      if guaranteed_rolls > 0
        cats.each.with_index do |ab, index|
          ab.each.with_index do |rolled_cat, a_or_b|
            guaranteed_slot_seed =
              cats.dig(index + guaranteed_rolls - 1, a_or_b, :rarity_seed)

            rolled_cat.guaranteed = dig_cat(guaranteed_slot_seed, Uber) if
              guaranteed_slot_seed
          end
        end
      end

      guaranteed_rolls
    end

    def rare_cats
      pool.dig_slot(Rare)
    end

    def sr_cats
      pool.dig_slot(SR)
    end

    def uber_cats
      pool.dig_slot(Uber)
    end

    private

    def roll_int
      seed
    end

    def roll_int!
      roll_int.tap{ advance_seed! }
    end

    def roll_cat rarity_seed
      score = rarity_seed.abs % Base
      rarity = if pool.platinum then Uber else dig_rarity(score) end
      slot_seed = if block_given? then yield else roll_int end
      cat = dig_cat(slot_seed, rarity)

      cat.score = score
      cat.rarity = rarity
      cat.rarity_seed = rarity_seed
      cat.slot_seed = slot_seed

      cat
    end

    def roll_cat! int_rarity
      roll_cat(int_rarity){ roll_int! }
    end

    def dig_rarity score
      case score
      when 0...rare
        Rare
      when rare...(Base - ssr)
        SR
      else
        Uber
      end
    end

    def dig_cat slot_seed, rarity
      slot = slot_seed.abs % pool.dig_slot(rarity).size
      id = pool.dig_slot(rarity, slot)

      Cat.new(id, pool.dig_cat(rarity, id))
    end

    def advance_seed!
      self.seed = advance_seed
    end

    def advance_seed base_seed=seed
      base_seed = shift(:<<, 13, base_seed)
      base_seed = shift(:>>, 17, base_seed)
      base_seed = shift(:<<, 15, base_seed)
    end

    def shift direction, bits, base_seed=seed
      base_seed ^= base_seed.public_send(direction, bits)
      (base_seed + 0x80000000) % (0x100000000) - (0x80000000)
    end
  end
end
