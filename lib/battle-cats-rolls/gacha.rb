# frozen_string_literal: true

require 'forwardable'

require_relative 'gacha_pool'

module BattleCatsRolls
  class Gacha < Struct.new(:pool, :seed)
    extend Forwardable

    def_delegators :pool, *%w[id start_on end_on name rare sr ssr]

    def initialize crystal_ball, event_id, seed
      super(GachaPool.new(crystal_ball, event_id), seed)
    end

    def roll!
      rarity = roll_rarity!
      slot = roll_slot!(rarity)

      dig_cat(rarity, slot)
    end

    def ubers
      pool.dig_slot(4)
    end

    private

    def dig_cat rarity, slot
      pool.dig_cat(rarity, pool.dig_slot(rarity, slot))
    end

    def roll_rarity!
      base = 10000

      case roll_score! % base
      when 0...(base - sr - ssr)
        2
      when sr...(base - ssr)
        3
      else
        4
      end
    end

    def roll_slot! rarity
      roll_score! % pool.dig_slot(rarity).size
    end

    def roll_score!
      score = seed.abs
      advance_seed!
      score
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
