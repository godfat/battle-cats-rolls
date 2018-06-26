# frozen_string_literal: true

require 'forwardable'

require_relative 'gacha_pool'

module BattleCatsRolls
  class Gacha < Struct.new(:pool, :seed)
    def self.from_ball_and_event_id ball, event_id, seed
      new(GachaPool.new(ball, event_id), seed)
    end

    extend Forwardable

    def_delegators :pool, *%w[id start_on end_on name rare sr ssr]

    def roll
      rarity = roll_rarity
      slot = roll_slot(rarity)

      pool.dig_cat(rarity, pool.dig_slot(rarity, slot))
    end

    private

    def roll_rarity
      base = 10000

      case roll_score % base
      when 0...(base - sr - ssr)
        2
      when sr...(base - ssr)
        3
      else
        4
      end
    end

    def roll_slot rarity
      roll_score % pool.dig_slot(rarity).size
    end

    def roll_score
      score = seed.abs
      advance_seed
      score
    end

    def advance_seed
      self.seed = shift(:<<, 13)
      self.seed = shift(:>>, 17)
      self.seed = shift(:<<, 15)
    end

    def shift direction, bits
      shifted = seed ^ seed.public_send(direction, bits)
      (shifted + 0x80000000) % (0x100000000) - (0x80000000)
    end
  end
end
