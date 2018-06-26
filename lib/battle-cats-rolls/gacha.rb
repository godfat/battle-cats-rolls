# frozen_string_literal: true

require 'forwardable'

module BattleCatsRolls
  class Gacha < Struct.new(:pool, :seed)
    extend Forwardable

    def roll
      rarity = roll_rarity
      slot = roll_slot(rarity)

      pool.ball.dig('cats', rarity, pool.dig(rarity, slot))
    end

    def sr
      pool.info['sr']
    end

    def ssr
      pool.info['ssr']
    end

    private

    def roll_rarity
      case roll_score % 10000
      when 0...(10000 - sr - ssr)
        2
      when sr...(10000 - ssr)
        3
      else
        4
      end
    end

    def roll_slot rarity
      roll_score % pool.dig(rarity).size
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
