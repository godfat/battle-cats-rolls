# frozen_string_literal: true

require_relative 'cat'
require_relative 'fruit'
require_relative 'gacha_pool'

require 'forwardable'

module BattleCatsRolls
  class Gacha < Struct.new(:pool, :seed)
    Rare   = 2
    Supa   = 3
    Uber   = 4
    Legend = 5

    extend Forwardable

    def_delegators :pool, *%w[rare supa uber legend]

    def initialize crystal_ball, event_name, seed
      super(GachaPool.new(crystal_ball, event_name), seed)
    end

    %w[Rare Supa Uber Legend].each do |rarity|
      define_method("#{rarity.downcase}_cats") do
        name = "@#{__method__}"

        instance_variable_get(name) ||
          instance_variable_set(name,
            pick_cats(self.class.const_get(rarity)))
      end
    end

    def current_seed_mode!
      advance_seed!
    end

    def roll_both_with_sequence! sequence
      roll_both!.each do |cat|
        cat.sequence = sequence
      end
    end

    def roll_both!
      a_fruit = roll_fruit!
      b_fruit = roll_fruit
      a_cat = roll_cat!(a_fruit)
      b_cat = roll_cat(b_fruit)

      [a_cat, b_cat]
    end

    def roll!
      roll_cat!(roll_fruit!)
    end

    def fill_guaranteed cats, guaranteed_rolls=pool.guaranteed_rolls
      if guaranteed_rolls > 0
        cats.each.with_index do |ab, index|
          ab.each.with_index do |rolled_cat, a_or_b|
            guaranteed_slot_fruit =
              cats.dig(index + guaranteed_rolls - 1, a_or_b, :rarity_fruit)

            if guaranteed_slot_fruit
              rolled_cat.guaranteed = dig_cat(guaranteed_slot_fruit, Uber)
              rolled_cat.guaranteed.sequence = rolled_cat.sequence
            end
          end
        end
      end

      guaranteed_rolls
    end

    private

    def pick_cats rarity
      pool.dig_slot(rarity).map do |id|
        Cat.new(id, pool.dig_cat(rarity, id))
      end
    end

    def roll_fruit
      Fruit.new(seed)
    end

    def roll_fruit!
      roll_fruit.tap{ advance_seed! }
    end

    def roll_cat rarity_fruit
      score = rarity_fruit.value % GachaPool::Base
      rarity = dig_rarity(score)
      slot_fruit = if block_given? then yield else roll_fruit end
      cat = dig_cat(slot_fruit, rarity)

      cat.rarity_fruit = rarity_fruit
      cat.score = score

      cat
    end

    def roll_cat! rarity_fruit
      roll_cat(rarity_fruit){ roll_fruit! }
    end

    def dig_rarity score
      rare_supa = rare + supa

      case score
      when 0...rare
        Rare
      when rare...rare_supa
        Supa
      when rare_supa...(rare_supa + uber)
        Uber
      else
        Legend
      end
    end

    def dig_cat slot_fruit, rarity
      slot = slot_fruit.value % pool.dig_slot(rarity).size
      id = pool.dig_slot(rarity, slot)

      Cat.new(id, pool.dig_cat(rarity, id), slot, slot_fruit, rarity)
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
      base_seed ^= base_seed.public_send(direction, bits) % 0x100000000
    end
  end
end
