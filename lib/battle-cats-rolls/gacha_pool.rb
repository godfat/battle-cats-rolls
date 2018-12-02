# frozen_string_literal: true

require 'forwardable'

module BattleCatsRolls
  class GachaPool < Struct.new(:cats, :gacha, :event)
    Base = 10000

    extend Forwardable

    def_delegator :cats, :dig, :dig_cat
    def_delegator :slots, :dig, :dig_slot

    %w[rare supa uber].each do |name|
      define_method(name) do
        event[name]
      end
    end

    def legend
      @legend ||= Base - rare - supa - uber
    end

    def initialize ball, event_name
      events = ball.dig('events')
      picked = events[event_name] || events.first.last
      # If there's no such event, pick the first active one

      super(
        ball.dig('cats'),
        ball.dig('gacha', picked['id']),
        picked)
    end

    def slots
      @slots ||= gacha.inject(Hash.new{|h,k|h[k]=[]}) do |result, cat_id|
        if rarity = find_rarity(cat_id)
          result[rarity] << cat_id
          result
        else
          raise "Cannot find cat: #{cat_id}"
        end
      end
    end

    def guaranteed_rolls
      @guaranteed_rolls ||=
        case
        when event['guaranteed']
          11
        when event['step_up']
          15
        else
          0
        end
    end

    def add_future_ubers amount
      range = -1.downto(-amount)

      if range.any?
        # Avoid modifying existing uber pool
        self.cats = cats.dup
        cats[Gacha::Uber] = cats[Gacha::Uber].dup

        range.each do |n|
          slots[Gacha::Uber].unshift(n)
          cats[Gacha::Uber][n] =
            {'name' => ["(#{n}?)"], 'desc' => ['An unknown future uber']}
        end
      end
    end

    private

    def find_rarity cat_id
      cats.find do |(rarity, cats)|
        break rarity if cats.member?(cat_id)
      end
    end
  end
end
