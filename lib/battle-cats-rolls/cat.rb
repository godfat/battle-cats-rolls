# frozen_string_literal: true

module BattleCatsRolls
  class Cat < Struct.new(
    :id, :all_names,
    :slot, :slot_fruit,
    :rarity, :rarity_fruit,
    :score, :sequence, :guaranteed)

    def name
      all_names.first
    end

    def == rhs
      id == rhs.id
    end

    def color_label
      case score
      when 0...6500
        :rare
      when 6500...7000
        :ssr_fest
      when 7000...9100
        :ssr
      when 9100...9500
        :uber_fest
      else
        :uber
      end
    end

    def to_ansi
      "\e[#{color_sequence}m#{name}\e[0m"
    end

    private

    def color_sequence
      case color_label
      when :rare
        '00;3'
      when :ssr_fest
        '33;1'
      when :ssr
        '33;3'
      when :uber_fest
        '32;1'
      when :uber
        '32;3'
      end
    end
  end
end
