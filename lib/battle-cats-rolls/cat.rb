# frozen_string_literal: true

module BattleCatsRolls
  class Cat < Struct.new(:rarity, :name)
    def to_s
      "\e[#{color_sequence}m#{name}\e[0m"
    end

    def rarity_label
      "#{'S' * (rarity - 1)}R"
    end

    private

    def color_sequence
      case rarity
      when 3
        33
      when 4
        32
      else
        '00'
      end
    end
  end
end
