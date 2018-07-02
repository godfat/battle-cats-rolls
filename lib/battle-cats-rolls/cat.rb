# frozen_string_literal: true

module BattleCatsRolls
  class Cat < Struct.new(:rarity, :name, :score)
    def to_s
      "\e[#{color_sequence}m#{name}\e[0m"
    end

    private

    def color_sequence
      case score
      when 0...6500
        '00'
      when 6500...7000
        '33;1'
      when 7000...9100
        '33'
      when 9100...9500
        '32;1'
      else
        '32'
      end
    end
  end
end
