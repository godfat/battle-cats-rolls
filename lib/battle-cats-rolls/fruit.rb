# frozen_string_literal: true

module BattleCatsRolls
  class Fruit < Struct.new(:seed)
    def value
      @value ||= [seed, alternative_seed].min
    end

    private

    def alternative_seed
      alt = if seed < 0x80000000 then 0x80000000 - seed else seed end

      0x100000000 - alt
    end
  end
end
