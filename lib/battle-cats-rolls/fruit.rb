# frozen_string_literal: true

module BattleCatsRolls
  class Fruit < Struct.new(:seed)
    def value
      @value ||= [seed, alternative_seed].min
    end

    private

    def alternative_seed
      0x100000000 - seed
    end
  end
end
