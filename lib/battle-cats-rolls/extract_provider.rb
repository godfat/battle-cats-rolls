# frozen_string_literal: true

module BattleCatsRolls
  class ExtractProvider < Struct.new(:dir)
    def gacha
      @gacha ||= File.binread("#{dir}/DataLocal.pack/GatyaDataSetR1.csv")
    end

    def unitbuy
      @unitbuy ||= File.binread("#{dir}/DataLocal.pack/unitbuy.csv")
    end

    def res
      @res ||= Dir["#{dir}/resLocal.pack/Unit_Explanation*_en.csv"].
        inject({}) do |result, path|
          result[File.basename(path)] = File.binread(path)
          result
        end
    end
  end
end
