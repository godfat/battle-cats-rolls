
require_relative 'pack_reader'

module BattleCatsRolls
  class Builder < Struct.new(:data_reader, :res_reader)

    def initialize dir
      super(
        PackReader.new("#{dir}/DataLocal.list"),
        PackReader.new("#{dir}/resLocal.list"))
    end

    def data_local
      @data_local ||= data_reader.list_lines.
        grep(/\A(?:GatyaDataSetR1|unitbuy)\.csv,\d+,\d+$/).
        inject({}) do |result, line|
          filename, data = data_reader.read(line)

          case filename
          when 'GatyaDataSetR1.csv'
            result[:gacha] = store_gacha(data)
          when 'unitbuy.csv'
            result[:rarity] = store_rarity(data)
          end

          result
        end
    end

    def cat_names
      @cat_names ||= res_reader.list_lines.
        grep(/\AUnit_Explanation\d+_en\.csv,\d+,\d+$/).
        inject({}) do |result, line|
          result.store(*read_cat_name(line))
          result
        end.compact
    end

    private

    def store_gacha data
      data.lines.each.with_index.inject({}) do |result, (line, index)|
        next result unless line =~ /\A\d+/

        slots = line.split(',')
        id = slots.pop until slots.empty? || id == '-1'
        result[index] = slots.map { |s| Integer(s) + 1 }
        result
      end
    end

    def store_rarity data
      data.lines.each.with_index.inject({}) do |result, (line, index)|
        result[index + 1] = Integer(line.match(/\A(?:\d+,){13}(\d+)/)[1])
        result
      end
    end

    def read_cat_name line
      filename, data = res_reader.read(line)

      [Integer(filename[/\d+/]), data[/\A[^\|]+/]]
    end
  end
end
