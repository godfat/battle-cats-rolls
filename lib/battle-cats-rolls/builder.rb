
module BattleCatsRolls
  class Builder < Struct.new(:provider)
    def gacha
      @gacha ||= store_gacha(provider.gacha)
    end

    def rarity
      @rarity ||= store_rarity(provider.rarity)
    end

    def cat_names
      @cat_names ||= store_cat_names(provider.res)
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

    def store_cat_names res_local
      res_local.inject({}) do |result, (filename, data)|
        result[Integer(filename[/\d+/])] = data[/\A[^\|]+/]
        result
      end
    end
  end
end
