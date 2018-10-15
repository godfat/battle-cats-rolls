# frozen_string_literal: true

module BattleCatsRolls
  class CatsBuilder < Struct.new(:provider)
    def cats
      @cats ||= Hash[build_cats.sort]
    end

    def gacha
      @gacha ||= store_gacha(provider.gacha)
    end

    def cat_names
      @cat_names ||= store_cat_names(provider.res)
    end

    def rarities
      @rarities ||= store_rarities(provider.unitbuy)
    end

    def == rhs
      cats == rhs.cats && gacha == rhs.gacha
    end

    private

    def build_cats
      rarities.inject(Hash.new{|h,k|h[k]={}}) do |result, (id, rarity)|
        name = cat_names[id]
        result[rarity].merge!(id => name) if name
        result
      end
    end

    def store_gacha data
      data.lines.each.with_index.inject({}) do |result, (line, index)|
        next result unless line =~ /\A\d+/

        slots = line.split(',')
        id = slots.pop until slots.empty? || id == '-1'
        result[index] = slots.map { |s| Integer(s) + 1 }
        result
      end
    end

    def store_rarities data
      data.lines.each.with_index.inject({}) do |result, (line, index)|
        result[index + 1] = Integer(line.match(/\A(?:\d+,){13}(\d+)/)[1])
        result
      end
    end

    def store_cat_names res_local
      res_local.inject({}) do |result, (filename, data)|
        separator =
          if filename.end_with?('_ja.csv')
            ','
          else
            '|'
          end

        names = data.scan(/^(?:[^#{separator}]+)/).uniq
        result[Integer(filename[/\d+/])] = names if names.any?
        result
      end.compact
    end
  end
end
