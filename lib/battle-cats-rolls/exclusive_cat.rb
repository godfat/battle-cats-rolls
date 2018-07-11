# frozen_string_literal: true

require_relative 'gacha'
require_relative 'cat'

module BattleCatsRolls
  class ExclusiveCat < Struct.new(:gacha, :ids)
    def self.ids
      [
        270, # "Baby Gao",
        319, # "Miko Mitama",
        381, # "D'artanyan",
        334, # "Shadow Gao",
        379, # "Dark Mitama",
      ]
    end

    def self.search gacha, **args
      new(gacha, ids).search(**args)
    end

    def initialize new_gacha, target_ids
      new_ids = new_gacha.uber_cats.select do |id|
        target_ids.include?(id)
      end

      super(new_gacha, new_ids)
    end

    def search cats: [], max: 999
      if ids.empty?
        []
      else
        found = search_deep(cats, max)

        if found.size < ids.size
          track_name = '+'.ord - 'A'.ord

          found.values + (ids - found.keys).map do |missing_id|
            cat =
              Cat.new(missing_id, gacha.pool.dig_cat(Gacha::Uber, missing_id))
            cat.sequence = max

            [cat, track_name]
          end
        else
          found.values
        end
      end
    end

    private

    def search_deep cats, max
      found = search_from_cats(cats, ids)

      if found.size < ids.size
        search_from_rolling(found, cats, max)
      else
        found
      end
    end

    def search_from_cats cats, remaining_ids
      cats.each.inject({}) do |result, ab|
        (remaining_ids - result.keys).each do |id|
          ab.each.with_index do |cat, a_or_b|
            result[id] = [cat, a_or_b] if id == cat.id
          end
        end

        if result.size == remaining_ids.size
          break result
        else
          next result
        end
      end
    end

    def search_from_rolling found, cats, max
      cats.size.succ.upto(max).inject(found) do |result, sequence|
        if result.size == ids.size
          break result
        else
          new_ab = gacha.roll_both_with_sequence!(sequence)

          next result.merge(
            search_from_cats([new_ab], ids - result.keys))
        end
      end
    end
  end
end
