# frozen_string_literal: true

require_relative 'gacha'
require_relative 'cat'

module BattleCatsRolls
  class FindCat < Struct.new(:gacha, :ids)
    Max = 999

    def self.exclusives
      @exclusives ||= [
        270, # "Baby Gao",
        284, # "Pai-Pai",
        287, # "Strike Unit R.E.I.",
        319, # "Miko Mitama",
        381, # "D'artanyan",
        334, # "Shadow Gao",
        379, # "Dark Mitama",
        398, # "Sakura Sonic",
        436, # "Li'l Valkyrie",
        442, # "D'arktanyan",
      ].freeze
    end

    def self.search gacha, find, **args
      new(gacha, exclusives + [find]).search(**args)
    end

    def initialize new_gacha, target_ids
      ids_in_gacha = %i[rare_cats supa_cats uber_cats legend_cats].
        flat_map(&new_gacha.method(:public_send)).select do |cat|
          target_ids.include?(cat.id)
        end.map(&:id)

      ids_in_gacha.concat(new_gacha.legend_cats.map(&:id))

      super(new_gacha, ids_in_gacha)
    end

    def search cats: [], guaranteed: true, max: Max
      if ids.empty?
        []
      else
        found = search_deep(cats, guaranteed, max)

        if found.size < ids.size
          found.values + (ids - found.keys).map do |missing_id|
            info = [Gacha::Legend, Gacha::Uber,
                    Gacha::Supa, Gacha::Rare].find do |rarity|
              found = gacha.pool.dig_cat(rarity, missing_id)
              break found if found
            end

            cat = Cat.new(missing_id, info)
            cat.sequence = max

            cat
          end
        else
          found.values
        end
      end
    end

    private

    def search_deep cats, guaranteed, max
      found = search_from_cats(cats, guaranteed, ids)

      if found.size < ids.size
        search_from_rolling(found, cats, guaranteed, max)
      else
        found
      end
    end

    def search_from_cats cats, guaranteed, remaining_ids
      cats.each.inject({}) do |result, ab|
        (remaining_ids - result.keys).each do |id|
          ab.each.with_index do |cat|
            case id
            when cat.id
              result[id] = cat
            when cat.guaranteed&.id
              result[id] = cat.guaranteed if guaranteed
            end
          end
        end

        if result.size == remaining_ids.size
          break result
        else
          next result
        end
      end
    end

    def search_from_rolling found, cats, guaranteed, max
      cats.size.succ.upto(max).inject(found) do |result, sequence|
        if result.size == ids.size
          break result
        else
          new_ab = gacha.roll_both_with_sequence!(sequence)
          # TODO: gacha.fill_guaranteed([new_ab])
          # will not work here because it's trying to fill guaranteed cats
          # with existing information, yet we're rolling one by one here,
          # thus guaranteed information doesn't exist.
          # We could fix this by rolling 11 times for each attempt

          next result.merge(
            search_from_cats([new_ab], guaranteed, ids - result.keys))
        end
      end
    end
  end
end
