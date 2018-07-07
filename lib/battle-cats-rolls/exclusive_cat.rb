# frozen_string_literal: true

module BattleCatsRolls
  module ExclusiveCat
    def self.ids
      [
        270, # "Baby Gao",
        319, # "Miko Mitama",
        381, # "D'artanyan",
        334, # "Shadow Gao",
        379, # "Dark Mitama",
      ]
    end

    def self.search gacha, cats: [], max: 999
      target_ids = ids

      potential_exclusives = gacha.ubers.select do |id|
        target_ids.include?(id)
      end

      if potential_exclusives.empty?
        []
      else
        found = search_from_ids(potential_exclusives, cats)

        if found.size < potential_exclusives.size
          cats.size.succ.upto(max).inject(found) do |result, sequence|
            if result.size == potential_exclusives.size
              break result
            else
              new_ab = gacha.roll_both_with_sequence!(sequence)

              next result.merge(
                search_from_ids(potential_exclusives - result.keys, [new_ab]))
            end
          end
        else
          found
        end.values
      end
    end

    def self.search_from_ids ids, cats
      cats.each.inject({}) do |result, ab|
        (ids - result.keys).each do |remaining_id|
          ab.each.with_index do |cat, a_or_b|
            result[remaining_id] = [cat, a_or_b] if remaining_id == cat.id
          end
        end

        if result.size == ids.size
          break result
        else
          next result
        end
      end
    end
  end
end
