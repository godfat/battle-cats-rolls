# frozen_string_literal: true

module BattleCatsRolls
  class Cat < Struct.new(
    :id, :info, :rarity,
    :slot_fruit, :slot,
    :rarity_fruit, :rarity_label,
    :score, :sequence, :track,
    :guaranteed)

    def name
      info.dig('name', 0)
    end

    def pick_name index
      info.dig('name', index) || pick_name(index - 1) if index >= 0
    end

    def pick_title index
      picked_name = pick_name(index)
      names = info.dig('name').join(' | ').sub(picked_name, "*#{picked_name}")

      "#{names}\n#{pick_description(index)}"
    end

    def pick_description index
      info.dig('desc', index) || pick_description(index - 1) if index >= 0
    end

    def sequence_track
      "#{sequence}#{track}"
    end

    def == rhs
      id == rhs.id
    end

    def rarity_label
      super ||
        case score
        when nil, 0...6500
          :rare
        when 6500...7000
          :supa_fest
        when 7000...9100
          :supa
        when 9100...9500
          :uber_fest
        when 9500...9970
          :uber
        else
          :legend
        end
      end
  end
end
