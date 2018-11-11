# frozen_string_literal: true

module BattleCatsRolls
  class Cat < Struct.new(
    :id, :info,
    :slot, :slot_fruit,
    :rarity, :rarity_fruit,
    :score, :sequence, :guaranteed)

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

    def == rhs
      id == rhs.id
    end

    def color_label
      case score
      when 0...6500
        :rare
      when 6500...7000
        :supa_fest
      when 7000...9100
        :supa
      when 9100...9500
        :uber_fest
      when 9500...9900
        :uber
      else
        :legend
      end
    end

    def to_ansi
      "\e[#{color_sequence}m#{name}\e[0m"
    end

    private

    def color_sequence
      case color_label
      when :rare
        '00;3'
      when :supa_fest
        '33;1'
      when :supa
        '33;3'
      when :uber_fest
        '32;1'
      when :uber
        '32;3'
      end
    end
  end
end
