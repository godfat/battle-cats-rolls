# frozen_string_literal: true

require_relative 'cat'
require_relative 'gacha'

module BattleCatsRolls
  class Help
    def read_the_tracks
      @read_the_tracks ||= fake_tracks.first(5)
    end

    def advance_the_tracks
      @advance_the_tracks ||=
        read_the_tracks.drop(2).each{ |cs| cs.each{ |c| c.sequence -= 2 }}
    end

    def lookup_cat_data gacha
      @lookup_cat_data ||= [[
        fake_cat(
          319, gacha.pool.dig_cat(Gacha::Uber, 319, 'name', 0), 1, 'A'),
        fake_cat(-1, 'Cat', 1, 'B')
      ]]
    end

    def guaranteed_tracks
      @guaranteed_tracks ||= begin
        tracks = fake_tracks.dup
        tracks.dig(0, 0).guaranteed =
          fake_cat(-1, '1A guaranteed uber', 1, 'AG')
        tracks.dig(0, 1).guaranteed =
          fake_cat(-1, '1B guaranteed uber', 1, 'BG')
        tracks
      end
    end

    private

    def fake_tracks
      @fake_tracks ||= [
        %i[rare supa rare rare supa supa rare uber supa rare legend],
        %i[supa rare uber supa rare rare supa rare rare supa rare]
      ].map.with_index do |column, a_or_b|
        column.map.with_index do |rarity_label, index|
          sequence = index + 1
          track = ('A'.ord + a_or_b).chr
          cat = fake_cat(
            -1, "#{sequence}#{track} #{rarity_label} cat", sequence, track)
          cat.rarity_label = rarity_label
          cat
        end
      end.transpose
    end

    def fake_cat id, name, sequence, track
      Cat.new(
        id, {'name' => [name]}, nil,
        nil, nil, nil, nil, nil,
        sequence, track)
    end
  end
end
