# frozen_string_literal: true

require_relative 'tsv_reader'

module BattleCatsRolls
  class EventsReader
    def self.read dir
      Dir["#{dir}/**/*.tsv"].inject(new) do |result, tsv|
        result << TsvReader.read(tsv)
      end.sort!
    end

    def << tsv
      tsv.gacha.each do |id, data_new|
        if data_old = gacha[id]
          gacha[id] = data_new if data_old['start_on'] < data_new['start_on']
        else
          gacha[id] = data_new
        end
      end

      self
    end

    def gacha
      @gacha ||= {}
    end

    def sort!
      @gacha = Hash[gacha.sort]

      self
    end
  end
end
