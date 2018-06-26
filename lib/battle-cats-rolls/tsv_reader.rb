# frozen_string_literal: true

require 'date'

module BattleCatsRolls
  class TsvReader < Struct.new(:tsv, :version)
    def self.current
      download(
        'https://ponos.s3.dualstack.ap-northeast-1.amazonaws.com/' \
        'appli/battlecats/event_data/battlecatsen_production/gatya.tsv')
    end

    def self.current_version
      '7.1.0'
    end

    def self.download url, version=current_version
      require 'net/http'

      new(Net::HTTP.get(URI.parse(url)).force_encoding('UTF-8'), version)
    end

    def self.read path, version=current_version
      new(File.read(path), version)
    end

    def self.fields version
      case version
      when '7.1.0'
        {gacha:
          {id: 10, start_on: 0, end_on: 2, version: 4,
           rare: 16, sr: 18, ssr: 20, guaranteed: 21, name: 24}}
      end
    end

    def initialize tsv, version=self.class.current_version
      super(tsv, version)
    end

    def == rhs
      gacha == rhs.gacha
    end

    def gacha
      @gacha ||= parsed_data.inject({}) do |result, row|
        data = convert_gacha(read_row(row, gacha_fields))
        result[data[:id]] = data
        result
      end
    end

    private

    def gacha_fields
      @gacha_fields ||= fields[:gacha]
    end

    def fields
      @fields ||= self.class.fields(version)
    end

    def convert_gacha data
      data.transform_values do |(key, value)|
        case key
        when :start_on, :end_on
          Date.parse(value)
        when :id, :rare, :sr, :ssr
          value.to_i
        when :guaranteed
          value.to_i > 0
        else
          value
        end
      end
    end

    def read_row row, row_fields
      Hash[
        row_fields.keys.zip(
          row_fields.keys.zip(
            row.values_at(*row_fields.values)))]
    end

    def parsed_data
      @parsed_data ||= tsv.lines.inject([]) do |result, line|
        if line.include?("\t")
          result << line.split("\t")
        else
          result
        end
      end
    end
  end
end
