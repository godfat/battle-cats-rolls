# frozen_string_literal: true

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

      new(Net::HTTP.get(URI.parse(url)), version)
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
      convert_gacha_guaranteed(
        data.transform_values do |value|
          case value
          when /\A(?:[1-9]\d*)|0\z/ # Avoid converting 010 to 10
            value.to_i
          else
            value
          end
        end
      )
    end

    def convert_gacha_guaranteed data
      data[:guaranteed] =
        case data[:guaranteed]
        when 0
          false
        when 1
          true
        else
          data[:guaranteed]
        end

      data
    end

    def read_row row, row_fields
      Hash[row_fields.keys.zip(row.values_at(*row_fields.values))]
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
