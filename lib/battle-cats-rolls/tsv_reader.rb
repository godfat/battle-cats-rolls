# frozen_string_literal: true

require 'date'

require_relative 'gacha'

module BattleCatsRolls
  class TsvReader < Struct.new(:tsv)
    OffsetIndex = 9

    def self.download url
      require 'net/http'

      new(Net::HTTP.get(URI.parse(url)).force_encoding('UTF-8'))
    end

    def self.read path
      new(File.read(path))
    end

    def self.event_fields
      @event_fields ||= {
        'start_on' => 0, 'end_on' => 2, 'version' => 4,
        'type' => 8, 'offset' => OffsetIndex
      }
    end

    def self.pool_fields
      @pool_fields ||= {
        'id' => 0, 'step_up' => 3,
        'rare' => 6, 'sr' => 8, 'ssr' => 10,
        'guaranteed' => 11
      }
    end

    def == rhs
      gacha == rhs.gacha
    end

    def gacha
      @gacha ||= parsed_data.inject({}) do |result, row|
        data = convert_event(read_event(row, self.class.event_fields))

        if data.delete('type') == 1
          pool = data.delete('pool')[data.delete('offset') - 1]

          if pool['id'] > 0
            data.merge!(pool)
            data['platinum'] = true if data['ssr'] == Gacha::Base

            # TODO: Remove me, this is only here to reduce diff for YAML
            data = {
              'id' => data['id'],
              'start_on' => data['start_on'],
              'end_on' => data['end_on'],
              'version' => data['version'],
              'rare' => data['rare'],
              'sr' => data['sr'],
              'ssr' => data['ssr'],
              'guaranteed' => data['guaranteed'],
              'step_up' => data['step_up'],
              'name' => data['name'],
              'platinum' => data['platinum']
            }
            data.delete('platinum') unless data['platinum']

            result["#{data['start_on']}_#{data['id']}"] = data
          end
        end

        result
      end
    end

    private

    def convert_event data
      data.transform_values do |(key, value)|
        case key
        when 'start_on', 'end_on'
          Date.parse(value)
        when 'type', 'offset'
          value.to_i
        when 'pool'
          convert_pool(value)
        else
          value
        end
      end
    end

    def convert_pool data
      data.map do |pool|
        pool.transform_values do |(key, value)|
          case key
          when 'id', 'rare', 'sr', 'ssr'
            value.to_i
          when 'step_up'
            value.to_i & 4 == 4
          when 'guaranteed'
            value.to_i > 0
          else
            value
          end
        end
      end
    end

    def read_event row, event_fields
      result = read_row(row, event_fields)

      result['name'] = ['name', row.last.strip]

      result['pool'] = ['pool', extract_pool(row).map do |pool_row|
        read_row(pool_row, self.class.pool_fields)
      end]

      result
    end

    def read_row row, row_fields
      Hash[
        row_fields.keys.zip(
          row_fields.keys.zip(
            row.values_at(*row_fields.values)))]
    end

    def extract_pool row
      row.drop(OffsetIndex + 1).each_slice(14).to_a[0...-1]
    end

    def parsed_data
      @parsed_data ||= tsv.lines.inject([]) do |result, line|
        if line.include?("\t")
          result << line.split(/\t+/)
        else
          result
        end
      end
    end
  end
end
