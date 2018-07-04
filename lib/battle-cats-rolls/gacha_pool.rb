# frozen_string_literal: true

require 'forwardable'

module BattleCatsRolls
  class GachaPool < Struct.new(:cats, :gacha, :event)
    extend Forwardable

    def_delegator :cats, :dig, :dig_cat
    def_delegator :slots, :dig, :dig_slot

    %w[id start_on end_on name rare sr ssr].each do |name|
      define_method(name) do
        event[name]
      end
    end

    def initialize ball, event_name
      event_id = event_name[/(?<=\:)\d+\z/].to_i

      super(
        ball.dig('cats'),
        ball.dig('gacha', event_id),
        ball.dig('events', event_name))
    end

    def slots
      @slots ||= gacha.inject(Hash.new{|h,k|h[k]=[]}) do |result, cat_id|
        if rarity = find_rarity(cat_id)
          result[rarity] << cat_id
          result
        else
          raise "Cannot find cat: #{cat_id}"
        end
      end
    end

    private

    def find_rarity cat_id
      cats.find do |(rarity, cats)|
        break rarity if cats.member?(cat_id)
      end
    end
  end
end
