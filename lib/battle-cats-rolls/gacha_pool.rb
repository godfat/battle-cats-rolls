# frozen_string_literal: true

require 'forwardable'

module BattleCatsRolls
  class GachaPool < Struct.new(:ball, :info, :slots)
    extend Forwardable

    # def_delegators :info, :id, :start_on, :end_on, :name, :rare, :sr, :ssr
    def_delegators :slots, :dig

    def initialize ball, event_id
      super(
        ball,
        ball.dig('events', event_id),
        ball.dig('gacha', event_id).
          inject(Hash.new{|h,k|h[k]=[]}) do |result, cat_id|
            if rarity = find_rarity(ball, cat_id)
              result[rarity] << cat_id
              result
            else
              raise "Cannot find cat: #{cat_id}"
            end
          end)
    end

    private

    def find_rarity ball, cat_id
      ball.dig('cats').find do |(rarity, cats)|
        break rarity if cats.member?(cat_id)
      end
    end
  end
end
