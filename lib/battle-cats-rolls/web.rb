# frozen_string_literal: true

require_relative 'crystal_ball'
require_relative 'gacha'

require 'jellyfish'
require 'erb'

module BattleCatsRolls
  class Web
    def self.ball
      @ball ||= CrystalBall.load('build/7.1.0')
    end

    class View < Struct.new(:arg)
      def render name
        erb(:layout){ erb(name) }
      end

      def each_ab_cat
        arg[:cats].each.with_index.inject(nil) do |prev_b, (ab, index)|
          yield(prev_b, ab, index + 1)

          ab.last
        end
      end

      def guaranteed_cat cat, sequence, offset
        if name = cat.guaranteed
          step = sequence + arg[:guaranteed_rolls] + offset

          if offset < 0
            "#{name}<br>-&gt; #{step}"
          else
            "#{name}<br>&lt;- #{step}"
          end
        end
      end

      private

      def erb name, &block
        ERB.new(views(name)).result(binding, &block)
      end

      def views name
        File.read("#{__dir__}/view/#{name}.erb")
      end
    end

    module Imp
      def gacha
        @gacha ||= Gacha.new(Web.ball, event, seed)
      end

      def event
        @event ||= request.GET['event']
      end

      def seed
        @seed ||= request.GET['seed'].to_i
      end

      def count
        @count ||= [1, [(request.GET['count'] || 100).to_i, 999].min].max
      end

      def render name, arg
        View.new(arg).render(name)
      end
    end

    include Jellyfish
    controller_include NormalizedPath, Imp

    get '/' do
      cats = 1.upto(count).map do |i|
        gacha.roll_both!
      end

      guaranteed_rolls = gacha.fill_guaranteed(cats)

      render :index, cats: cats, guaranteed_rolls: guaranteed_rolls
    end
  end
end