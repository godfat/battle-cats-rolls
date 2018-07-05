# frozen_string_literal: true

require_relative 'crystal_ball'
require_relative 'gacha'

require 'jellyfish'

require 'cgi'
require 'erb'
require 'forwardable'

module BattleCatsRolls
  class Web
    def self.ball
      @ball ||= CrystalBall.load('build/7.1.0')
    end

    class View < Struct.new(:controller, :arg)
      extend Forwardable

      def_delegator :controller, :request

      def render name
        erb(:layout){ erb(name) }
      end

      def each_ab_cat
        arg[:cats].each.with_index.inject(nil) do |prev_b, (ab, index)|
          sequence = index + 1 # Human counts from 1
          next_a = arg.dig(:cats, sequence, 0)

          yield(prev_b, ab, next_a, sequence)

          ab.last
        end
      end

      def guaranteed_cat cat, sequence, offset
        if name = cat.guaranteed
          next_sequence = sequence + arg[:guaranteed_rolls] + offset
          next_cat = arg.dig(:cats, next_sequence - 1, offset)
                               # Back to count from 0, ^ Swap track!
          link = link_to_roll(name, next_cat)

          if offset < 0
            "#{link}<br>-&gt; #{next_sequence}"
          else
            "#{link}<br>&lt;- #{next_sequence}"
          end
        end
      end

      def link_to_roll name, next_cat
        if next_cat
          %Q{<a href="#{uri_to_roll(next_cat)}">#{h name}</a>}
        else
          name
        end
      end

      def h str
        CGI.escape_html(str)
      end

      def u str
        CGI.escape(str)
      end

      private

      def uri_to_roll cat
        uri(seed: cat.rarity_seed,
            event: controller.event,
            count: controller.count)
      end

      def uri query={}
        query_string = query.map do |key, value|
          "#{u key.to_s}=#{u value.to_s}"
        end.join('&')

        path = "#{request.base_url}#{request.path}"

        if query.empty?
          path
        else
          "#{path}?#{query_string}"
        end
      end

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

      def seed
        @seed ||= request.GET['seed'].to_i
      end

      def event
        @event ||= request.GET['event']
      end

      def count
        @count ||= [1, [(request.GET['count'] || 100).to_i, 999].min].max
      end

      def render name, arg
        View.new(self, arg).render(name)
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
