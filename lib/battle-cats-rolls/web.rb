# frozen_string_literal: true

require_relative 'crystal_ball'
require_relative 'gacha'

require 'jellyfish'

require 'cgi'
require 'erb'
require 'date'
require 'forwardable'

module BattleCatsRolls
  class Web
    def self.ball
      @ball ||= CrystalBall.load('build/7.1.0')
    end

    class View < Struct.new(:controller, :arg)
      extend Forwardable

      def_delegators :controller, *%w[request upcoming_events past_events]

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
        if guaranteed = cat.guaranteed
          next_sequence = sequence + arg[:guaranteed_rolls] + offset
          next_cat = arg.dig(:cats, next_sequence - 1, offset)
                               # Back to count from 0, ^ Swap track!
          link = link_to_roll(guaranteed, next_cat)

          if offset < 0
            "#{link}<br>-&gt; #{next_sequence}"
          else
            "#{link}<br>&lt;- #{next_sequence}"
          end
        end
      end

      def link_to_roll cat, next_cat=nil
        if next_cat
          %Q{<a href="#{h uri_to_roll(next_cat)}">#{h cat.name}</a>} +
          %Q{<a href="#{h uri_to_cat_db(cat)}">&#128062;</a>}
        else
          h cat.name
        end
      end

      def selected_current_event event_name
        'selected="selected"' if controller.event == event_name
      end

      def checked_show_seed
        'checked="checked"' if show_seed
      end

      def show_event info
        h "#{info['start_on']} ~ #{info['end_on']}: #{info['name']}"
      end

      def h str
        CGI.escape_html(str)
      end

      def u str
        CGI.escape(str)
      end

      # Copied from ActionView
      def j str
        map = {
          '\\' => '\\\\',
          "</" => '<\/',
          "\r\n" => '\n',
          "\n" => '\n',
          "\r" => '\n',
          '"' => '\\"',
          "'" => "\\'"
        }

        target = /(\\|<\/|\r\n|\342\200\250|\342\200\251|[\n\r"'])/u

        str.gsub(target){ |match| map[match] }
      end

      private

      def seed_column
        yield if show_seed
      end

      def show_seed
        return @show_seed if instance_variable_defined?(:@show_seed)

        @show_seed = !request.GET['show_seed'].to_s.strip.empty? || nil
      end

      def uri_to_roll cat
        uri(seed: cat.rarity_seed,
            event: controller.event,
            count: controller.count,
            show_seed: show_seed)
      end

      def uri_without_event
        uri(seed: controller.seed,
            count: controller.count,
            show_seed: show_seed)
      end

      def uri_to_cat_db cat
        "https://battlecats-db.com/unit/#{'%03d' % cat.id}.html"
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
        @event ||= request.GET['event'] || current_event
      end

      def count
        @count ||= [1, [(request.GET['count'] || 100).to_i, 999].min].max
      end

      def current_event
        @current_event ||=
          upcoming_events.find{ |_, info| info['platinum'].nil? }&.first
      end

      def upcoming_events
        @upcoming_events ||= all_events[true]
      end

      def past_events
        @past_events ||= all_events[false]
      end

      def all_events
        @all_events ||= begin
          today = Date.today

          Web.ball.dig('events').group_by do |key, value|
            today <= value['end_on']
          end
        end
      end

      def render name, arg=nil
        View.new(self, arg).render(name)
      end
    end

    include Jellyfish
    controller_include NormalizedPath, Imp

    get '/' do
      if event && seed != 0
        cats = 1.upto(count).map do |i|
          gacha.roll_both!
        end

        guaranteed_rolls = gacha.fill_guaranteed(cats)

        render :index, cats: cats, guaranteed_rolls: guaranteed_rolls
      else
        render :index
      end
    end
  end
end
