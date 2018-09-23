# frozen_string_literal: true

require_relative 'crystal_ball'
require_relative 'gacha'
require_relative 'find_cat'
require_relative 'seek_seed'
require_relative 'cache'

require 'jellyfish'

require 'cgi'
require 'erb'
require 'date'
require 'forwardable'

module BattleCatsRolls
  class Web
    Max = 999

    def self.ball
      @ball ||= CrystalBall.load('build')
    end

    class View < Struct.new(:controller, :arg)
      extend Forwardable

      def_delegators :controller,
        *%w[request gacha event find upcoming_events past_events]

      def render name
        erb(:layout){ erb(name) }
      end

      def each_ab_cat
        arg[:cats].each.inject(nil) do |prev_b, ab|
          next_a = arg.dig(:cats, ab.dig(0, :sequence), 0)

          yield(prev_b, ab, next_a)

          ab.last
        end
      end

      def guaranteed_cat cat, offset
        if guaranteed = cat.guaranteed
          next_sequence = cat.sequence + arg[:guaranteed_rolls] + offset
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
          %Q{<a href="#{h uri_to_roll(next_cat)}">#{h cat.name}</a>}
        else
          h cat.name
        end + %Q{<a href="#{h uri_to_cat_db(cat)}">&#128062;</a>}
      end

      def selected_current_event event_name
        'selected="selected"' if event == event_name
      end

      def selected_find cat
        'selected="selected"' if find == cat.id
      end

      def checked_details
        'checked="checked"' if details
      end

      def show_event info
        h "#{info['start_on']} ~ #{info['end_on']}: #{info['name']}"
      end

      def show_gacha_slots cats
        cats.map.with_index{ |cat, i| "#{i} #{cat.name}" }.join(', ')
      end

      def h str
        CGI.escape_html(str)
      end

      def u str
        CGI.escape(str)
      end

      private

      def seed_column fruit
        return unless details

        <<~HTML
          <td>#{fruit.seed}</td>
          <td>#{if fruit.seed == fruit.value then '-' else fruit.value end}</td>
        HTML
      end

      def details
        return @details if instance_variable_defined?(:@details)

        @details = !request.GET['details'].to_s.strip.empty? || nil
      end

      def uri_to_roll cat
        uri(seed: cat.rarity_fruit.seed,
            event: event,
            count: controller.count,
            details: details,
            find: find)
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

      def seek_host
        ENV['SEEK_HOST'] || request.host_with_port
      end

      def web_host
        ENV['WEB_HOST'] || request.host_with_port
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
        @event ||= request.params['event'] || current_event
      end

      def count
        @count ||= [1, [(request.GET['count'] || 100).to_i, Max].min].max
      end

      def find
        return @find if instance_variable_defined?(:@find)

        id = request.GET['find'].to_i

        @find = id.nonzero? && id || nil
      end

      def current_event
        @current_event ||=
          upcoming_events.find{ |_, info| info['platinum'].nil? }&.first
      end

      def upcoming_events
        @upcoming_events ||= grouped_events[true]
      end

      def past_events
        @past_events ||= grouped_events[false]
      end

      def grouped_events
        @grouped_events ||= begin
          today = Date.today

          all_events.group_by do |_, value|
            if value['platinum']
              current_platinum['id'] == value['id']
            else
              today <= value['end_on']
            end
          end
        end
      end

      def current_platinum
        @current_platinum ||= begin
          past = Date.new

          all_events.max_by do |_, value|
            if value['platinum'] then value['start_on'] else past end
          end.last
        end
      end

      def all_events
        @all_events ||= Web.ball.dig('events')
      end

      def seek_source
        @seek_source ||=
          [gacha.rare, gacha.sr, gacha.ssr,
           gacha.rare_cats.size, gacha.sr_cats.size, gacha.uber_cats.size,
           *request.POST['rolls']].join(' ').squeeze(' ')
      end

      def cache
        @cache ||= Cache.default(logger(env))
      end

      def logger env
        env['rack.logger'] || begin
          require 'logger'
          Logger.new(env['rack.errors'])
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
        # Human counts from 1
        cats = 1.upto(count).map do |sequence|
          gacha.roll_both_with_sequence!(sequence)
        end

        guaranteed_rolls = gacha.fill_guaranteed(cats)
        found_cats = FindCat.search(gacha, find, cats: cats, max: Max)

        render :index,
          cats: cats,
          guaranteed_rolls: guaranteed_rolls,
          found_cats: found_cats
      else
        render :index
      end
    end

    class Seek
      include Jellyfish
      controller_include NormalizedPath, Imp

      get '/seek' do
        render :seek
      end

      post '/seek/enqueue' do
        key = SeekSeed.enqueue(seek_source, cache)

        found "/seek/result/#{key}"
      end

      get %r{^/seek/result/?(?<key>\w*)} do |m|
        key = m[:key]
        seed = cache[key] if /./.match?(key)
        seek = SeekSeed.queue.dig(key, :seek)

        seek.yield if seek&.ended?

        render :seek_result, seed: seed, seek: seek
      end
    end
  end
end
