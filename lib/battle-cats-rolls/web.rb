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

    def self.ball_en
      @ball_en ||= CrystalBall.load('build', 'en')
    end

    def self.ball_tw
      @ball_tw ||= CrystalBall.load('build', 'tw')
    end

    def self.ball_jp
      @ball_jp ||= CrystalBall.load('build', 'jp')
    end

    class View < Struct.new(:controller, :arg)
      extend Forwardable

      def_delegators :controller, *%w[request gacha]

      def render name
        erb(:layout){ erb(name) }
      end

      def each_ab_cat
        arg[:cats].each.inject(nil) do |prev_b, ab|
          yield(prev_b, ab)

          ab.last
        end
      end

      def guaranteed_cat cat, offset
        if guaranteed = cat.guaranteed
          link = link_to_roll(guaranteed)
          next_sequence = cat.sequence + controller.guaranteed_rolls + offset

          if offset < 0
            "#{link}<br>-&gt; #{next_sequence}"
          else
            "#{link}<br>&lt;- #{next_sequence}"
          end
        end
      end

      def link_to_roll cat
        name = h cat.pick_name(controller.name)
        title = h cat.pick_title(controller.name)

        if cat.slot_fruit
          %Q{<a href="#{h uri_to_roll(cat)}" title="#{title}">#{name}</a>}
        else
          %Q{<span title="#{title}">#{name}</span>}
        end +
          if cat.id > 0
            %Q{<a href="#{h uri_to_cat_db(cat)}">&#128062;</a>}
          else
            ''
          end
      end

      def selected_lang lang_name
        'selected="selected"' if controller.lang == lang_name
      end

      def selected_name name_name
        'selected="selected"' if controller.name == name_name
      end

      def selected_current_event event_name
        'selected="selected"' if controller.event == event_name
      end

      def selected_find cat
        'selected="selected"' if controller.find == cat.id
      end

      def checked_no_guaranteed
        'checked="checked"' if controller.no_guaranteed
      end

      def selected_force_guaranteed n
        'selected="selected"' if controller.force_guaranteed == n
      end

      def selected_ubers n
        'selected="selected"' if controller.ubers == n
      end

      def checked_details
        'checked="checked"' if details
      end

      def show_event info
        h "#{info['start_on']} ~ #{info['end_on']}: #{info['name']}"
      end

      def show_gacha_slots cats
        cats.map.with_index do |cat, i|
          "#{i} #{cat_name(cat)}"
        end.join(', ')
      end

      def cat_name cat
        h cat.pick_name(controller.name)
      end

      def event_lang
        case controller.lang
        when 'jp'
        else
          controller.lang
        end
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

        @details = !request.params['details'].to_s.strip.empty? || nil
      end

      def uri_to_roll cat
        uri({seed: cat.slot_fruit.seed}.merge(default_query))
      end

      def uri_to_cat_db cat
        "https://battlecats-db.com/unit/#{'%03d' % cat.id}.html"
      end

      def uri query=default_query
        path = "#{request.base_url}#{request.path}"

        if query.empty?
          path
        else
          "#{path}?#{query_string(query)}"
        end
      end

      def permalink
        if controller.next_seed.nonzero?
          uri({next_seed: controller.next_seed}.merge(default_query))
        else
          uri({seed: controller.seed}.merge(default_query))
        end
      end

      def default_query
        cleanup_query(
          event: controller.event,
          lang: controller.lang,
          name: controller.name,
          count: controller.count,
          find: controller.find,
          force_guaranteed: controller.force_guaranteed,
          ubers: controller.ubers,
          details: details)
      end

      def cleanup_query query
        query.compact.select do |key, value|
          if (key == :next_seed && value == 0) ||
             (key == :seed && value == 0) ||
             (key == :lang && value == 'en') ||
             (key == :name && value == 0) ||
             (key == :count && value == 100) ||
             (key == :find && value == 0) ||
             (key == :force_guaranteed && value == 0) ||
             (key == :ubers && value == 0)
            false
          else
            true
          end
        end
      end

      def query_string query
        query.map do |key, value|
          "#{u key.to_s}=#{u value.to_s}"
        end.join('&')
      end

      def seek_host
        ENV['SEEK_HOST'] || request.host_with_port
      end

      def web_host
        ENV['WEB_HOST'] || request.host_with_port
      end

      def seek_uri
        "//#{seek_host}/seek?#{query_string(default_query)}"
      end

      def erb name, &block
        ERB.new(views(name)).result(binding, &block)
      end

      def views name
        File.read("#{__dir__}/view/#{name}.erb")
      end
    end

    module Imp
      def lang
        @lang ||=
          case value = request.params['lang']
          when 'tw', 'jp'
            value
          else
            'en'
          end
      end

      def name
        @name ||=
          case value = request.params['name'].to_i
          when 1, 2
            value
          else
            0
          end
      end

      def ball
        @ball ||= Web.public_send("ball_#{lang}")
      end

      def gacha
        @gacha ||= Gacha.new(ball, event, gacha_seed)
      end

      # This is the seed we're using to calculate the tracks
      def gacha_seed
        @gacha_seed ||=
          if next_seed.nonzero?
            next_seed
          else
            seed
          end
      end

      # This is the seed from the seed input field
      def seed
        @seed ||= request.params['seed'].to_i
      end

      # This is a special seed indicates that we don't want to
      # advance the seed for the first roll. In Gacha#current_seed_mode!,
      # we advance the seed once to simulate the actual states of the game.
      # That is, the game will advance the seed before rolling.
      #
      # This is all good, but this also means that we do not know the
      # very first seed from the seek result, because we can't roll back
      # the very first seed to see what will roll into the current seed.
      # Here the `next_seed` indicates that we should treat this seed
      # as the next seed, therefore we don't advance the first seed.
      # Thus in the controller we do:
      #
      #     gacha.current_seed_mode! if next_seed.zero?
      #
      # When there's no next seed, we enter current seed mode to advance
      # the first seed. Otherwise, we just use the next seed as the first one.
      # This is a special case for the very first seek result. Therefore
      # we don't provide this as an option with an input. This will only
      # be used in seek_result.erb. After the user clicks on the other links,
      # they go back to the normal current seed mode because now we know
      # what exactly is the current seed.
      def next_seed
        @next_seed ||= request.params['next_seed'].to_i
      end

      def event
        @event ||= request.params['event'] || current_event
      end

      def count
        @count ||= [1, [(request.params['count'] || 100).to_i, Max].min].max
      end

      def find
        @find ||= request.params['find'].to_i
      end

      def no_guaranteed
        return @no_guaranteed if instance_variable_defined?(:@no_guaranteed)

        @no_guaranteed =
          !request.params['no_guaranteed'].to_s.strip.empty? || nil
      end

      def force_guaranteed
        @force_guaranteed ||= request.params['force_guaranteed'].to_i
      end

      def guaranteed_rolls
        @guaranteed_rolls ||=
          if force_guaranteed.zero?
            gacha.pool.guaranteed_rolls
          else
            force_guaranteed
          end
      end

      def ubers
        @ubers ||= request.params['ubers'].to_i
      end

      def current_event
        @current_event ||=
          upcoming_events.find{ |_, info| info['platinum'].nil? }&.first
      end

      def upcoming_events
        @upcoming_events ||= grouped_events[true] || []
      end

      def past_events
        @past_events ||= grouped_events[false] || []
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
        @all_events ||= ball.dig('events')
      end

      def seek_source
        @seek_source ||=
          [gacha.rare, gacha.supa, gacha.uber,
           gacha.rare_cats.size, gacha.supa_cats.size, gacha.uber_cats.size,
           *request.POST['rolls']].join(' ').squeeze(' ')
      end

      def cache
        @cache ||= Cache.default(logger)
      end

      def logger
        @logger ||= env['rack.logger'] || begin
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
      if event && gacha_seed != 0
        gacha.current_seed_mode! if next_seed.zero?

        gacha.pool.add_future_ubers(ubers) if ubers > 0

        # Human counts from 1
        cats = 1.upto(count).map do |sequence|
          gacha.roll_both_with_sequence!(sequence)
        end

        gacha.fill_guaranteed(cats, guaranteed_rolls)

        found_cats =
          FindCat.search(gacha, find,
            cats: cats, guaranteed: !no_guaranteed, max: Max)

        render :index, cats: cats, found_cats: found_cats
      else
        render :index
      end
    end

    class Seek
      include Jellyfish
      controller_include NormalizedPath, Imp

      get '/seek' do
        render :seek, queue_size: SeekSeed.queue.size
      end

      post '/seek/enqueue' do
        key = SeekSeed.enqueue(seek_source, cache, logger)

        found "/seek/result/#{key}?event=#{event}&lang=#{lang}&name=#{name}"
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
