# frozen_string_literal: true

require_relative 'crystal_ball'
require_relative 'gacha'
require_relative 'find_cat'
require_relative 'seek_seed'
require_relative 'cache'
require_relative 'aws_auth'

require 'jellyfish'
require 'tilt'

require 'cgi'
require 'erb'
require 'date'
require 'json'
require 'forwardable'
require 'net/http'

module BattleCatsRolls
  class Web
    def self.root
      @root ||= "#{__dir__}/../.."
    end

    def self.ball_en
      @ball_en ||= CrystalBall.load("#{root}/build", 'en')
    end

    def self.ball_tw
      @ball_tw ||= CrystalBall.load("#{root}/build", 'tw')
    end

    def self.ball_jp
      @ball_jp ||= CrystalBall.load("#{root}/build", 'jp')
    end

    class View < Struct.new(:controller, :arg)
      extend Forwardable

      def_delegators :controller, *%w[request gacha details]

      def render name
        erb(:layout){ erb(name) }
      end

      def each_ball_cat
        arg[:cats].each do |rarity, data|
          yield(rarity, data.map{ |id, info| Cat.new(id, info) })
        end
      end

      def each_ab_cat
        arg[:cats].inject(nil) do |prev_b, ab|
          yield(prev_b, ab)

          ab.last
        end
      end

      def guaranteed_cat cat, offset
        if guaranteed = cat.guaranteed
          link = link_to_roll(guaranteed)
          next_sequence = cat.sequence + controller.guaranteed_rolls + offset

          if offset < 0
            "#{link}<br>-&gt; #{next_sequence}B"
          else
            "#{link}<br>&lt;- #{next_sequence}A"
          end
        end
      end

      def color_label cat
        "pick #{color_rarity(cat)} #{color_picked(cat)}"
      end

      def color_label_guaranteed cat
        if cat.guaranteed
          "pick #{color_guaranteed(cat)} #{color_picked_guaranteed(cat)}"
        end
      end

      def color_picked cat
        sequence = cat.sequence
        pick_position = controller.pick_position
        pick_guaranteed = controller.pick_guaranteed
        guaranteed_rolls = controller.guaranteed_rolls
        guaranteed_position = pick_position + guaranteed_rolls

        if pick_position > 0
          if cat.track == controller.pick_track
            if pick_guaranteed
              if sequence < pick_position
                :picked
              elsif sequence < guaranteed_position - 1
                :picked_cumulatively
              end
            elsif sequence <= pick_position
              :picked
            elsif sequence == pick_position + 1
              :next_position
            end
          elsif pick_guaranteed &&
                sequence == guaranteed_position - (cat.track.ord - 'A'.ord)
            :next_position
          end
        end
      end

      def color_picked_guaranteed cat
        :picked_cumulatively if
          controller.pick == cat.guaranteed.sequence_track
      end

      def color_rarity cat
        case rarity_label = cat.rarity_label
        when :legend
          :legend
        else
          case cat.id
          when controller.find
            :found
          when *FindCat.exclusives
            :exclusive
          else
            rarity_label
          end
        end
      end

      def color_guaranteed cat
        case cat.guaranteed.id
        when controller.find
          :found
        when *FindCat.exclusives
          :exclusive
        when Integer
          :rare
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
            %Q{<a href="#{h uri_to_cat_db(cat)}">🐾</a>}
          else
            ''
          end
      end

      def pick_option cats
        cats.map.with_index do |cat, slot|
          <<~HTML
            <option value="#{cat.rarity} #{slot}">#{cat_name(cat)}</option>
          HTML
        end.join
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

      def hidden_inputs *input_names
        input_names.map do |name|
          <<~HTML
            <input type="hidden" name="#{name}" value="#{controller.public_send(name)}">
          HTML
        end.join("\n")
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

      def header n, name
        id = name.to_s.downcase.gsub(/\W+/, '-')

        <<~HTML
          <a href="##{id}">⚓</a> <h#{n} id="#{id}">#{name}</h#{n}>
        HTML
      end

      def seed_column fruit
        return unless details

        <<~HTML
          <td>#{fruit.seed}</td>
          <td>#{if fruit.seed == fruit.value then '-' else fruit.value end}</td>
        HTML
      end

      def onclick_pick(cat)
        return unless cat

        %Q{onclick="pick('#{cat.sequence_track}')"}
      end

      def uri_to_roll cat
        uri(query: {seed: cat.slot_fruit.seed})
      end

      def uri_to_cat_db cat
        "https://battlecats-db.com/unit/#{sprintf('%03d', cat.id)}.html"
      end

      def uri path: "//#{web_host}/", query: {}
        # keep query hash order
        query = cleanup_query(query.merge(default_query).merge(query))

        if query.empty?
          path
        else
          "#{path}?#{query_string(query)}"
        end
      end

      def default_query
        {
          next_seed: controller.next_seed,
          seed: controller.seed,
          event: controller.event,
          lang: controller.lang,
          name: controller.name,
          count: controller.count,
          find: controller.find,
          no_guaranteed: controller.no_guaranteed,
          force_guaranteed: controller.force_guaranteed,
          ubers: controller.ubers,
          details: details
        }
      end

      def cleanup_query query
        query.compact.select do |key, value|
          if (key == :next_seed && (value == 0 || query[:seed].nonzero?)) ||
             (key == :seed && value == 0) ||
             (key == :lang && value == 'en') ||
             (key == :name && value == 0) ||
             (key == :count && value == 100) ||
             (key == :find && value == 0) ||
             (key == :no_guaranteed && value == 0) ||
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

      def event_base_uri
        "#{request.scheme}://#{seek_host}/seek"
      end

      def cats_uri
        uri(path: "//#{web_host}/cats")
      end

      def help_uri
        uri(path: "//#{web_host}/help")
      end

      def logs_uri
        uri(path: "//#{web_host}/logs")
      end

      def seek_uri
        uri(path: "//#{seek_host}/seek")
      end

      def erb name, &block
        self.class.template(name).render(self, &block)
      end

      def self.template name
        (@template ||= {})[name.to_s] ||=
          Tilt.new("#{__dir__}/view/#{name}.erb")
      end

      def self.warmup
        prefix = Regexp.escape("#{__dir__}/view/")

        Dir.glob("#{__dir__}/view/**/*") do |name|
          next if File.directory?(name)

          View.template(name[/\A#{prefix}(.+)\.erb\z/m, 1])
        end
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
        @count ||=
          [1,
           [(request.params['count'] || 100).to_i, FindCat::Max].min
          ].max
      end

      def find
        @find ||= request.params['find'].to_i
      end

      def pick
        @pick ||= request.params['pick'].to_s
      end

      def pick_position
        @pick_pos ||= pick.to_i
      end

      def pick_track
        @pick_track ||= pick[/\A\d+(\w)/, 1]
      end

      def pick_guaranteed
        return @pick_guaranteed if
          instance_variable_defined?(:@pick_guaranteed)

        @pick_guaranteed = pick.end_with?('G')
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

      def details
        return @details if instance_variable_defined?(:@details)

        @details = !request.params['details'].to_s.strip.empty? || nil
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
          [gacha.rare, gacha.supa, gacha.uber, gacha.legend,
           gacha.rare_cats.size, gacha.supa_cats.size,
           gacha.uber_cats.size, gacha.legend_cats.size,
           *request.POST['rolls']].join(' ').squeeze(' ')
      end

      def serve_tsv file
        cache[file] || cache.store(file, request_tsv(file), expires_in: 300)
      end

      def request_tsv file
        aws = aws_auth(file)
        request = Net::HTTP::Get.new(aws.uri)

        aws.headers.each do |key, value|
          request[key] = value
        end

        response = Net::HTTP.start(
          aws.uri.hostname,
          aws.uri.port,
          use_ssl: true) do |http|
          http.request(request)
        end

        response.body
      end

      def aws_auth file
        url =
          "https://nyanko-events-prd.s3.ap-northeast-1.amazonaws.com/battlecats_production/#{file}"
        AwsAuth.new(:get, url)
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
      if event && gacha_seed.nonzero? && gacha.pool.exist?
        gacha.current_seed_mode! if next_seed.zero?

        gacha.pool.add_future_ubers(ubers) if ubers > 0

        # Human counts from 1
        cats = 1.upto(count).map do |sequence|
          gacha.roll_both_with_sequence!(sequence)
        end

        gacha.fill_guaranteed(cats, guaranteed_rolls)

        found_cats =
          FindCat.search(gacha, find,
            cats: cats, guaranteed: !no_guaranteed, max: FindCat::Max)

        render :index, cats: cats, found_cats: found_cats
      else
        render :index
      end
    end

    get '/warmup' do
      cache
      Web.ball_en
      Web.ball_tw
      Web.ball_jp
      View.warmup
      'OK'
    end

    get '/cats' do
      render :cats, cats: ball.dig('cats')
    end

    get '/help' do
      render :help
    end

    get '/logs' do
      render :logs
    end

    class Seek
      include Jellyfish
      controller_include NormalizedPath, Imp

      %w[gatya.tsv item.tsv sale.tsv].each do |file|
        get "/seek/#{file}" do
          headers 'Content-Type' => 'text/plain; charset=utf-8'
          body serve_tsv(file)
        end

        get "/seek/curl/#{file}" do
          headers 'Content-Type' => 'text/plain; charset=utf-8'
          body "#{aws_auth(file).to_curl}\n"
        end

        get "/seek/json/#{file}" do
          headers 'Content-Type' => 'application/json; charset=utf-8'
          body JSON.dump(aws_auth(file).headers)
        end
      end

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
        seek = SeekSeed.queue[key]

        seek.yield if seek&.ended?

        render :seek_result, seed: seed, seek: seek
      end
    end
  end
end
