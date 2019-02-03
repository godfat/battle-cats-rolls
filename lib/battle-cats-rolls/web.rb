# frozen_string_literal: true

require_relative 'crystal_ball'
require_relative 'gacha'
require_relative 'find_cat'
require_relative 'seek_seed'
require_relative 'view'
require_relative 'cache'
require_relative 'aws_auth'

require 'jellyfish'

require 'date'
require 'json'
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
        cache[file] ||
          cache.store(file, request_tsv(file), expires_in: tsv_expires_in)
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

      def tsv_expires_in
        600
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
      cats = [
        %i[rare supa rare rare supa],
        %i[rare uber rare supa supa]
      ].map do |column|
        column.map.with_index do |rarity, index|
          cat = Cat.new(-1, {'name' => ["A #{rarity} cat"]}, rarity)
          cat.rarity_label = rarity
          cat.sequence = index + 1
          cat
        end
      end.transpose

      render :help, cats: cats
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
