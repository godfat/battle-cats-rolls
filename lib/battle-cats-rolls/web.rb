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

    class View < Struct.new(:request)
      def render name, arg=nil
        erb(:layout){ erb(name, arg) }
      end

      private

      def erb name, arg=nil, &block
        ERB.new(views(name)).result(binding, &block)
      end

      def views name
        File.read("#{__dir__}/view/#{name}.erb")
      end
    end

    module Imp
      def gacha
        @gacha ||= Gacha.new(Web.ball, event_id, seed)
      end

      def event_id
        @event_id ||= request.GET['event_id'].to_i
      end

      def seed
        @seed ||= request.GET['seed'].to_i
      end

      def render *args
        view.render(*args)
      end

      def view
        @view ||= View.new(request)
      end
    end

    include Jellyfish
    controller_include NormalizedPath, Imp

    get '/' do
      cats = 1.upto(100).map do |i|
        gacha.roll_both!
      end

      render :index, cats
    end
  end
end
