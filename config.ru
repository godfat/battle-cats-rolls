# frozen_string_literal: true

require_relative 'lib/battle-cats-rolls/server'

warmup do |app|
  base = "http://#{ENV['WEB_YAHNS']}" if ENV['WEB_YAHNS']

  print Rack::MockRequest.new(app).get("#{base}/warmup").errors
end

run BattleCatsRolls::Server
