# frozen_string_literal: true

require_relative 'lib/battle-cats-rolls/server'

warmup do |app|
  base = "http://#{ENV['WEB_HOST']}" if ENV['WEB_HOST']

  print Rack::MockRequest.new(app).get("#{base}/warmup").errors
end

run BattleCatsRolls::Server
