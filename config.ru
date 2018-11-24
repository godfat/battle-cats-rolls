# frozen_string_literal: true

require_relative 'lib/battle-cats-rolls/server'

warmup do |app|
  Rack::MockRequest.new(app).get('/warmup')
end

run BattleCatsRolls::Server
