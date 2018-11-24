# frozen_string_literal: true

require_relative 'lib/battle-cats-rolls/server'

warmup do |app|
  print Rack::MockRequest.new(app).get('/warmup').errors
end

run BattleCatsRolls::Server
