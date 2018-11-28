# frozen_string_literal: true

require_relative 'lib/battle-cats-rolls/server'

warmup(&BattleCatsRolls.method(:warmup))

run BattleCatsRolls::Server
