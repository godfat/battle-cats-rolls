# frozen_string_literal: true

module BattleCatsRolls
  class CrystalBall < Struct.new(:cats, :events)
    def dump dir
      require 'fileutils'
      require 'yaml'

      FileUtils.mkdir_p(dir)
      File.write("#{dir}/cats.yaml", YAML.dump(to_hash))
    end

    def to_hash
      @to_hash ||=
        {'cats' => cats.cats,
         'gacha' => cats.gacha,
         'events' => events.gacha}
    end
  end
end
