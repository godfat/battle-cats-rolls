
require 'yaml'

module BattleCatsRolls
  class Dumper < Struct.new(:dir, :builder)
    def dump
      File.write("#{dir}/cats.yaml", YAML.dump(Hash[builder.cats.sort]))
    end
  end
end
