# frozen_string_literal: true

require 'forwardable'

module BattleCatsRolls
  class CrystalBall < Struct.new(:data)
    def self.from_pack_and_events pack, events
      new(
        'cats' => pack.cats,
        'gacha' => pack.gacha,
        'events' => events.gacha)
    end

    def self.load dir, lang
      require 'yaml'

      new(YAML.load_file("#{dir}/bc-#{lang}.yaml"))
    end

    extend Forwardable

    def_delegators :data, :dig

    def dump dir, lang
      require 'fileutils'
      require 'yaml'

      FileUtils.mkdir_p(dir)
      File.write("#{dir}/bc-#{lang}.yaml", dump_yaml)
    end

    def dump_yaml
      visitor = Psych::Visitors::YAMLTree.create
      visitor << data
      visitor.tree.grep(Psych::Nodes::Sequence).each do |seq|
        seq.style = Psych::Nodes::Sequence::FLOW
      end

      visitor.tree.yaml(nil, line_width: -1)
    end
  end
end
