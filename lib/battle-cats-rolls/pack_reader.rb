
require_relative 'extractor'
require_relative 'unpacker'

require 'forwardable'

module BattleCatsRolls
  class PackReader < Struct.new(
    :extractor, :list_unpacker, :pack_unpacker, :name)

    extend Forwardable

    def_delegators :extractor, :list_path, :pack_path

    def initialize new_list_path
      super(
        Extractor.new(new_list_path),
        Unpacker.for_list,
        Unpacker.for_pack,
        File.basename(new_list_path, '.list'))
    end

    def each
      if block_given?
        list_lines.each do |line|
          yield(*read(line))
        end
      else
        to_enum(__method__)
      end
    end

    def list_lines
      # Drop first line for number of files
      @list_lines ||= list_unpacker.decrypt(extractor.list_data).lines.drop(1)
    end

    def read line
      filename, offset, size = line.split(',')
      data = pack_unpacker.decrypt(
        extractor.pack_data[offset.to_i, size.to_i])

      [filename, data]
    end
  end
end
