
require_relative 'extractor'
require_relative 'unpacker'

require 'forwardable'

module BattleCatsRolls
  class PackReader < Struct.new(
    :extractor, :list_unpacker, :pack_unpacker, :name)

    extend Forwardable

    def_delegators :extractor, :list_path, :pack_path

    def initialize list_path
      super(
        Extractor.new(list_path),
        Unpacker.for_list,
        Unpacker.for_pack,
        File.basename(list_path, '.list'))
    end

    def each
      if block_given?
        list_lines.each do |line|
          filename, offset, size = line.split(',')
          data = pack_unpacker.decrypt(
            extractor.pack_data[offset.to_i, size.to_i])

          yield(filename, data)
        end
      else
        to_enum(__method__)
      end
    end

    private

    def list_lines
      # Drop first line for number of files
      list_unpacker.decrypt(extractor.list_data).lines.drop(1)
    end
  end
end
