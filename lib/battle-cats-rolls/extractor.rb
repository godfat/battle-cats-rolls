
module BattleCatsRolls
  class Extractor < Struct.new(:list_path, :pack_path)
    def initialize new_list_path
      name = new_list_path[0...new_list_path.rindex('.')]

      super(new_list_path, "#{name}.pack")
    end

    def list_data
      @list_data ||= File.binread(list_path)
    end

    def pack_data
      @pack_data ||= File.binread(pack_path)
    end
  end
end
