# frozen_string_literal: true

module BattleCatsRolls
  module Runner
    module_function

    def version
      '7.3.0'
    end

    def extract dir=nil
      require_relative 'pack_reader'
      require 'fileutils'

      each_list(dir) do |file|
        reader = PackReader.new(file)

        dir = "extract/#{version}/#{reader.name}.pack"
        FileUtils.mkdir_p(dir)

        puts "Extracting #{reader.pack_path}"

        reader.each do |filename, data|
          File.binwrite("#{dir}/#{filename}", data.call)
        end
      end
    end

    def list dir=nil
      require_relative 'unpacker'

      unpacker = Unpacker.for_list

      each_list(dir) do |file|
        puts "#{file}:"
        puts unpacker.decrypt(File.binread(file))
        puts "---"
      end
    end

    def each_list dir=nil
      root = dir || "data/#{version}"

      Dir["#{root}/**/*.list"].each do |file|
        yield(file)
      end
    end
  end
end
