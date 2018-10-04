# frozen_string_literal: true

module BattleCatsRolls
  module Runner
    module_function

    def version
      '7.3.0'
    end

    def build
      write_events
      write_data
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

    def write_events
      puts "Downloading event data..."

      require_relative 'tsv_reader'

      current = TsvReader.current
      last_date = current.gacha.
        delete_if { |_, data| data['platinum'] }.
        sort_by { |key, data| data['end_on'] }.
        dig(-1, -1, 'end_on').
        strftime('%Y%m%d')
      File.write("data/events/#{last_date}.tsv", current.tsv)
    end

    def write_data
      require_relative 'events_reader'
      require_relative 'crystal_ball'

      events = EventsReader.read('data/events')
      ball = CrystalBall.from_pack_and_events(cats_pack, events)

      puts "Writing data..."

      ball.dump('build')
    end

    def cats_pack
      require_relative 'cats_builder'

      CatsBuilder.new(provider)
    end

    def provider
      if File.exist?("extract/#{version}")
        puts "Loading from extract..."

        require_relative 'extract_provider'

        ExtractProvider.new("extract/#{version}")
      else
        puts "Loading from pack..."

        require_relative 'pack_provider'

        PackProvider.new("data/#{version}/app")
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
