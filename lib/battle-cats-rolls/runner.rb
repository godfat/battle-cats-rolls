# frozen_string_literal: true

module BattleCatsRolls
  class Runner < Struct.new(:lang, :version, :event_url, :apk_url)
    def self.en
      @en ||= [
        __method__,
        '7.4.0',
        'https://ponos.s3.dualstack.ap-northeast-1.amazonaws.com/appli/battlecats/event_data/battlecatsen_production/gatya.tsv',
        # https://www.apkmirror.com/apk/ponos/the-battle-cats/
        'https://www.apkmirror.com/wp-content/themes/APKMirror/download.php?id=505303'
      ]
    end

    def self.tw
      @tw ||= [
        __method__,
        '7.4.0',
        'https://ponos.s3.dualstack.ap-northeast-1.amazonaws.com/appli/battlecats/event_data/battlecatstw_production/gatya.tsv',
        # https://apkpure.com/a/jp.co.ponos.battlecatstw
        'https://download.apkpure.com/b/apk/anAuY28ucG9ub3MuYmF0dGxlY2F0c3R3XzcwNDAwMF82OWZiOGJiYw?_fn=6LKT5ZKq5aSn5oiw54itX3Y3LjQuMF9hcGtwdXJlLmNvbS5hcGs&k=f57df15a9baefc727d13528fbbf8fd005bb8f8d8&as=3d399c8dd956bb27731555744d5d4d145bb65650&_p=anAuY28ucG9ub3MuYmF0dGxlY2F0c3R3&c=2%7CGAME_CASUAL%7CZGV2PVBPTk9TJnQ9YXBrJnZuPTcuNC4wJnZjPTcwNDAwMA'
      ]
    end

    def self.locale lang
      public_send(lang || :en)
    end

    def self.build lang=nil
      runner = new(*locale(lang))

      runner.write_events
      runner.write_data
    end

    def self.extract lang=nil, dir=nil
      new(*locale(lang)).extract(dir)
    end

    def self.list lang=nil, dir=nil
      new(*locale(lang)).list(dir)
    end

    def self.favicon lang=nil
      new(*locale(lang)).favicon
    end

    def extract dir=nil
      require_relative 'pack_reader'
      require 'fileutils'

      each_list(dir) do |file|
        reader = PackReader.new(file)

        dir = "#{extract_path}/#{reader.name}.pack"
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

    def favicon
      require_relative 'pack_reader'
      require 'fileutils'

      reader = PackReader.new("#{app_data_path}/ImageLocal.list")

      dir = "#{extract_path}/#{reader.name}.pack"
      asset = "lib/battle-cats-rolls/asset/image"
      FileUtils.mkdir_p(dir)
      FileUtils.mkdir_p(asset)

      puts "Extracting #{reader.pack_path}"

      mapicon, data = reader.find do |filename, _|
        filename == 'mapicon.png'
      end

      path = "#{dir}/#{mapicon}"

      File.binwrite(path, data.call)

      puts "Cropping #{path}"

      # Install ImageMagick for this
      system('convert', '-crop', '60x60+60+0', path, "#{asset}/treasure.png")
    end

    def write_events
      current = download_current_event_data

      last_date = current.gacha.
        delete_if { |_, data| data['platinum'] }.
        sort_by { |key, data| data['end_on'] }.
        dig(-1, -1, 'end_on').
        strftime('%Y%m%d')

      require 'fileutils'
      FileUtils.mkdir_p(event_path)

      File.write("#{event_path}/#{last_date}.tsv", current.tsv)
    end

    def download_current_event_data
      puts "Downloading event data..."

      require_relative 'tsv_reader'

      TsvReader.download(event_url)
    end

    def write_data
      require_relative 'events_reader'
      require_relative 'crystal_ball'

      events = EventsReader.read(event_path)
      ball = CrystalBall.from_pack_and_events(cats_pack, events)

      puts "Writing data..."

      ball.dump('build', lang)
    end

    def cats_pack
      require_relative 'cats_builder'

      CatsBuilder.new(provider)
    end

    def provider
      if File.exist?(extract_path)
        load_extract
      elsif File.exist?(app_data_path)
        load_pack
      else
        download_apk unless File.exist?(apk_path)
        write_pack
        load_pack
      end
    end

    def download_apk
      puts "Downloading APK..."

      require 'fileutils'
      FileUtils.mkdir_p(app_data_path)

      system(
        'wget',
        '--user-agent=Mozilla/5.0',
        '-O', apk_path,
        apk_url)
    end

    def write_pack
      paths =
        %w[DataLocal resLocal ImageLocal].product(
          ['.list', '.pack']).map(&:join).map do |name|
          "assets/#{name}"
        end

      system('unzip', apk_path, *paths, '-d', app_data_path)

      require 'fileutils'
      assets = Dir["#{app_data_path}/assets/*"]
      FileUtils.mv(assets, app_data_path, verbose: true)
      FileUtils.rmdir("#{app_data_path}/assets", verbose: true)
    end

    def each_list dir=nil
      root = dir || app_data_path

      Dir["#{root}/**/*.list"].each do |file|
        yield(file)
      end
    end

    def load_extract
      puts "Loading from extract..."

      require_relative 'extract_provider'

      ExtractProvider.new(extract_path)
    end

    def load_pack
      puts "Loading from pack..."

      require_relative 'pack_provider'

      PackProvider.new(app_data_path)
    end

    def event_path
      @event_path ||= "data/#{lang}/events"
    end

    def extract_path
      @extract_path ||= "extract/#{lang}/#{version}"
    end

    def app_data_path
      @data_path ||= "data/#{lang}/#{version}/app"
    end

    def apk_path
      @apk_path ||= "data/#{lang}/#{version}/bc-#{lang}.apk"
    end
  end
end
