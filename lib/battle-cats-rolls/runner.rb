# frozen_string_literal: true

module BattleCatsRolls
  class Runner < Struct.new(:lang, :version, :event_url, :apk_url)
    def self.en
      @en ||= [
        __method__,
        '7.5.0',
        'https://ponos.s3.dualstack.ap-northeast-1.amazonaws.com/appli/battlecats/event_data/battlecatsen_production/gatya.tsv',
        # https://www.apkmirror.com/apk/ponos/the-battle-cats/
        'https://www.apkmirror.com/wp-content/themes/APKMirror/download.php?id=531837'
      ]
    end

    def self.tw
      @tw ||= [
        __method__,
        '7.4.2',
        'https://ponos.s3.dualstack.ap-northeast-1.amazonaws.com/appli/battlecats/event_data/battlecatstw_production/gatya.tsv',
        # https://apkpure.com/a/jp.co.ponos.battlecatstw
        'https://download.apkpure.com/b/apk/anAuY28ucG9ub3MuYmF0dGxlY2F0c3R3XzcwNDAyMF82YmI1NTFkNw?_fn=6LKT5ZKq5aSn5oiw54itX3Y3LjQuMl9hcGtwdXJlLmNvbS5hcGs&k=a4b66406a3c23cdb284f39320b97bc6e5bd97113&as=e718be834e37f6f6bff6fb9a1d71357f5bd6ce8b&_p=anAuY28ucG9ub3MuYmF0dGxlY2F0c3R3&c=2%7CGAME_CASUAL%7CZGV2PVBPTk9TJnQ9YXBrJnZuPTcuNC4yJnZjPTcwNDAyMA'
      ]
    end

    def self.jp
      @jp ||= [
        __method__,
        '8.0.2',
        'https://ponos.s3.dualstack.ap-northeast-1.amazonaws.com/appli/battlecats/event_data/battlecats_production/gatya.tsv',
        # https://apkpure.co/%E3%81%AB%E3%82%83%E3%82%93%E3%81%93%E5%A4%A7%E6%88%A6%E4%BA%89/
        'https://apk.apkpure.co/jp.co.ponos.battlecats/0e17213d7197ab35b55978c00de4d3c3/8.0.2/d88ea0deccb640d51eb14b86a1a8aa76/'
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
