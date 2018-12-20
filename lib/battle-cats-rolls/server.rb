# frozen_string_literal: true

require_relative 'web'

require 'jellyfish'
require 'rack'

module BattleCatsRolls
  Server = Jellyfish::Builder.app do
    use Rack::CommonLogger
    use Rack::Chunked
    use Rack::ContentLength
    use Rack::Deflater
    use Rack::ContentType, 'text/html; charset=utf-8'

    rewrite \
      '/asset' => '',
      '/robots.txt' => '/robots.txt' do
      run Rack::File.new(File.expand_path('asset', __dir__))
    end

    map '/seek', to: '/seek', host: ENV['SEEK_HOST'] do
      run Web::Seek.new
    end

    map '/', host: ENV['WEB_HOST'] do
      run Web.new
    end
  end

  def self.warmup app
    base = "http://#{ENV['WEB_HOST']}" if ENV['WEB_HOST']

    print Rack::MockRequest.new(app).get("#{base}/warmup").errors

    @shutdown = false
    monitor_memory
  end

  def self.shutdown
    monitor_memory.wakeup
    @shutdown = true
    monitor_memory.join
    SeekSeed::Pool.shutdown
  end

  def self.monitor_memory
    @monitor_memory ||= Thread.new do
      Kernel.at_exit(&method(:shutdown))

      until @shutdown do
        printf \
          "Memory total: %.2fM, current: %.2fM, CPU: %.2f%%,%s\n",
          *ps, `uptime`[/(?<=users,).+\z/m]
        sleep(10)
      end

      puts "Shutting down"
    end
  end

  def self.ps
    cpid = Process.pid

    `ps -Ao pid,rss,pcpu`.scan(/(\d+)\s+(\d+)\s+(\d+\.\d+)/).
      inject([0, 0, 0]) do |result, (pid, rss, pcpu)|
        mem = rss.to_f / 1024
        cpu = pcpu.to_f
        result[0] += mem

        if pid.to_i == cpid
          result[1] = mem
          result[2] = cpu
        end

        result
      end
  end
end
