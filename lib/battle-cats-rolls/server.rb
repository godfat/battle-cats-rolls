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

    map '/asset' do
      run Rack::Directory.new(File.expand_path('asset', __dir__))
    end

    map '/seek', to: '/seek', host: ENV['SEEK_HOST'] do
      run Web::Seek.new
    end

    map '/', host: ENV['WEB_HOST'] do
      run Web.new
    end
  end
end
