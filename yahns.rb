
rack, = Rack::Builder.parse_file(File.expand_path(__dir__, 'config.ru'))

app :rack, rack, preload: true do
  listen ENV['SEEK_YAHNS'] || 9090

  queue do
    worker_threads 25
  end
end

app :rack, rack, preload: true do
  listen ENV['WEB_YAHNS'] || 8080

  queue do
    worker_threads 5
  end
end
