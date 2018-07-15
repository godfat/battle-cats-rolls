
configru = File.expand_path(__dir__, 'config.ru')

app :rack, configru, preload: true do
  listen ENV['SEEK_YAHNS'] || 9090

  queue do
    worker_threads 25
  end
end

app :rack, configru, preload: true do
  listen ENV['WEB_YAHNS'] || 8080

  queue do
    worker_threads 5
  end
end
