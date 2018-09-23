
module BattleCatsRolls
  module Cache
    EXPIRES_IN = 86400
    LRU_SIZE = 8192

    module_function
    def default logger
      @cache ||= Cache.pick(logger)
    end

    def pick logger
      memcache(logger) || lru_cache(logger)
    end

    def memcache logger
      require 'dalli'
      client = Dalli::Client.new(nil, :expires_in => EXPIRES_IN)
      File.open(IO::NULL) do |null|
        Dalli.logger = Logger.new(null)
        client.alive!
        Dalli.logger = logger
      end
      logger.info("Memcached connected to #{client.version.keys.join(', ')}")
      client.extend(RestCore::DalliExtension)
      client
    rescue LoadError, Dalli::RingError => e
      logger.debug("Skip memcached because: #{e}")
      nil
    end

    def lru_cache logger
      require 'lru_redux'
      logger.info("LRU cache size: #{LRU_SIZE}")
      cache = LruRedux::ThreadSafeCache.new(LRU_SIZE)
      cache.extend(Module.new{
        def fetch key # original fetch could deadlock
          self[key] || self[key] = yield
        end
      })
      cache
    rescue LoadError => e
      logger.debug("Skip LRU cache because: #{e}")
      nil
    end
  end
end
