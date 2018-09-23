# frozen_string_literal: true

require 'promise_pool'
require 'digest/sha1'

module BattleCatsRolls
  class Seek < Struct.new(:source, :key, :promise, :seed, :previous_count)
    Pool = PromisePool::ThreadPool.new(1)
    Mutex = Mutex.new

    def self.processed
      @processed ||= 0
    end

    def self.finishing key, seed
      Mutex.synchronize do
        cache = queue.dig(key, :cache)
        cache[key] = seed
        queue.delete(key)
        @processed += 1
      end
    end

    def self.enqueue source, cache
      key = Digest::SHA1.hexdigest(source)

      cache[key] || queue[key] = {seek: new(source, key).start, cache: cache}

      key
    end

    def self.queue
      @queue ||= {}
    end

    def start
      Mutex.synchronize do
        enqueue
      end

      self
    end

    def started?
      promise.started?
    end

    def ended?
      promise.resolved?
    end

    def yield
      promise.yield
    end

    def position
      previous_count - self.class.processed + 1
    end

    private

    def enqueue
      self.previous_count = Pool.queue_size + self.class.processed
      self.promise = PromisePool::Promise.new.defer(Pool) do
        self.seed = seek
        self.class.finishing(key, seed)
      end
    end

    def seek
      IO.popen([
        'Seeker/Seeker',
        *ENV['SEEKER_OPT'].to_s.split(' '),
        err: %i[child out]], 'r+') do |io|
        io.puts source
        io.close_write
        io.read.to_i
      end
    end
  end
end
