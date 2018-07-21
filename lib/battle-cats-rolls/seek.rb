# frozen_string_literal: true

require 'promise_pool'

module BattleCatsRolls
  class Seek < Struct.new(:source, :promise, :seed, :previous_count)
    Pool = PromisePool::ThreadPool.new(1)
    Mutex = Mutex.new

    def self.processed
      @processed ||= 0
    end

    def self.incr_processed
      Mutex.synchronize do
        @processed += 1
      end
    end

    def start
      Mutex.synchronize do
        enqueue
      end
    end

    def started?
      promise.started?
    end

    def ended?
      promise.resolved?
    end

    def position
      previous_count - self.class.processed
    end

    private

    def enqueue
      self.previous_count = Pool.queue_size + self.class.processed
      self.promise = PromisePool::Promise.new.defer(Pool) do
        # IO.popen([
        #   'Seeker/Seeker',
        #   *ENV['SEEKER_OPT'].to_s.split(' '),
        #   err: %i[child out]], 'r+') do |io|
        #   io.puts source
        #   io.close_write
        #   self.seed = io.read.to_i
        # end
        sleep(10)
        self.seed = rand

        self.class.incr_processed
      end
    end
  end
end
