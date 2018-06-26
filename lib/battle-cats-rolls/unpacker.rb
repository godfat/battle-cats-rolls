# frozen_string_literal: true

require 'openssl'
require 'digest/md5'

module BattleCatsRolls
  class Unpacker < Struct.new(:key, :bad_data)
    def self.for_list
      new(Digest::MD5.hexdigest('pack')[0, 16])
    end

    def self.for_pack
      new(Digest::MD5.hexdigest('battlecats')[0, 16])
    end

    def initialize new_key
      super(new_key, false)
    end

    def decrypt data
      if bad_data
        data
      else
        cipher = OpenSSL::Cipher.new('aes-128-ecb')
        cipher.decrypt
        cipher.key = key
        cipher.update(data) + cipher.final
      end
    rescue OpenSSL::Cipher::CipherError => e
      warn "#{e.class}:#{e}, turning off decryption"
      self.bad_data = true
      data
    end
  end
end
