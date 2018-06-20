
require 'openssl'

module BattleCatsRolls
  class Unpacker < Struct.new(:key, :bad_data)
    def initialize(new_key)
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
