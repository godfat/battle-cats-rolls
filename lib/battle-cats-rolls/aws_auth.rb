# frozen_string_literal: true

require 'uri'
require 'time'
require 'digest'
require 'openssl'

module BattleCatsRolls
  # https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html
  # https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
  class AwsAuth < Struct.new(:verb, :url, :id, :secret, :region, :service)
    def id
      super || self.id = ENV['AWSAccessKeyId']
    end

    def secret
      super || self.secret = ENV['AWSSecretAccessKey']
    end

    def region
      super || self.region = 'ap-northeast-1'
    end

    def service
      super || self.service = 's3'
    end

    def uri
      @uri ||= URI.parse(url)
    end

    def refresh
      self.class.new(*to_a)
    end

    def headers
      @headers ||= {
        'Authorization' => authorization
      }.merge(signing_headers)
    end

    def to_curl
      options = headers.map{ |key, value| "-H '#{key}: #{value}'" }.join(' ')

      "curl -X #{verb_upcase} #{options} #{url}"
    end

    private

    def authorization
      @authorization ||=
        "#{algorithm} Credential=#{credential}" \
          ", SignedHeaders=#{signed_headers}" \
          ", Signature=#{signature}"
    end

    def algorithm
      'AWS4-HMAC-SHA256'
    end

    def credential
      "#{id}/#{scope}"
    end

    def scope
      "#{date}/#{region}/#{service}/#{version}"
    end

    def version
      'aws4_request'
    end

    def verb_upcase
      verb.to_s.upcase
    end

    def host
      uri.host
    end

    def path
      uri.path
    end

    def date
      @date ||= time.strftime('%Y%m%d')
    end

    def time_iso8601
      @time_iso8601 ||= time.strftime('%Y%m%dT%H%M%SZ')
    end

    def time
      @time ||= Time.now.utc
    end

    def signed_headers
      @signed_headers ||= signing_headers.keys.sort.join(';').downcase
    end

    def signing_headers
      @signing_headers ||= {
        'Host' => host,
        'X-Amz-Date' => time_iso8601,
        'X-Amz-Content-Sha256' => content_sha256
      }
    end

    def content_sha256
      @content_sha256 ||= sha256hex('')
    end

    def signature
      @signature ||= hmac_sha256hex(signing_key, string_to_sign)
    end

    def signing_key
      @signing_key ||=
        [date, region, service, version].
          inject("AWS4#{secret}", &method(:hmac_sha256))
    end

    def string_to_sign
      @string_to_sign ||= <<~STRING.chomp
        #{algorithm}
        #{time_iso8601}
        #{scope}
        #{sha256hex(canonical_request)}
      STRING
    end

    def canonical_request
      @canonical_request ||= <<~REQUEST.chomp
        #{verb_upcase}
        #{path}
        #{uri_encoded_queries}
        #{canonical_headers}

        #{signed_headers}
        #{sha256hex('')}
      REQUEST
    end

    def uri_encoded_queries
      @uri_encoded_queries ||= '' # NOT IMPLEMENTED YET
    end

    def canonical_headers
      @canonical_headers ||= signing_headers.sort.map do |(key, value)|
        "#{key.downcase}:#{value}"
      end.join("\n")
    end

    def sha256hex data
      Digest::SHA256.hexdigest(data)
    end

    def hmac_sha256hex key, data
      OpenSSL::HMAC.hexdigest('sha256', key, data)
    end

    def hmac_sha256 key, data
      OpenSSL::HMAC.digest('sha256', key, data)
    end
  end
end
