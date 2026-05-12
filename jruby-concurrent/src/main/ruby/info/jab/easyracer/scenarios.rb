# frozen_string_literal: true

require "concurrent"
require "net/http"
require "rjack-logback"
require "uri"

class Scenarios
  LOG = RJack::SLF4J["info.jab.easyracer.Scenarios"]

  LEFT = :left
  RIGHT = :right

  def initialize(base_url, announce: true)
    @base = base_url.to_s.chomp("/")
    @announce = announce
  end

  def close
    true
  end

  def scenario1
    announce_scenario(1)
    race_right([
      -> { get_scenario_result("/1") },
      -> { get_scenario_result("/1") }
    ])
  end

  def scenario2
    announce_scenario(2)
    race_right([
      -> { get_scenario_result("/2") },
      -> { get_scenario_result("/2") }
    ])
  end

  def scenario3
    announce_scenario(3)
    race_right(Array.new(10_000) { -> { get_scenario_result("/3") } })
  end

  def scenario4
    announce_scenario(4)
    race_right([
      -> { get_scenario_result("/4", read_timeout: 1) },
      -> { get_scenario_result("/4") }
    ])
  end

  def scenario5
    announce_scenario(5)
    race_right([
      -> { get_scenario_result("/5") },
      -> { get_scenario_result("/5") }
    ])
  end

  def scenario6
    announce_scenario(6)
    race_right([
      -> { get_scenario_result("/6") },
      -> { get_scenario_result("/6") },
      -> { get_scenario_result("/6") }
    ])
  end

  private

  def announce_scenario(number)
    return unless @announce

    LOG.info("Scenario #{number}")
  end

  # First RIGHT wins. concurrent-ruby schedules racers; an unbounded queue avoids producer blocking.
  def race_right(builders)
    completion = Queue.new
    futures = builders.map do |builder|
      Concurrent::Future.execute do
        completion.push((builder.call rescue LEFT))
      end
    end

    builders.size.times do
      return RIGHT if completion.pop == RIGHT
    end
    LEFT
  ensure
    futures&.each { |f| f.cancel rescue nil }
  end

  def get_scenario_result(path, read_timeout: nil)
    uri = join_uri(path)
    limit = read_timeout.nil? ? 300 : read_timeout
    response = Net::HTTP.start(
      uri.hostname,
      uri.port,
      open_timeout: 5,
      read_timeout: limit,
      use_ssl: uri.scheme == "https"
    ) do |http|
      http.request(Net::HTTP::Get.new(uri))
    end
    response_to_result(response)
  rescue StandardError
    LEFT
  end

  def join_uri(path)
    suffix = path.delete_prefix("/")
    URI.join("#{@base}/", suffix)
  end

  def response_to_result(response)
    return LEFT unless response.code.to_i == 200

    body = response.body.to_s.strip
    body == "right" ? RIGHT : LEFT
  end
end
