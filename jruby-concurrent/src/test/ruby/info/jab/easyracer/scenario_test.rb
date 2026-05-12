# frozen_string_literal: true

unless RUBY_ENGINE == "jruby"
  warn "Easy Racer integration tests require JRuby (detected #{RUBY_ENGINE} #{RUBY_VERSION}). Use: jruby -S bundle exec rake test"
  exit 1
end

require "minitest/autorun"
require "net/http"
require "timeout"
require "uri"
require "testcontainers"

require "info/jab/easyracer/scenarios"

class ScenarioTest < Minitest::Test
  EASY_RACER_IMAGE = "ghcr.io/jamesward/easyracer"
  # When set (e.g. http://127.0.0.1:8080), skip Ruby Testcontainers and hit this server directly.
  # Use when Docker API is unreliable or you already ran: docker run -p 8080:8080 ghcr.io/jamesward/easyracer
  URL_ENV_KEY = "EASYRACER_URL"

  # One container + one Scenarios for the whole class; per-test lifecycle is too heavy for per-test setup.
  @@suite = {
    mutex: Mutex.new,
    container: nil,
    scenarios: nil,
    registered_cleanup: false,
    fatal: nil
  }

  def setup
    self.class.ensure_suite!
    raise @@suite[:fatal] if @@suite[:fatal]

    @scenarios = @@suite[:scenarios]
    refute_nil @scenarios, "scenarios should exist after suite init"
  end

  def self.ensure_suite!
    @@suite[:mutex].synchronize do
      return if @@suite[:scenarios]
      # Do not hammer Docker/Testcontainers once startup has failed in this JVM
      return if @@suite[:fatal]

      begin
        from_env = ENV[URL_ENV_KEY]&.strip
        if from_env && !from_env.empty?
          base_url = from_env.chomp("/")
          wait_for_server(base_url)
          @@suite[:container] = nil
          @@suite[:scenarios] = Scenarios.new(base_url)
        else
          container = start_easyracer_container
          base_url = "http://#{container.host}:#{container.mapped_port(8080)}"
          wait_for_server(base_url)
          @@suite[:container] = container
          @@suite[:scenarios] = Scenarios.new(base_url)
        end

        unless @@suite[:registered_cleanup]
          Minitest.after_run { ScenarioTest.teardown_suite! }
          @@suite[:registered_cleanup] = true
        end
      rescue StandardError => e
        @@suite[:fatal] = e
        @@suite[:scenarios] = nil
        warn "[easyracer] #{e.class}: #{e.message} | export #{URL_ENV_KEY}=http://127.0.0.1:8080 when the scenario server runs outside Testcontainers (see README)."
      end
    end
  end

  def self.start_easyracer_container
    ensure_docker_image!(EASY_RACER_IMAGE)

    without_redundant_image_create(EASY_RACER_IMAGE) do
      container = Testcontainers::DockerContainer.new(EASY_RACER_IMAGE).with_exposed_port(8080)
      container.start
      container
    end
  end

  def self.ensure_docker_image!(image)
    return if system("docker", "image", "inspect", image, out: File::NULL, err: File::NULL)

    Timeout.timeout(60) do
      raise "docker pull failed for #{image}" unless system("docker", "pull", image)
    end
  rescue Timeout::Error
    raise "timed out pulling #{image}"
  end

  # testcontainers-ruby always calls Docker::Image.create before starting a container.
  # On JRuby + Docker Desktop this streaming pull call can hang even when the image is already local.
  # Keep Testcontainers for lifecycle/ports, but use the pre-verified local image instead.
  def self.without_redundant_image_create(image)
    singleton = class << Docker::Image; self; end
    original_create = Docker::Image.method(:create)

    singleton.define_method(:create) do |opts = {}, creds = nil, conn = Docker.connection, &block|
      requested = opts["fromImage"] || opts[:fromImage]
      return Docker::Image.get(image, {}, conn) if requested == image

      original_create.call(opts, creds, conn, &block)
    end

    yield
  ensure
    singleton&.define_method(:create, original_create)
  end

  def self.teardown_suite!
    @@suite[:mutex].synchronize do
      @@suite[:scenarios]&.close
      @@suite[:scenarios] = nil
      @@suite[:fatal] = nil
      if (container = @@suite[:container])
        container.stop
        container.delete
      end
      @@suite[:container] = nil
    end
  end

  # Integration scope is intentionally minimal for now; add entries here as scenarios are implemented.
  {
    1 => :scenario1
  }.each do |number, method|
    define_method("test_scenario_#{number}") do
      assert_equal Scenarios::RIGHT, @scenarios.public_send(method),
                   "scenario #{number} should resolve to RIGHT"
    end
  end

  # Probe the root path, not a scenario path. Scenario endpoints (e.g. /1) only respond when the
  # expected number of concurrent racers connect, so a single GET hangs and turns startup into a long delay.
  def self.wait_for_server(base_url, attempts = 40)
    probe = URI.parse("#{base_url}/")
    attempts.times do
      Net::HTTP.start(probe.hostname, probe.port, open_timeout: 2, read_timeout: 2,
                                                  use_ssl: probe.scheme == "https") do |http|
        response = http.request(Net::HTTP::Get.new(probe))
        return if response.code.to_i < 500
      end
    rescue StandardError
      sleep 0.25
    end
    raise "easyracer did not become ready at #{base_url}"
  end
end
