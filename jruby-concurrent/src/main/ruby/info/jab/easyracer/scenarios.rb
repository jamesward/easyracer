# frozen_string_literal: true

require "concurrent"
require "digest"
require "net/http"
require "securerandom"
require "rjack-logback"
require "uri"

java_import java.lang.management.ManagementFactory
java_import com.sun.management.OperatingSystemMXBean

class Scenarios
  LOG = RJack::SLF4J["info.jab.easyracer.Scenarios"]

  # No enum in JRuby, so we use symbols
  LEFT = :left
  RIGHT = :right

  # JDK 21+ virtual-thread-per-task executor exposed as concurrent-ruby's ExecutorService.
  # JDK 25+ only exposes Executors.newVirtualThreadPerTaskExecutor() (no ThreadFactory overload in this JVM),
  # so we assign a distinct thread name per submitted task so log patterns like %thread stay readable.
  class VirtualThreadExecutorService < Concurrent::JavaExecutorService
    TASK_NAME_SEQ = java.util.concurrent.atomic.AtomicLong.new(0)

    def post(*args, &task)
      raise ArgumentError, "no block given" unless block_given?

      wrapped = proc do
        thr = java.lang.Thread.current_thread
        prior = thr.name
        thr.name = "easyracer-vt-#{TASK_NAME_SEQ.get_and_increment}"
        begin
          task.call(*args)
        ensure
          thr.name = prior
        end
      end
      super(*args, &wrapped)
    end

    private

    def ns_initialize(opts = {})
      @fallback_policy = opts.fetch(:fallback_policy, :abort)
      @executor = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor
    end
  end

  class << self
    # Prefer virtual threads for blocking HTTP racers (e.g. scenario 3); falls back on older JDKs.
    def async_executor
      @async_executor ||= (VirtualThreadExecutorService.new(auto_terminate: false) rescue Concurrent.global_io_executor)
    end
  end

  private

  def join_uri(path)
    suffix = path.delete_prefix("/")
    URI.join("#{@base}/", suffix)
  end

  def sleep(seconds)
    Kernel.sleep(seconds)
  end

  def fetch_response(path, read_timeout: nil)
    uri = join_uri(path)
    limit = read_timeout.nil? ? 300 : read_timeout
    Net::HTTP.start(
      uri.hostname,
      uri.port,
      open_timeout: 5,
      read_timeout: limit,
      use_ssl: uri.scheme == "https"
    ) do |http|
      http.request(Net::HTTP::Get.new(uri))
    end
  end

  def response_to_result(response)
    return LEFT unless response.code.to_i == 200

    body = response.body.to_s.strip
    body == "right" ? RIGHT : LEFT
  end

  def request(path, read_timeout: nil)
    response_to_result(fetch_response(path, read_timeout: read_timeout))
  rescue StandardError
    LEFT
  end

  # First RIGHT wins. Stop publishing late LEFT results once a winner is observed.
  def race(builders)
    completion = Queue.new
    winner = Concurrent::AtomicBoolean.new(false)
    futures = builders.map do |builder|
      Concurrent::Future.execute(executor: self.class.async_executor) do
        result = (builder.call rescue LEFT)
        completion.push(result) if result == RIGHT || !winner.true?
      end
    end

    builders.size.times do
      if completion.pop == RIGHT
        winner.make_true
        return RIGHT
      end
    end
    LEFT
  ensure
    winner&.make_true
    futures&.each { |f| f.cancel rescue nil }
  end

  def scenario8_flow
    open_res = fetch_response("/8?open")
    return LEFT unless open_res.code.to_i == 200

    resource_id = open_res.body.to_s.strip
    LOG.info("Scenario 8: opened resource id=#{resource_id}")
    verdict = LEFT
    begin
      use_res = fetch_response("/8?use=#{resource_id}")
      verdict = response_to_result(use_res)
    ensure
      begin
        LOG.info("Scenario 8: closing resource id=#{resource_id}")
        fetch_response("/8?close=#{resource_id}")
      rescue StandardError
        nil
      end
    end
    verdict
  rescue StandardError
    LEFT
  end

  def scenario10_poll_loop(id, cancelled)
    os_bean = ManagementFactory.get_platform_mx_bean(OperatingSystemMXBean.java_class)
    processors = Java::java.lang.Runtime.get_runtime.available_processors
    loop do
      load = os_bean.get_process_cpu_load
      load = 0.0 if load.negative?
      sample = load * processors
      response = fetch_response("/10?#{id}=#{sample}")
      code = response.code.to_i
      if code >= 200 && code < 300
        cancelled.make_true
        return response_to_result(response)
      elsif code >= 300 && code < 400
        sleep(1)
      else
        cancelled.make_true
        return LEFT
      end
    end
  end

  public

  # Analogous concept of Constructor in Java
  def initialize(base_url)
    @base = base_url.to_s.chomp("/")
  end

  def scenario1
    LOG.info("Scenario 1")
    race([
      -> { request("/1") },
      -> { request("/1") }
    ])
  end

  def scenario2
    LOG.info("Scenario 2")
    race([
      -> { request("/2") },
      -> { request("/2") }
    ])
  end

  def scenario3
    LOG.info("Scenario 3")
    race(
      Array.new(10_000) { 
        -> { request("/3") 
      } 
    })
  end

  def scenario4
    LOG.info("Scenario 4")
    race([
      -> { request("/4", read_timeout: 1) },
      -> { request("/4") }
    ])
  end

  def scenario5
    LOG.info("Scenario 5")
    race([
      -> { request("/5") },
      -> { request("/5") }
    ])
  end

  def scenario6
    LOG.info("Scenario 6")
    race([
      -> { request("/6") },
      -> { request("/6") },
      -> { request("/6") }
    ])
  end

  # Hedging: primary GET immediately, duplicate GET after ≥3s (server checks ~2s gap).
  def scenario7
    LOG.info("Scenario 7")
    race([
      -> { request("/7") },
      -> { sleep(3); request("/7") }
    ])
  end

  # Race two open→use→close pipelines; always close in ensure (server couples winner to close id).
  def scenario8
    LOG.info("Scenario 8")
    race([-> { scenario8_flow }, -> { scenario8_flow }])
  end

  def scenario9
    LOG.info("Scenario 9")
    observations = Concurrent::Array.new
    futures = Array.new(10) do
      Concurrent::Future.execute(executor: self.class.async_executor) do
        resp = fetch_response("/9")
        observations << { at: Time.now, resp: resp }
      rescue StandardError
        observations << { at: Time.now, resp: nil }
      end
    end
    futures.each do |f|
      raise "scenario 9 timeout" unless f.wait(120)
    end
    pieces = observations.sort_by { |row| row[:at] }
      .filter_map { |row| row[:resp] }
      .select { |r| r.code.to_i == 200 }
      .map { |r| r.body.to_s.strip }
    pieces.join == "right" ? RIGHT : LEFT
  end

  # Blocker connection + CPU digest loop + load polling (server checks mean load during block).
  def scenario10
    LOG.info("Scenario 10")
    id = SecureRandom.uuid.to_s
    cancelled = Concurrent::AtomicBoolean.new(false)

    cpu_future = Concurrent::Future.execute(executor: self.class.async_executor) do
      buf = SecureRandom.random_bytes(512)
      buf = Digest::SHA512.digest(buf) until cancelled.true?
    end

    blocker_future = Concurrent::Future.execute(executor: self.class.async_executor) do
      begin
        fetch_response("/10?#{id}")
      rescue StandardError
        nil
      ensure
        cancelled.make_true
      end
    end

    verdict = scenario10_poll_loop(id, cancelled)
    blocker_future.wait(120)
    cpu_future.cancel rescue nil
    verdict
  end

  # Triple concurrent GET: third arrival wins (README: nested race semantics;
  def scenario11
    LOG.info("Scenario 11")
    race(Array.new(3) { -> { request("/11") } })
  end
end
