defmodule EasyRacerTest do
  use ExUnit.Case, async: false

  alias Testcontainers.Container
  alias Testcontainers.HttpWaitStrategy
  alias Testcontainers.PullPolicy

  @image "ghcr.io/jamesward/easyracer"

  setup_all do
    # `pull_if_missing` because `test/test_helper.exs` already pulled the image
    # via the Docker CLI; skipping the default `always_pull` keeps the slow pull
    # out of the hardcoded 300s `Testcontainers.start_container/1` timeout.
    config =
      Container.new(@image)
      |> Container.with_exposed_port(8080)
      |> Container.with_pull_policy(PullPolicy.pull_if_missing())
      |> Container.with_waiting_strategy(HttpWaitStrategy.new("/", 8080, status_code: 200))

    {:ok, container} = Testcontainers.start_container(config)

    on_exit(fn ->
      Testcontainers.stop_container(container.container_id)
    end)

    host = Testcontainers.get_host(container)
    port = Testcontainers.get_port(container, 8080)
    base_url = "http://#{host}:#{port}"

    {:ok, base_url: base_url}
  end

  test "scenario 1 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario1(base_url) == "right"
  end

  test "scenario 2 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario2(base_url) == "right"
  end

  # 10k concurrent TCP connections is unreliable on macOS (and other non-Linux OSes);
  # see easyracer kotlin-coroutines / java-cf notes. Linux CI matches typical deployment.
  if match?({:unix, :linux}, :os.type()) do
    @tag timeout: 300_000
    test "scenario 3 returns right", %{base_url: base_url} do
      assert EasyRacer.scenario3(base_url) == "right"
    end
  else
    @tag skip: "Scenario 3 (10,000 concurrent connections) runs on Linux only"
    test "scenario 3 returns right", %{base_url: base_url} do
      assert EasyRacer.scenario3(base_url) == "right"
    end
  end

  test "scenario 4 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario4(base_url) == "right"
  end

  test "scenario 5 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario5(base_url) == "right"
  end

  test "scenario 6 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario6(base_url) == "right"
  end

  test "scenario 7 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario7(base_url) == "right"
  end

  test "scenario 8 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario8(base_url) == "right"
  end

  test "scenario 9 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario9(base_url) == "right"
  end

  @tag timeout: 60_000
  test "scenario 10 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario10(base_url) == "right"
  end

  test "scenario 11 returns right", %{base_url: base_url} do
    assert EasyRacer.scenario11(base_url) == "right"
  end
end
