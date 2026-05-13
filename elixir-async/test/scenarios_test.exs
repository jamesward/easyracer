defmodule EasyRacer.ScenariosTest do
  use ExUnit.Case, async: false

  require Logger

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

  # Scenario 3 needs a long budget on Linux; scenario 10 had a 60s per-test budget before.
  # 10k concurrent TCP connections (scenario 3) is unreliable on macOS and other non-Linux OSes;
  # see easyracer kotlin-coroutines / java-cf notes. Linux CI matches typical deployment.
  @tag timeout: 400_000
  test "all scenarios return right", %{base_url: base_url} do
    # Not started as an application module; load before `String.to_existing_atom/1`.
    Code.ensure_loaded!(EasyRacer.Scenarios)

    linux? = match?({:unix, :linux}, :os.type())

    for n <- 1..11 do
      cond do
        n == 3 and not linux? ->
          Logger.info("Scenario 3 skipped")

        true ->
          fun = String.to_existing_atom("scenario#{n}")
          assert apply(EasyRacer.Scenarios, fun, [base_url]) == "right"
      end
    end
  end
end
