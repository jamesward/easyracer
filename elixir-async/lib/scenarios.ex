defmodule EasyRacer.Scenarios do
  require Logger

  @moduledoc """
  Easy Racer HTTP client. This module implements the obstacle scenarios from the
  [Easy Racer](https://github.com/jamesward/easyracer) suite.
  """

  ####################
  # Helper functions #
  ####################

  # Run every request in parallel, take the first `"right"`/`"left"` from any task (or timeout),
  # then kill the rest so leftover work cannot linger.
  defp race(requests, timeout_ms \\ 120_000) when is_list(requests) do
    tasks =
      Enum.map(requests, fn
        fun when is_function(fun, 0) ->
          Task.async(fun)

        {url, opts} when is_binary(url) and is_list(opts) ->
          Task.async(fn -> http_get_exclusive(url, opts) end)

        url when is_binary(url) ->
          Task.async(fn -> http_get_exclusive(url) end)
      end)

    try do
      await_first_right(tasks, System.monotonic_time(:millisecond) + timeout_ms)
    after
      Enum.each(tasks, &Task.shutdown(&1, :brutal_kill))
    end
  end

  # Split `tasks` into the task whose `.ref` matches `ref` (if any) and the rest; used when
  # correlating `Task` messages with the still-running task list.
  defp pop_task_by_ref(tasks, ref) do
    {matches, remaining} = Enum.split_with(tasks, &(&1.ref == ref))
    {List.first(matches), remaining}
  end

  # Poll the caller mailbox for task replies: return the first `"right"`/`"left"` from a
  # successful `{:ok, text}`; drop failed or irrelevant replies and crashed tasks; if the
  # deadline passes or no tasks remain, default to `"left"`.
  defp await_first_right([], _deadline_ms), do: "left"

  defp await_first_right(tasks, deadline_ms) do
    now = System.monotonic_time(:millisecond)

    if now > deadline_ms do
      "left"
    else
      receive do
        {ref, {:ok, text}} when text in ["right", "left"] ->
          Process.demonitor(ref, [:flush])
          text

        {ref, _result} ->
          {_task, remaining} = pop_task_by_ref(tasks, ref)
          Process.demonitor(ref, [:flush])
          await_first_right(remaining, deadline_ms)

        {:DOWN, ref, :process, _pid, _reason} ->
          {_task, remaining} = pop_task_by_ref(tasks, ref)
          Process.demonitor(ref, [:flush])
          await_first_right(remaining, deadline_ms)
      after
        max(0, min(deadline_ms - now, 1_000)) ->
          await_first_right(tasks, deadline_ms)
      end
    end
  end

  # One socket per request: required for scenario 1 (first response waits for the
  # second connection at the server).
  defp http_get_exclusive(url, opts \\ []) when is_binary(url) and is_list(opts) do
    case http_get_response_exclusive(url, opts) do
      {:ok, %{status: status, body: body}} when status in 200..299 ->
        {:ok, String.trim(body)}

      {:ok, %{}} ->
        {:error, :status}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Minimal HTTP GET over a fresh TCP socket: connect, send request bytes, read until a
  # parsed response or recv deadline; always shut down the socket afterward.
  defp http_get_response_exclusive(url, opts \\ []) when is_binary(url) and is_list(opts) do
    uri = URI.parse(url)

    port = uri.port || default_port(uri.scheme)
    host = uri.host |> to_charlist()
    path = (uri.path || "/") <> query_string(uri.query)

    recv_timeout_ms = Keyword.get(opts, :recv_timeout_ms, 120_000)
    recv_deadline_ms = System.monotonic_time(:millisecond) + recv_timeout_ms

    tcp_opts = [:binary, active: false, packet: :raw]

    case :gen_tcp.connect(host, port, tcp_opts, :timer.seconds(10)) do
      {:ok, sock} ->
        try do
          req = request_bytes(uri, path, port)

          with :ok <- :gen_tcp.send(sock, req),
               {:ok, response} <- recv_response(sock, <<>>, recv_deadline_ms) do
            {:ok, update_in(response.body, &String.trim/1)}
          else
            {:error, reason} -> {:error, reason}
            _ -> {:error, :http}
          end
        after
          :gen_tcp.shutdown(sock, :read_write)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Incremental HTTP response read: recv chunks into `acc` until `parse_http_response/1`
  # succeeds, fails, or the overall recv deadline is exceeded.
  defp recv_response(sock, acc, recv_deadline_ms) do
    now = System.monotonic_time(:millisecond)

    if now >= recv_deadline_ms do
      {:error, :timeout}
    else
      chunk_timeout = min(max(recv_deadline_ms - now, 1), 60_000)

      case :gen_tcp.recv(sock, 0, chunk_timeout) do
        {:ok, data} when data != <<>> ->
          next = acc <> data

          case parse_http_response(next) do
            {:ok, response} -> {:ok, response}
            :more -> recv_response(sock, next, recv_deadline_ms)
            {:error, _} = e -> e
          end

        {:ok, <<>>} ->
          parse_http_response(acc)

        {:error, :closed} ->
          parse_http_response(acc)

        {:error, _} = e ->
          e
      end
    end
  end

  # Default TCP port when the URL omits an explicit `:port`.
  defp default_port("https"), do: 443
  defp default_port(_), do: 80

  # Append `?query` to a path, or `""` when there is no query component.
  defp query_string(nil), do: ""
  defp query_string(q), do: "?" <> q

  # Minimal HTTP/1.1 GET request bytes (Host + Connection: close) for `:gen_tcp.send/2`.
  defp request_bytes(uri, path, port) do
    host = uri.host
    scheme = uri.scheme || "http"
    default = default_port(scheme)

    host_header =
      if port == default, do: host, else: "#{host}:#{port}"

    [
      "GET ",
      path,
      " HTTP/1.1\r\nHost: ",
      host_header,
      "\r\nConnection: close\r\n\r\n"
    ]
  end

  # Split raw bytes into status + headers and body; return `:more` until `\r\n\r\n` is present.
  defp parse_http_response(data) do
    case :binary.split(data, <<"\r\n\r\n">>, []) do
      [headers, body] ->
        header_lines = String.split(headers, <<"\r\n">>)

        case List.first(header_lines) do
          <<"HTTP/1.1 ", status::binary-size(3), _::binary>> ->
            parse_status_line(status, header_lines, body)

          <<"HTTP/1.0 ", status::binary-size(3), _::binary>> ->
            parse_status_line(status, header_lines, body)

          _ ->
            {:error, :parse}
        end

      _ ->
        :more
    end
  end

  # Parse numeric status and combine with `parse_body/2` into a response map (or `:more`/error).
  defp parse_status_line(status, header_lines, body) do
    status = String.to_integer(status)

    case parse_body(header_lines, body) do
      {:ok, parsed_body} -> {:ok, %{status: status, body: parsed_body}}
      :more -> :more
      {:error, _} = e -> e
    end
  end

  # Resolve body from headers: full body when `Content-Length` is satisfied, or chunked decode.
  defp parse_body(header_lines, body) do
    headers =
      header_lines
      |> Enum.drop(1)
      |> Map.new(fn line ->
        case String.split(line, ":", parts: 2) do
          [key, value] -> {String.downcase(key), String.trim(value)}
          [key] -> {String.downcase(key), ""}
        end
      end)

    cond do
      length = headers["content-length"] ->
        length = String.to_integer(length)

        if byte_size(body) >= length do
          {:ok, binary_part(body, 0, length)}
        else
          :more
        end

      String.downcase(headers["transfer-encoding"] || "") =~ "chunked" ->
        decode_chunked(body)

      true ->
        :more
    end
  end

  # HTTP/1.1 chunked transfer: read hex size lines and chunk data until a zero-length chunk.
  defp decode_chunked(body), do: decode_chunked(body, <<>>)

  defp decode_chunked(body, acc) do
    case :binary.split(body, <<"\r\n">>, []) do
      [size_hex, rest] ->
        size_hex = size_hex |> String.split(";", parts: 2) |> hd()

        with {size, ""} <- Integer.parse(size_hex, 16),
             true <- byte_size(rest) >= size + 2 do
          chunk = binary_part(rest, 0, size)
          after_chunk = binary_part(rest, size + 2, byte_size(rest) - size - 2)

          if size == 0 do
            {:ok, acc}
          else
            decode_chunked(after_chunk, acc <> chunk)
          end
        else
          _ -> :more
        end

      _ ->
        :more
    end
  end

  # Join base URL and path with a single slash boundary (trims base, ensures path starts with `/`).
  defp join_url(base, path) do
    base = String.trim_trailing(base, "/")
    path = if String.starts_with?(path, "/"), do: path, else: "/" <> path
    base <> path
  end

  #############
  # Scenarios #
  #############

  @doc """
  Scenario 1: race two concurrent requests to `GET /1`.
  Returns the response body (`"right"` or `"left"`). On failure, returns `"left"`.
  """
  @spec scenario1(String.t()) :: String.t()
  def scenario1(base_url) when is_binary(base_url) do
    Logger.info("Scenario 1", pid: self())
    url = join_url(base_url, "/1")
    race([url, url])
  end

  @doc """
  Scenario 2: race two concurrent `GET /2` where the server interrupts one connection
  once both are open. Same race semantics as scenario 1.
  """
  @spec scenario2(String.t()) :: String.t()
  def scenario2(base_url) when is_binary(base_url) do
    Logger.info("Scenario 2", pid: self())
    url = join_url(base_url, "/2")
    race([url, url])
  end

  @doc """
  Scenario 3: race 10_000 concurrent `GET /3`. The server returns
  `"right"` only after all connections are open; each racer uses its own socket.
  """
  @spec scenario3(String.t()) :: String.t()
  def scenario3(base_url) when is_binary(base_url) do
    Logger.info("Scenario 3", pid: self())
    url = join_url(base_url, "/3")
    scenario_3_racers = 10_000

    url
    |> List.duplicate(scenario_3_racers)
    |> race(600_000)
  end

  @doc """
  Scenario 4: race two `GET /4` where one racer uses a 1 second read budget so it
  fails and cancels; the server then completes the other with `"right"`.
  """
  @spec scenario4(String.t()) :: String.t()
  def scenario4(base_url) when is_binary(base_url) do
    Logger.info("Scenario 4", pid: self())
    url = join_url(base_url, "/4")

    race([{url, recv_timeout_ms: 1_000}, url])
  end

  @doc """
  Scenario 5: race two `GET /5` where the first response can be HTTP 500; non-200
  completions are ignored until a 200 with `"right"` or `"left"` arrives.
  """
  @spec scenario5(String.t()) :: String.t()
  def scenario5(base_url) when is_binary(base_url) do
    Logger.info("Scenario 5", pid: self())
    url = join_url(base_url, "/5")
    race([url, url])
  end

  @doc """
  Scenario 6: race three concurrent `GET /6` (first may be 500; non-200 ignored).
  """
  @spec scenario6(String.t()) :: String.t()
  def scenario6(base_url) when is_binary(base_url) do
    Logger.info("Scenario 6", pid: self())
    url = join_url(base_url, "/6")

    race([url, url, url])
  end

  @doc """
  Scenario 7: hedge a request by starting one `GET /7`, waiting at least 3
  seconds, and then starting a second request to the same endpoint.
  """
  @spec scenario7(String.t()) :: String.t()
  def scenario7(base_url) when is_binary(base_url) do
    Logger.info("Scenario 7", pid: self())
    url = join_url(base_url, "/7")

    t1 = Task.async(fn -> http_get_exclusive(url) end)
    Process.sleep(3_000)
    t2 = Task.async(fn -> http_get_exclusive(url) end)

    try do
      await_first_right([t1, t2], System.monotonic_time(:millisecond) + 120_000)
    after
      Task.shutdown(t1, :brutal_kill)
      Task.shutdown(t2, :brutal_kill)
    end
  end

  @doc """
  Scenario 8: race two resource flows. Each flow opens a resource, uses it, and
  closes it even when the use request loses with a non-2xx response.
  """
  @spec scenario8(String.t()) :: String.t()
  def scenario8(base_url) when is_binary(base_url) do
    Logger.info("Scenario 8", pid: self())
    url = join_url(base_url, "/8")

    flow =
      fn ->
        with {:ok, %{status: status, body: id}} when status in 200..299 <-
               http_get_response_exclusive("#{url}?open") do
          try do
            case http_get_response_exclusive("#{url}?use=#{URI.encode_www_form(id)}") do
              {:ok, %{status: use_status, body: body}} when use_status in 200..299 ->
                {:ok, body}

              {:ok, %{}} ->
                {:error, :status}

              {:error, reason} ->
                {:error, reason}
            end
          after
            http_get_response_exclusive("#{url}?close=#{URI.encode_www_form(id)}")
          end
        else
          {:ok, %{}} -> {:error, :status}
          {:error, reason} -> {:error, reason}
        end
      end

    race([flow, flow])
  end

  @doc """
  Scenario 9: start 10 concurrent `GET /9` requests and concatenate the five
  successful response bodies in completion order.
  """
  @spec scenario9(String.t()) :: String.t()
  def scenario9(base_url) when is_binary(base_url) do
    Logger.info("Scenario 9", pid: self())
    url = join_url(base_url, "/9")

    tasks =
      for _ <- 1..10 do
        Task.async(fn -> http_get_response_exclusive(url) end)
      end

    deadline_ms = System.monotonic_time(:millisecond) + 120_000

    collect = fn c, tasks, deadline_ms, bodies, success_count ->
      if success_count == 5 do
        Enum.join(Enum.reverse(bodies))
      else
        now = System.monotonic_time(:millisecond)

        cond do
          now > deadline_ms ->
            "left"

          tasks == [] ->
            Enum.join(Enum.reverse(bodies))

          true ->
            receive do
              {ref, result} ->
                {task, remaining} = pop_task_by_ref(tasks, ref)
                if task, do: Process.demonitor(ref, [:flush])

                case result do
                  {:ok, %{status: status, body: body}} when status in 200..299 ->
                    c.(c, remaining, deadline_ms, [body | bodies], success_count + 1)

                  _ ->
                    c.(c, remaining, deadline_ms, bodies, success_count)
                end

              {:DOWN, ref, :process, _pid, _reason} ->
                {_task, remaining} = pop_task_by_ref(tasks, ref)
                c.(c, remaining, deadline_ms, bodies, success_count)
            after
              min(max(deadline_ms - now, 1), 1_000) ->
                c.(c, tasks, deadline_ms, bodies, success_count)
            end
        end
      end
    end

    try do
      collect.(collect, tasks, deadline_ms, [], 0)
    after
      Enum.each(tasks, &Task.shutdown(&1, :brutal_kill))
    end
  end

  @doc """
  Scenario 10: keep a CPU-heavy task busy while a blocker request is open, report
  process load once per second, and stop the CPU task when the blocker closes.
  """
  @spec scenario10(String.t()) :: String.t()
  def scenario10(base_url) when is_binary(base_url) do
    Logger.info("Scenario 10", pid: self())
    url = join_url(base_url, "/10")
    id = Base.encode16(:crypto.strong_rand_bytes(16), case: :lower)

    runtime_sample = fn ->
      {runtime_ms, _} = :erlang.statistics(:runtime)
      {wall_ms, _} = :erlang.statistics(:wall_clock)
      {runtime_ms, wall_ms}
    end

    current_process_load = fn {previous_runtime_ms, previous_wall_ms} ->
      {runtime_ms, wall_ms} = runtime_sample.()
      runtime_delta = max(runtime_ms - previous_runtime_ms, 0)
      wall_delta = max(wall_ms - previous_wall_ms, 1)
      load = runtime_delta / wall_delta
      {load |> max(0.0) |> min(1.0), {runtime_ms, wall_ms}}
    end

    format_load = fn load -> :erlang.float_to_binary(load, decimals: 3) end

    report_load_until_done =
      fn r, url, id, blocker, cpu_task, previous_sample ->
        Process.sleep(1_000)
        {load, sample} = current_process_load.(previous_sample)

        load =
          case Task.yield(blocker, 0) do
            nil ->
              max(load, 0.95)

            _ ->
              Task.shutdown(cpu_task, :brutal_kill)
              min(load, 0.05)
          end

        case http_get_response_exclusive("#{url}?#{id}=#{format_load.(load)}",
               recv_timeout_ms: 10_000
             ) do
          {:ok, %{status: status, body: body}} when status in 200..299 ->
            body

          {:ok, %{status: status}} when status in 300..399 ->
            r.(r, url, id, blocker, cpu_task, sample)

          {:ok, %{body: body}} ->
            "left: #{body}"

          {:error, reason} ->
            "left: #{inspect(reason)}"
        end
      end

    busy_loop = fn bl, bytes -> bl.(bl, :crypto.hash(:sha512, bytes)) end

    cpu_task =
      Task.async(fn -> busy_loop.(busy_loop, :crypto.strong_rand_bytes(512)) end)

    blocker =
      Task.async(fn -> http_get_response_exclusive("#{url}?#{id}", recv_timeout_ms: 30_000) end)

    try do
      report_load_until_done.(report_load_until_done, url, id, blocker, cpu_task, runtime_sample.())
    after
      Task.shutdown(cpu_task, :brutal_kill)
      Task.shutdown(blocker, :brutal_kill)
    end
  end

  @doc """
  Scenario 11: race one request against a nested race of two requests, treating
  the nested all-failures case as a loser instead of crashing the outer race.
  """
  @spec scenario11(String.t()) :: String.t()
  def scenario11(base_url) when is_binary(base_url) do
    Logger.info("Scenario 11", pid: self())
    url = join_url(base_url, "/11")

    nested =
      Task.async(fn ->
        case race([url, url]) do
          "right" -> {:ok, "right"}
          _ -> {:error, :nested_lost}
        end
      end)

    single = Task.async(fn -> http_get_exclusive(url) end)

    try do
      await_first_right([nested, single], System.monotonic_time(:millisecond) + 120_000)
    after
      Task.shutdown(nested, :brutal_kill)
      Task.shutdown(single, :brutal_kill)
    end
  end
end
