defmodule EasyRacer do
  require Logger

  @moduledoc """
  Easy Racer HTTP client. This module implements the obstacle scenarios from the
  [Easy Racer](https://github.com/jamesward/easyracer) suite.

  Scenarios use `Task.async/1` so each racer has its own TCP connection (`:gen_tcp`).
  The server often blocks the first connection until a second (or third) arrives;
  sharing one HTTP/1 connection would deadlock.

  Races await the first successful HTTP 200 body of `"right"` or `"left"`, ignoring
  connection errors, timeouts, and non-200 responses while any racer is still in flight.

  ## Logging

  Each scenario logs at **info** like `java-cf` `Scenarios` (`logger.info("Scenario N")`).
  The console format is configured in `config/config.exs` to mirror Logback's
  `%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg` (see `EasyRacer.LogFormat`).
  """

  @doc """
  Scenario 1: race two concurrent requests to `GET /1`.

  Returns the response body (`"right"` or `"left"`). On failure, returns `"left"`.
  """
  @spec scenario1(String.t()) :: String.t()
  def scenario1(base_url) when is_binary(base_url) do
    Logger.info("Scenario 1", pid: self())
    url = join_url(base_url, "/1")
    race_two(url, [])
  end

  @doc """
  Scenario 2: race two concurrent `GET /2` where the server interrupts one connection
  once both are open. Same race semantics as scenario 1.
  """
  @spec scenario2(String.t()) :: String.t()
  def scenario2(base_url) when is_binary(base_url) do
    Logger.info("Scenario 2", pid: self())
    url = join_url(base_url, "/2")
    race_two(url, [])
  end

  @scenario_3_racers 10_000

  @doc """
  Scenario 3: race #{@scenario_3_racers} concurrent `GET /3`. The server returns
  `"right"` only after all connections are open; each racer uses its own socket.
  """
  @spec scenario3(String.t()) :: String.t()
  def scenario3(base_url) when is_binary(base_url) do
    Logger.info("Scenario 3", pid: self())
    url = join_url(base_url, "/3")

    tasks =
      for _ <- 1..@scenario_3_racers do
        Task.async(fn -> http_get_exclusive(url) end)
      end

    deadline_ms = System.monotonic_time(:millisecond) + 600_000

    try do
      await_first_right(tasks, deadline_ms)
    after
      Enum.each(tasks, &Task.shutdown(&1, :brutal_kill))
    end
  end

  @doc """
  Scenario 4: race two `GET /4` where one racer uses a 1 second read budget so it
  fails and cancels; the server then completes the other with `"right"`.
  """
  @spec scenario4(String.t()) :: String.t()
  def scenario4(base_url) when is_binary(base_url) do
    Logger.info("Scenario 4", pid: self())
    url = join_url(base_url, "/4")

    t1 = Task.async(fn -> http_get_exclusive(url, recv_timeout_ms: 1_000) end)
    t2 = Task.async(fn -> http_get_exclusive(url) end)

    try do
      await_first_right([t1, t2], System.monotonic_time(:millisecond) + 120_000)
    after
      Task.shutdown(t1, :brutal_kill)
      Task.shutdown(t2, :brutal_kill)
    end
  end

  @doc """
  Scenario 5: race two `GET /5` where the first response can be HTTP 500; non-200
  completions are ignored until a 200 with `"right"` or `"left"` arrives.
  """
  @spec scenario5(String.t()) :: String.t()
  def scenario5(base_url) when is_binary(base_url) do
    Logger.info("Scenario 5", pid: self())
    url = join_url(base_url, "/5")
    race_two(url, [])
  end

  @doc """
  Scenario 6: race three concurrent `GET /6` (first may be 500; non-200 ignored).
  """
  @spec scenario6(String.t()) :: String.t()
  def scenario6(base_url) when is_binary(base_url) do
    Logger.info("Scenario 6", pid: self())
    url = join_url(base_url, "/6")

    t1 = Task.async(fn -> http_get_exclusive(url) end)
    t2 = Task.async(fn -> http_get_exclusive(url) end)
    t3 = Task.async(fn -> http_get_exclusive(url) end)

    try do
      await_first_right([t1, t2, t3], System.monotonic_time(:millisecond) + 120_000)
    after
      Task.shutdown(t1, :brutal_kill)
      Task.shutdown(t2, :brutal_kill)
      Task.shutdown(t3, :brutal_kill)
    end
  end

  defp race_two(url, opts) do
    t1 = Task.async(fn -> http_get_exclusive(url, opts) end)
    t2 = Task.async(fn -> http_get_exclusive(url, opts) end)

    try do
      await_first_right([t1, t2], System.monotonic_time(:millisecond) + 120_000)
    after
      Task.shutdown(t1, :brutal_kill)
      Task.shutdown(t2, :brutal_kill)
    end
  end

  defp await_first_right(tasks, deadline_ms) do
    if System.monotonic_time(:millisecond) > deadline_ms do
      "left"
    else
      pairs = Task.yield_many(tasks, 250)

      case Enum.find_value(pairs, fn {_task, out} ->
             case out do
               {:ok, {:ok, text}} when text in ["right", "left"] -> text
               {:ok, {:error, _}} -> nil
               {:ok, nil} -> nil
               {:exit, _} -> nil
               _ -> nil
             end
           end) do
        text when is_binary(text) ->
          text

        nil ->
          still =
            pairs
            |> Enum.filter(fn {_task, out} -> out == nil end)
            |> Enum.map(&elem(&1, 0))

          if still == [] do
            "left"
          else
            await_first_right(still, deadline_ms)
          end
      end
    end
  end

  # One socket per request: required for scenario 1 (first response waits for the
  # second connection at the server).
  defp http_get_exclusive(url, opts \\ []) when is_binary(url) and is_list(opts) do
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
               {:ok, body} <- recv_response(sock, <<>>, recv_deadline_ms) do
            {:ok, String.trim(body)}
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

  defp recv_response(sock, acc, recv_deadline_ms) do
    now = System.monotonic_time(:millisecond)

    if now >= recv_deadline_ms do
      {:error, :timeout}
    else
      chunk_timeout = min(max(recv_deadline_ms - now, 1), 60_000)

      case :gen_tcp.recv(sock, 0, chunk_timeout) do
        {:ok, data} when data != <<>> ->
          next = acc <> data

          case parse_http_ok_body(next) do
            {:ok, body} -> {:ok, body}
            :more -> recv_response(sock, next, recv_deadline_ms)
            {:error, _} = e -> e
          end

        {:ok, <<>>} ->
          parse_http_ok_body(acc)

        {:error, :closed} ->
          parse_http_ok_body(acc)

        {:error, _} = e ->
          e
      end
    end
  end

  defp default_port("https"), do: 443
  defp default_port(_), do: 80

  defp query_string(nil), do: ""
  defp query_string(q), do: "?" <> q

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

  defp parse_http_ok_body(data) do
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

  defp parse_status_line(status, header_lines, body) do
    case String.to_integer(status) do
      code when code in 200..299 ->
        parse_body(header_lines, body)

      _ ->
        {:error, :status}
    end
  end

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

  defp join_url(base, path) do
    base = String.trim_trailing(base, "/")
    path = if String.starts_with?(path, "/"), do: path, else: "/" <> path
    base <> path
  end
end
