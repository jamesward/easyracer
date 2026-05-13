defmodule EasyRacer.LogFormat do
  @moduledoc false

  # Mirrors java-cf test layout (logback-test.xml):
  # %d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n

  @spec format(Logger.level(), term(), Logger.timestamp(), Logger.metadata()) :: IO.chardata()
  def format(level, message, {{_, _, _}, {h, mi, s, sub}}, metadata) do
    # Elixir/OTP may pass subsecond as integer (µs or ms) or as `{microsecond, precision}`.
    ms = frac_ms(sub)
    time = :io_lib.format("~2..0w:~2..0w:~2..0w.~3..0w", [h, mi, s, ms])
    thread = metadata[:pid] |> pid_to_string()
    lvl = level |> to_string() |> String.upcase() |> String.pad_trailing(5)
    logger_name = metadata[:mfa] |> logger_name_36()
    msg = chardata_to_string(message)

    [time, ?\s, ?[, thread, ?], ?\s, lvl, ?\s, logger_name, " - ", msg, ?\n]
  rescue
    _ ->
      ["log-format-error: ", inspect(message), ?\n]
  end

  defp frac_ms(sub) when is_integer(sub) and sub >= 0 and sub <= 999, do: sub
  defp frac_ms(sub) when is_integer(sub), do: sub |> div(1000) |> rem(1000)
  defp frac_ms({micro, _}) when is_integer(micro), do: micro |> div(1000) |> rem(1000)
  defp frac_ms(_), do: 0

  defp pid_to_string(nil), do: "???"
  defp pid_to_string(pid) when is_pid(pid), do: :erlang.pid_to_list(pid) |> List.to_string()

  defp logger_name_36(nil), do: "?"
  defp logger_name_36({m, f, a}) do
    raw = Exception.format_mfa(m, f, a)
    String.slice(raw, 0, 36)
  end

  defp chardata_to_string(msg) when is_function(msg, 0), do: msg.() |> chardata_to_string()
  defp chardata_to_string(msg) when is_binary(msg), do: msg
  defp chardata_to_string(msg), do: IO.iodata_to_binary(msg)
end
