defmodule EasyRacer.LogFormat do
  @moduledoc false

  # Mirrors java-cf test layout (logback-test.xml):
  # %d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n

  @spec format(Logger.level(), term(), Logger.timestamp(), Logger.metadata()) :: IO.chardata()
  def format(level, message, {{_, _, _}, {h, mi, s, sub}}, metadata) do
    [
      :io_lib.format("~2..0w:~2..0w:~2..0w.~3..0w", [h, mi, s, millisecond(sub)]),
      " [",
      pid_to_string(metadata[:pid]),
      "] ",
      format_level(level),
      ?\s,
      logger_name(metadata[:mfa]),
      " - ",
      message_to_string(message),
      ?\n
    ]
  rescue
    _ ->
      ["log-format-error: ", inspect(message), ?\n]
  end

  defp millisecond(sub) when is_integer(sub) and sub in 0..999, do: sub
  defp millisecond(sub) when is_integer(sub), do: div(sub, 1000) |> rem(1000)

  defp millisecond({microsecond, _precision}) when is_integer(microsecond),
    do: millisecond(microsecond)

  defp millisecond(_), do: 0

  defp format_level(level) do
    level
    |> to_string()
    |> String.upcase()
    |> String.pad_trailing(5)
  end

  defp pid_to_string(nil), do: "???"
  defp pid_to_string(pid) when is_pid(pid), do: pid |> :erlang.pid_to_list() |> List.to_string()

  defp logger_name(nil), do: "?"

  defp logger_name({module, function, arity}),
    do: module |> Exception.format_mfa(function, arity) |> String.slice(0, 36)

  defp message_to_string(message) when is_function(message, 0),
    do: message.() |> message_to_string()

  defp message_to_string(message) when is_binary(message), do: message
  defp message_to_string(message), do: IO.chardata_to_string(message)
end
