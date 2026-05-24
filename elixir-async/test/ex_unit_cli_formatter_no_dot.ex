# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

defmodule EasyRacer.ExUnitCLIFormatterNoDot do
  @moduledoc false
  use GenServer

  def init(opts), do: ExUnit.CLIFormatter.init(opts)

  def handle_cast({:test_finished, %ExUnit.Test{state: nil}} = event, %{trace: false} = config) do
    silence_formatter_io(fn -> ExUnit.CLIFormatter.handle_cast(event, config) end)
  end

  def handle_cast(event, config), do: ExUnit.CLIFormatter.handle_cast(event, config)

  defp silence_formatter_io(fun) do
    {:ok, io} = StringIO.open("")
    original_group_leader = Process.group_leader()

    try do
      Process.group_leader(self(), io)
      fun.()
    after
      Process.group_leader(self(), original_group_leader)
    end
  end
end
