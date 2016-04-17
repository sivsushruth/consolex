defmodule Mix.Tasks.Consolex do
  use Mix.Task

  def run(_) do
    Application.ensure_all_started(:consolex)
    :timer.sleep(:infinity)
  end
end