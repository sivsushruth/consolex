alias Porcelain.Process, as: Proc
defmodule Consolex.Shell do

  def start_port(task, websocket_handler) do
    proc = %Proc{pid: _pid} = Porcelain.spawn_shell("#{task}", async_in: true, out: {:send, self()})
    loop(proc, websocket_handler)
  end

  def loop(proc, websocket_handler) do
    receive do
      {:send, input, options} ->
        execute_inputs(proc, input, options)
      {:terminate} ->
        exit(:shutdown)
      {_pid, :data, :out, output} ->
         case should_reply?(output) do
           true -> Consolex.reply(websocket_handler, "#{output}")
           false -> :ok
         end
      data->
        Consolex.reply(websocket_handler, "#{inspect data}")
    end
    loop(proc, websocket_handler)
  end

  defp should_reply?(output) do
    !String.match?(output, ~r/(\W){3,}[\(](\d)*[\)]>/)
  end

  defp execute_inputs(proc, input, options) do
    command = Regex.replace(~r/(\n)*/, input, "\\g{1}" )
    case Map.get(options, "inputType") do
      "multiple" ->
        send_input(command, proc)
      _ ->
        single_command = Regex.replace(~r/\n/, command, ";" )
        Regex.replace(~r/(;)*/, single_command, "\\g{1}" )
        |> String.replace(";|>", "|>")
        |> send_input(proc)
    end
  end

  defp send_input(command, proc) do
    command
    |> String.split("\n")
    |> Enum.filter(&(String.strip(&1) != ""))
    |> Enum.each(fn expr -> Proc.send_input(proc, "#{expr}\n");:timer.sleep(50) end)
  end


end