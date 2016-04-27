defmodule Consolex.Shell do

  def start_port(task, websocket_handler) do
    port = Port.open({:spawn, "#{task}"}, [:stream, :exit_status])
    loop(port, websocket_handler)
  end

  def loop(port, websocket_handler) do
    receive do
      {:send, input, options} ->
        execute_inputs(port, input, options)
      {:terminate} ->
        exit(:shutdown)
      {port, {:data, output}} ->
         case should_reply?(output) do
           true -> Consolex.reply(websocket_handler, "#{output}")
           false -> :ok
         end
      data->
        Consolex.reply(websocket_handler, "#{inspect data}")
    end
    loop(port, websocket_handler)
  end

  defp should_reply?(output) do
    IO.inspect output
    !String.match?(to_string(output), ~r/(\W){3,}[\(](\d)*[\)]>/)
  end

  defp execute_inputs(port, input, options) do
    command = Regex.replace(~r/(\n)*/, input, "\\g{1}" )
    case Map.get(options, "inputType") do
      "multiple" ->
        send_input(command, port)
      _ ->
        single_command = Regex.replace(~r/\n/, command, ";" )
        Regex.replace(~r/(;)*/, single_command, "\\g{1}" )
        |> String.replace(";|>", "|>")
        |> send_input(port)
    end
  end

  defp send_input(command, port) do
    command
    |> String.split("\n")
    |> Enum.filter(&(String.strip(&1) != ""))
    |> Enum.each(fn expr ->  IO.inspect expr;Port.command(port, "#{command}\r\n");:timer.sleep(100) end)
  end


end