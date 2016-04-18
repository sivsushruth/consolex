defmodule Consolex.Shell do
  
  def start_port(task, websocket_handler) do
    port = Port.open({:spawn, "#{task}"}, [:stream, :exit_status])
    loop(port, websocket_handler)
  end

  def loop(port, websocket_handler) do
    receive do
      {port, {:data, output}} ->
        Consolex.reply(websocket_handler, "#{output}")
      {:send, input} ->
        execute_inputs(input, port)
      {:terminate} -> 
        exit(:shutdown)
      data->
        Consolex.reply(websocket_handler, "#{inspect data}")
    end 
    loop(port, websocket_handler)
  end

  defp execute_inputs(input, port) do
    command_stripped_lines = Regex.replace(~r/(\n)*/, input, "\\g{1}" )
    command = Regex.replace(~r/[^,.]\n/, command_stripped_lines, ";" )
    |> String.replace(";|>", "|>")
    Port.command(port, "#{command} \r\n")
  end

end