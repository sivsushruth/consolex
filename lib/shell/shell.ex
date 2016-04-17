defmodule Consolex.Shell do
  
  def start_port(task, websocket_handler) do
    port = Port.open({:spawn, "#{task}"}, [:stream, :exit_status])
    loop(port, websocket_handler)
  end

  def loop(port, websocket_handler) do
    receive do
      {port, {:data, output}} -> 
        IO.puts output
        Consolex.reply(websocket_handler, "#{output}")
      {:send, input} ->
        IO.inspect input
        execute_inputs(input, port)
      data ->
        Consolex.reply(websocket_handler, "#{inspect data}")
    end 
    loop(port, websocket_handler)
  end

  defp execute_inputs(input, port) do
    command = input
    |> String.replace("\n", ";")
    |> String.replace(";|>", "|>")
    Port.command(port, "#{command} \r\n")
  end

end