defmodule Consolex.WebSocketHandler do
  @behaviour :cowboy_websocket_handler

  def init({_tcp, _http}, _req, _opts) do
    {:upgrade, :protocol, :cowboy_websocket}
  end

  def reply(server, msg) do
    send(server, {:reply, msg})
  end

  def websocket_init(_TransportName, req, _opts) do
    {:ok, req, {:pid, self()}}
  end

  def websocket_terminate(_reason, _req, _state) do
    :ok
  end

  def websocket_handle({:text, raw_content}, req, {:pid, pid}) do
    case JSX.is_json?(raw_content) do
      true ->
        start_shell(raw_content, req, pid)
      false ->
        {:reply, {:text, "Could not start shell"}, req, {:pid, pid}}
    end
  end

  def websocket_handle({:text, raw_content}, req, [{:shell, shell}, {:pid, pid}]) do
    is_json = JSX.is_json?(raw_content)
    case is_json do
      true ->
        raw_content |> JSX.decode! |> shell_task(req, shell, pid)
      _ ->
        # Consolex.execute(shell, raw_content)
        # {:ok, req, [{:shell, shell}, {:pid, pid}]}
        {:reply, {:text, "Could not understand task"}, req, {:pid, pid}}
    end
  end

  def websocket_handle({:text, _raw_content}, req, state) do
    {:ok, req, state}
  end

  def websocket_info({:reply, msg}, req, shell) do
    {:reply, {:text, msg}, req, shell}
  end

  def websocket_info(_info, req, state) do
    {:ok, req, state}
  end

  defp start_shell(content, req, pid) do
    content = JSX.decode!(content)
    case Map.fetch(content, "task") do
      {:ok, task} ->
        {:ok, shell} = Consolex.start_link(pid, [])
        Consolex.start_shell(shell, task)
        {:reply, {:text, "Started shell..."}, req, [{:shell, shell}, {:pid, pid}]}
       _ ->
        {:reply, {:text, "Could not start shell"}, req, {:pid, pid}}
    end
  end

  defp shell_task(content, req, shell, pid) do
    code = Map.get(content, "code")
    options = Map.get(content, "options")
    case Map.get(content, "task") do
      "terminate" ->
        Consolex.terminate(shell)
        {:reply, {:text, "Terminated shell"}, req, {:pid, pid}}
      "execute" ->
        Consolex.execute(shell, code, options)
        {:ok, req, [{:shell, shell}, {:pid, pid}]}
      _ ->
        {:reply, {:text, "Could not understand task"}, req, [{:shell, shell}, {:pid, pid}]}
    end
  end

end