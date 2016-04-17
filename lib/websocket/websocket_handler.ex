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

  def websocket_handle({:text, content}, req, {:pid, pid}) do
    case JSX.is_json?(content) do
      true -> 
        {:ok, content_map} = JSX.decode(content)
        case Map.fetch(content_map, "task") do
          {:ok, task} -> 
            {:ok, shell} = Consolex.start_link(pid, [])
            Consolex.start_shell(shell, task)
            {:reply, {:text, "Started shell..."}, req, [{:shell, shell}, {:pid, pid}]}
           _ ->
            {:reply, {:text, "Could not start shell"}, req, {:pid, pid}}
        end
      false -> 
        {:reply, {:text, "Could not start shell"}, req, {:pid, pid}}
    end
  end
  
  def websocket_handle({:text, content}, req, [{:shell, shell}, {:pid, pid}]) do
    is_json = JSX.is_json?(content)
    content_map = case is_json do
      true -> 
        {:ok, content_map_decoded} = JSX.decode(content)
        content_map_decoded
      false -> 
        nil
    end
    case is_json do
      true when is_map(content_map) -> 
        case Map.fetch(content_map, "task") do
          {:ok, "terminate"} -> 
            Consolex.terminate(shell)    
            {:reply, {:text, "Terminated shell"}, req, {:pid, pid}}
           _ ->
            {:reply, {:text, "Could not understand task"}, req, [{:shell, shell}, {:pid, pid}]}
        end
      _ -> 
        Consolex.execute(shell, content)
        {:ok, req, [{:shell, shell}, {:pid, pid}]}
    end    
  end
  
  def websocket_handle({:text, _content}, req, state) do    
    {:ok, req, state}
  end
  
  def websocket_info({:reply, msg}, req, shell) do
    {:reply, {:text, msg}, req, shell}
  end

  def websocket_info(_info, req, state) do
    {:ok, req, state}
  end

end