defmodule Consolex.Server do
  use Application

  def start(_type, _args) do
    dispatch = :cowboy_router.compile([
      {:_, [
        {'/websocket', Consolex.WebSocketHandler, []},
        {"/", :cowboy_static, {:priv_file, :consolex, "index.html"}},
        {"/css/[...]", :cowboy_static, {:priv_dir, :consolex, "css"}},
        {"/js/[...]", :cowboy_static, {:priv_dir, :consolex, "js"}}
        ]}
      ])
    IO.puts "Started listening on port 5984..."
    :cowboy.start_http :my_http_listener, 100, [{:port, 5984}], [{:env, [{:dispatch, dispatch}]}]
    Consolex.WebSocketHandlerSup.start_link([])
  end

  def stop(_state) do
    :ok
  end
end