defmodule Consolex do
  use GenServer

  def start_link(init_value \\ 0, opts \\ []) do
    GenServer.start_link(Consolex, init_value, opts)
  end

  def start_shell(shell, task) do
    GenServer.cast(shell, {:start_shell, task, shell})
  end

  def execute(shell, command) do
    GenServer.cast(shell, {:execute, command})
  end

  def reply(shell, msg) do
    GenServer.cast(shell, {:reply, msg})
  end

  def init(pid) do
    {:ok, {:pid, pid}}
  end

  def handle_call(y, x, state) do
    {:reply, state, state}
  end

  def handle_cast({:start_shell, task, shell}, {:pid, pid}) do
    port = Kernel.spawn_link(fn -> Consolex.Shell.start_port(task, shell) end) 
    {:noreply, [{:port, port}, {:pid, pid}]}
  end

  def handle_cast({:reply, msg}, [{:port, port}, {:pid, pid}]) do
    Consolex.WebSocketHandler.reply(pid, msg)
    {:noreply, [{:port, port}, {:pid, pid}]}
  end

  def handle_cast({:execute, command}, [{:port, port}, {:pid, pid}]) do
    send(port, {:send, command})
    {:noreply, [{:port, port}, {:pid, pid}]}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

end
