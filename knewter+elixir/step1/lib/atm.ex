defmodule Atm do
  def start do
    started = spawn_link(Atm, :await, [])
    Process.register(started, :atm)
    :started
  end

  def await() do
  end

  def check_balance(_) do
    :no_such_account
  end
end
