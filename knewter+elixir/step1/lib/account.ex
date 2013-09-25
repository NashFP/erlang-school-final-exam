defmodule Account do
  def start do
    await([])
  end

  def await(events) do
    receive do
      {:check_balance, pid} ->
        divulge_balance(pid, events)
        await(events)
      {:deposit, amount} ->
        events = deposit(amount, events)
        await(events)
    end
  end

  # Adds a deposit for the appropriate amount at the end of the events, and returns them
  defp deposit(amount, events) do
    events ++ [{:deposit, amount}]
  end

  defp divulge_balance(pid, events) do
    pid <- calculate_balance(events)
  end

  defp calculate_balance(events) do
    balance = Enum.reduce(events, 0, fn({:deposit, amount}, acc) -> acc + amount end)
    {:balance, balance}
  end
end
