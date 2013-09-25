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
      {:withdraw, amount} ->
        events = withdraw(amount, events)
        await(events)
    end
  end

  # Adds a deposit for the appropriate amount at the end of the events, and returns them
  defp deposit(amount, events) do
    events ++ [{:deposit, amount}]
  end

  # Adds a withdrawal for the appropriate amount at the end of the events, and returns them
  defp withdraw(amount, events) do
    events ++ [{:withdrawal, amount}]
  end

  defp divulge_balance(pid, events) do
    pid <- calculate_balance(events)
  end

  defp calculate_balance(events) do
    deposits = sum(just_deposits(events))
    withdrawals = sum(just_withdrawals(events))
    {:balance, deposits - withdrawals}
  end

  defp sum(events) do
    Enum.reduce(events, 0, fn({_, amount}, acc) -> acc + amount end)
  end

  # Just returns events that are deposits
  defp just_deposits(events) do
    just_type(events, :deposit)
  end

  # Just returns events that are withdrawals
  defp just_withdrawals(events) do
    just_type(events, :withdrawal)
  end

  # Just returns events that are of type expected_type
  defp just_type(events, expected_type) do
    Enum.filter(events, fn({type, _}) -> type == expected_type end)
  end
end
