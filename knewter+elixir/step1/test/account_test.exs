defmodule Account do
  def start do
    await([])
  end

  def await(events) do
    receive do
      {:check_balance, pid} -> divulge_balance(pid, events)
      await(events)
    end
  end

  defp divulge_balance(pid, events) do
    pid <- calculate_balance(events)
  end

  defp calculate_balance(events) do
    {:balance, 0}
  end
end

defmodule AccountTest do
  use ExUnit.Case

  test "starts off with a balance of 0" do
    account = spawn_link(Account, :start, [])
    verify_balance_is 0, account
  end

  def verify_balance_is(expected_balance, account) do
    account <- {:check_balance, self()}
    assert_receive({:balance, expected_balance})
  end
end
