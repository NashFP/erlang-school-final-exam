defmodule AccountTest do
  use ExUnit.Case

  test "starts off with a balance of 0" do
    account = spawn_link(Account, :start, [])
    verify_balance_is 0, account
  end

  test "has balance incremented by the amount of a deposit" do
    account = spawn_link(Account, :start, [])
    account <- {:deposit, 10}
    verify_balance_is 10, account
  end

  # NOTE: It would be better to use assert_receive in this, but https://github.com/elixir-lang/elixir/issues/1724
  def verify_balance_is(expected_balance, account) do
    account <- {:check_balance, self()}
    receive do
      {:balance, balance} -> assert(balance == expected_balance)
    end
  end
end
