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

  test "has balance decremented by the amount of a withdrawal" do
    account = spawn_link(Account, :start, [])
    account <- {:deposit, 20}
    account <- {:withdraw, 10}
    verify_balance_is 10, account
  end

  def verify_balance_is(expected_balance, account) do
    account <- {:check_balance, self()}
    assert_receive {:balance, ^expected_balance}
  end
end
