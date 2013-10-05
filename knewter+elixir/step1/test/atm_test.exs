defmodule AtmTest do
  use ExUnit.Case

  test "responds properly when started" do
    Atm.stop()
    assert Atm.start() == :started
  end

  test "responds with :no_such_account if balance checked on nonexistent account" do
    assert Atm.check_balance('1234') == :no_such_account
  end

  test "registers its process when started" do
    assert Process.whereis(:atm) != nil
  end

  test "allows depositing into an account number" do
    assert Atm.deposit("12345", 10) == {:new_balance, 10}
  end

  test "allows checking balance" do
    Atm.deposit("123", 10)
    assert Atm.check_balance("123") == {:balance, 10}
  end

  setup do
    Atm.start()
    :ok
  end

  teardown do
    Atm.stop()
    :ok
  end
end
