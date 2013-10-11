defmodule AtmTest do
  use ExUnit.Case

  test "responds properly when started" do
    assert Atm.start() == :started
  end

  test "responds properly when stopped" do
    assert Atm.stop() == :atm_closed
    assert Atm.start()
    assert Atm.stop() == :stopped
  end

  test "responds with :no_such_account if balance checked on nonexistent account" do
    Atm.start()
    assert Atm.check_balance('1234') == :no_such_account
  end

  test "registers its process when started" do
    Atm.start()
    assert Process.whereis(:atm) != nil
  end

  test "depositing into an account number" do
    Atm.start()
    assert Atm.deposit("12345", 10) == {:new_balance, 10}
  end

  test "checking balance" do
    Atm.start()
    Atm.deposit("123", 10)
    assert Atm.check_balance("123") == {:balance, 10}
  end

  test "withdrawing" do
    Atm.start()
    Atm.deposit("12", 100)
    assert Atm.withdraw("12", 50) == {:new_balance, 50}
  end

  test "withdrawing more than the account contains" do
    Atm.start()
    Atm.deposit("10", 49)
    assert Atm.withdraw("10", 50) == :overdrawn
  end

  test "withdrawing from nonexistent account" do
    Atm.start()
    assert Atm.withdraw("11", 50) == :no_such_account
  end

  test "withdrawing when closed" do
    assert Atm.withdraw("9", 50) == :atm_closed
  end

  setup do
    :ok
  end

  teardown do
    Atm.stop()
    :ok
  end
end
