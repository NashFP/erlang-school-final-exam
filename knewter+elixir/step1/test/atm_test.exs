defmodule AtmTest do
  use ExUnit.Case

  test "responds properly when started" do
    assert Atm.start() == :started
  end

  test "responds with :no_such_account if balance checked on nonexistent account" do
    assert Atm.check_balance('1234') == :no_such_account
  end

  test "registers its process when started" do
    Atm.start()
    assert Process.whereis(:atm) != nil
  end
end
