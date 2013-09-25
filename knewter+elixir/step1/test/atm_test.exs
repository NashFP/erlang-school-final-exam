defmodule Atm do
  def start do
    :started
  end
end

defmodule AtmTest do
  use ExUnit.Case

  test "responds properly when started" do
    assert Atm.start() == :started
  end
end
