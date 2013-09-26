defmodule Atm do
  use Application.Behaviour

  def start(_type, state) do
    Atm.Supervisor.start_link(state)
  end
end