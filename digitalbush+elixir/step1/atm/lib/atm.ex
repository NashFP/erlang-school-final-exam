defmodule Atm do
  use Application.Behaviour

  def start(_type, state) do
    Atm.Supervisor.start_link(state)
  end

  def check_balance(account_number) do
  	call({:check_balance,account_number})
  end

  def withdraw(account_number, amount) do
  	call({:withdraw,account_number,amount})
  end

  def deposit(account_number, amount) do
  	call({:deposit,account_number,amount})
  end

  defp call(request) do
  	:gen_server.call(:atm,request);
  end
end