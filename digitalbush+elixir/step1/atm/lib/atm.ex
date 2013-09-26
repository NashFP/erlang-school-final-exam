defmodule Atm do
  use Application.Behaviour

  def start(_type, state) do
    Atm.Supervisor.start_link(state)
  end

  def check_balance(account_number) do
  	:gen_server.call(:atm,{:check_balance,account_number})
  end

  def withdraw(account_number, amount) do
  	:gen_server.call(:atm,{:withdraw,account_number,amount})
  end

  def deposit(account_number, amount) do
  	:gen_server.call(:atm,{:deposit,account_number,amount})
  end
end