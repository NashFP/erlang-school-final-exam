defmodule Atm.Server do
	use GenServer.Behaviour

	def start_link(accounts) do
	    :gen_server.start_link({ :local, :atm }, __MODULE__, accounts, [])
	  end

	def init(accounts) do
		{:ok, accounts}
	end

	def handle_call({:check_balance,account_number}, _from, accounts) do
		response = case HashDict.fetch(accounts,account_number) do
			{:ok, balance} -> {:balance, balance}
			:error -> :no_such_account
		end
		{:reply, response, accounts}
	end

	def handle_call({:withdraw,account_number,amount}, _from, accounts) do
		response = case HashDict.fetch(accounts, account_number) do
			{:ok, balance} when balance-amount < 0 -> :insufficient_funds
			{:ok, balance} -> 
				new_balance = balance - amount
				accounts = HashDict.put(accounts,account_number,new_balance)
				{:new_balance, new_balance}
			:error -> :no_such_account
		end
		{:reply, response, accounts}
	end

	def handle_call({:deposit,account_number,amount}, _from, accounts) do
		balance = HashDict.get(accounts,account_number,0)
		new_balance = balance + amount
		{:reply, {:new_balance,new_balance}, HashDict.put(accounts,account_number,new_balance)}
	end
end