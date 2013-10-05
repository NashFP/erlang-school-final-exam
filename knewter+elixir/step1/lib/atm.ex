defrecord AtmState, accounts: [], state: :open

defmodule Atm do
  def start do
    started = spawn_link(Atm, :await, [])
    Process.register(started, :atm)
    :started
  end

  def stop do
    Process.unregister(:atm)
  end

  def deposit(account_number, amount) do
    :atm <- {:deposit, account_number, amount, self()}
    receive do
      {:new_balance, account_number, amount} -> {:new_balance, amount}
      :no_such_account -> :no_such_account
    end
  end

  def check_balance(account_number) do
    :atm <- {:balance, account_number, self()}
    receive do
      {:balance, account_number, amount} -> {:balance, amount}
      :no_such_account -> :no_such_account
    end
  end

  def await() do
    await(AtmState.new)
  end
  def await(state) do
    receive do
      {:deposit, account_number, amount, requestor} ->
        state = handle_deposit_into(account_number, amount, state)
        case get_balance(account_number) do
          :no_such_account -> requestor <- :no_such_account
          new_balance -> requestor <- {:new_balance, account_number, new_balance}
        end
      {:balance, account_number, requestor} ->
        case get_balance(account_number) do
          :no_such_account -> requestor <- :no_such_account
          balance -> requestor <- {:balance, account_number, balance}
        end
    end
    await(state)
  end

  def get_balance(:no_such_account) do
    :no_such_account
  end
  def get_balance(account_number) when is_pid(account_number) do
    account_number <- {:check_balance, self()}
    receive do
      {:balance, amount} -> amount
    after
      10 -> :timeout
    end
  end
  def get_balance(account_number) do
    account = get_account(account_number)
    get_balance(account)
  end

  def handle_deposit_into(account_number, amount, state) do
    account = find_or_create_account(account_number)
    account <- {:deposit, amount}
  end

  def find_or_create_account(account_number) do
    case get_account(account_number) do
      :no_such_account ->
        register_account(account_number)
        get_account(account_number)
      account -> account
    end
  end

  def get_account(account_number) do
    try do
      :gproc.get_value({:n, :l, account_number})
    rescue
      [ArgumentError] -> :no_such_account
    end
  end

  def register_account(account_number) do
    account = spawn_link(Account, :start, [])
    :gproc.reg({:n, :l, account_number}, account)
  end
end
