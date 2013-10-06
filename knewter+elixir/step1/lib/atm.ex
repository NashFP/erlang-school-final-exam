defrecord AtmState, accounts: [], state: :open

defmodule Atm do
  def start do
    started = spawn_link(Atm, :await, [])
    Process.register(started, :atm)
    :started
  end

  def stop do
    case is_closed? do
      true -> :atm_closed
      _else ->
        Process.unregister(:atm)
        :stopped
    end
  end

  def deposit(account_number, amount) do
    if is_closed? do
      :atm_closed
    else
      :atm <- {:deposit, account_number, amount, self()}
      receive do
        {:new_balance, account_number, amount} -> {:new_balance, amount}
        :no_such_account -> :no_such_account
      end
    end
  end

  def withdraw(account_number, amount) do
    if is_closed? do
      :atm_closed
    else
      :atm <- {:withdraw, account_number, amount, self()}
      receive do
        {:new_balance, account_number, amount} -> {:new_balance, amount}
        :overdrawn -> :overdrawn
        :no_such_account -> :no_such_account
      end
    end
  end

  def check_balance(account_number) do
    if is_closed? do
      :atm_closed
    else
      :atm <- {:balance, account_number, self()}
      receive do
        {:balance, account_number, amount} -> {:balance, amount}
        :no_such_account -> :no_such_account
      end
    end
  end

  def is_closed? do
    case Process.whereis(:atm) do
      nil -> true
      _else -> false
    end
  end

  def await() do
    await(AtmState.new)
  end
  def await(state) do
    receive do
      {:deposit, account_number, amount, requestor} ->
        handle_deposit_into(account_number, amount)
        case get_balance(account_number) do
          :no_such_account -> requestor <- :no_such_account
          new_balance -> requestor <- {:new_balance, account_number, new_balance}
        end
      {:withdraw, account_number, amount, requestor} ->
        handle_withdrawal_from(account_number, amount)
        case get_balance(account_number) do
          :no_such_account -> requestor <- :no_such_account
          new_balance -> cond do
            new_balance < 0 -> requestor <- :overdrawn
            new_balance -> requestor <- {:new_balance, account_number, new_balance}
          end
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

  def handle_deposit_into(account_number, amount) do
    account = find_or_create_account(account_number)
    account <- {:deposit, amount}
  end

  def handle_withdrawal_from(account_number, amount) do
    case get_account(account_number) do
      :no_such_account -> :no_such_account
      account -> account <- {:withdraw, amount}
    end
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
      :gproc.get_value({:n, :g, account_number})
    rescue
      [ArgumentError] -> :no_such_account
    end
  end

  def register_account(account_number) do
    account = spawn_link(Account, :start, [])
    :gproc.reg({:n, :g, account_number}, account)
  end
end
