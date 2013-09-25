# Erlang School Final Exam - Step 1

See [this link](https://github.com/NashFP/erlang-school-final-exam) for details
mnkay?

## Step 1: super simple ATM
We will start with a super simple server. It will help everyone get comfortable
compiling (and running) code.

It will also show the idea of a server loop, messages and a receive block.

This super-simple ATM will support the following actions.  An account is created
by depositing money to it.

````
Atm.start() -> started
Atm.check_balance(account_number) -> {:balance, amount} | :no_such_account | :atm_closed
Atm.withdraw(account_number, amount) -> {:new_balance, amount} | :overdrawn | :no_such_account | :atm_closed
Atm.deposit(account_number, amount) -> {:new_balance, amount} | :atm_closed
Atm.stop() -> :stopped | :atm_closed
````

To create an account, deposit money to it. To start we won't bother with PIN
numbers.
