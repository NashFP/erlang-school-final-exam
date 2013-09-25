Erlang School Final Exam
========================

For the exam we will use the example of a bank ATM. The ATM supports multiple accounts and maintains a balance. 

Step 1: super simple ATM
------------------------
We will start with a super simple server. It will help everyone get comfortable compiling (and running) code. 

It will also show the idea of a server loop, messages and a receive block.

This super-simple ATM will support the following actions. An account is created by depositing money to it.

````
atm:start() -> started
atm:check_balance(AccountNumber) -> {balance, Amount} | no_such_account | atm_closed
atm:withdraw(AccountNumber, Amount) -> {new_balance, Amount} | overdrawn | no_such_account | atm_closed
atm:deposit(AccountNumber, Amount) -> {new_balance, Amount} | atm_closed
atm:stop() -> stopped | atm_closed
````

To create an account, deposit money to it. To start we won't bother with PIN numbers.


Step 2: a cluster!
------------------
* Bring up multiple Erlang nodes (node1, node2,...). Connect them with ````net_adm:ping````. 
* Bring an ATM up on each node. 
* Deposit money to account 10000 on node1. 
* Check account balances on various nodes and ensure they all agree. 
* Withdraw money from account 10000 on node2. 
* Check account balances on various nodes and ensure they all agree. 
* Add a new node to the cluster. 
* Check account balances from new node and ensure they are correct.
* Disconnect node1 from the cluster. 
* Deposit money to account 10000 on node1.
* Ensure the deposit is refleced on node1, but not on the other nodes.
* Deposit money to account 20000 on node2.
* Ensure the deposit is refleced on node2 and the other conneced nodes, but not on node1.
* Rejoin node1 to the cluster.
* Ensure the balances for each node all jive.


How to submit your exam
-----------------------

Contribute your solution(s) by adding a **folder** named {your twitter handle}+{your erlang-vm language} such as: 
````
/bryan_hunter+erlang/step2
/knewter+elixir/step1
/knewter+erlang/step1
````

Time to play!
