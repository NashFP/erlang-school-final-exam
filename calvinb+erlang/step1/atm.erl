-module(atm).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start/0, check_balance/1, withdraw/2, deposit/2, stop/0]).
-behavior(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% API functions

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []),
	started.

check_balance(AccountNumber) ->
	call({check_balance, AccountNumber}).

withdraw(AccountNumber, Amount) ->
	call({withdraw, AccountNumber, Amount}).

deposit(AccountNumber, Amount) ->
	call({deposit, AccountNumber, Amount}).

stop() ->
	case whereis(?MODULE) of
		undefined ->
			atm_closed;
		_ ->
			gen_server:cast(?MODULE, stop),
			stopped
	end.

%% gen_server callbacks

init(State) ->
	{ok, State}.

handle_call({deposit, AccountNumber, Amount}, _From, State) ->
	NewState = [{now(), AccountNumber, Amount}|State],
	{balance, Balance} = get_balance(AccountNumber, NewState),
	{reply, {new_balance, Balance}, NewState};

handle_call({check_balance, AccountNumber}, _From, State) ->
	{reply, get_balance(AccountNumber, State), State};

handle_call({withdraw, AccountNumber, Amount}, _From, State) ->
	case get_balance(AccountNumber, State) of
		no_such_account ->
			{reply, no_such_account, State};
		{balance, Balance} ->
			NewBalance = Balance - Amount,
			case NewBalance < 0 of
				true ->
					{reply, overdrawn, State};
				false ->
					NewState = [{now(), AccountNumber, -Amount}|State],
					{reply, {new_balance, NewBalance}, NewState}
			end
	end.


handle_cast(stop, _State) ->
	{stop, shutdown, []}.


terminate(_, _) ->
	ok.

%% Private functions

call(Message) ->
	case whereis(?MODULE) of
		undefined ->
			atm_closed;
		_ ->
			gen_server:call(?MODULE, Message, 1000)
	end.

get_balance(AccountNumber, State) ->
	Amounts = [Amount || {_Now, AccountNumber1, Amount} <- State, AccountNumber1 =:= AccountNumber],
	case Amounts of
		[] ->
			no_such_account;
		_ ->
			{balance, lists:sum(Amounts)}
	end.

%% Tests

start_test() ->
	?assertEqual(started, start()),
	stop().

deposit_when_closed_test() ->
	?assertEqual(atm_closed, deposit(123, 100)).

deposit_once_test() ->
	start(),
	?assertEqual({new_balance, 100}, deposit(123, 100)),
	stop().

deposit_twice_test() ->
	start(),
	deposit(123, 50),
	?assertEqual({new_balance, 75}, deposit(123, 25)),
	stop().

deposit_to_two_accounts_test() ->
	start(),
	deposit(123, 100),
	?assertEqual({new_balance, 110}, deposit(456, 110)),
	stop().

check_balance_when_closed_test() ->
	?assertEqual(atm_closed, check_balance(123)).

check_balance_when_no_such_account_test() ->
	start(),
	?assertEqual(no_such_account, check_balance(124)),
	stop().

check_balance_when_account_exists_test() ->
	start(),
	deposit(123, 100),
	?assertEqual({balance, 100}, check_balance(123)),
	stop().

withdraw_when_closed_test() ->
	?assertEqual(atm_closed, withdraw(123, 100)).

withdraw_when_no_such_account_test() ->
	start(),
	?assertEqual(no_such_account, withdraw(123, 100)),
	stop().

withdraw_when_enough_test() ->
	start(),
	deposit(123, 100),
	?assertEqual({new_balance, 60}, withdraw(123, 40)),
	stop().

withdraw_when_overdrawn_test() ->
	start(),
	deposit(123, 100),
	?assertEqual(overdrawn, withdraw(123, 150)),
	stop().

stop_when_closed_test() ->
	?assertEqual(atm_closed, stop()).

stop_when_open_test() ->
	start(),
	?assertEqual(stopped, stop()).