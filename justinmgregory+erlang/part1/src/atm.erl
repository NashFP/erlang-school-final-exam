%%% atm.erl
%%% @doc A simple ATM server for the NashFP Erlang School final exam
%%% @author Justin Gregory

-module(atm).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, check_balance/1, deposit/2, withdraw/2, stop/0]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, terminate/2]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> started.
start_link() ->
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    started.

-spec check_balance(integer()) -> {balance, number()} | no_such_account | atm_closed.
check_balance(AccountNumber) ->
	try_call({check_balance, AccountNumber}).

% @todo: what if Amount is negative?
-spec deposit(integer(), number()) -> {new_balance, number()} | atm_closed.
deposit(AccountNumber, Amount) ->
	case check_balance(AccountNumber) of
		{balance, Balance} -> set_balance(AccountNumber, Balance + Amount);
		no_such_account -> set_balance(AccountNumber, Amount);
		atm_closed -> atm_closed
	end.

-spec withdraw(integer(), number()) -> {new_balance, number()} | overdrawn | no_such_account | atm_closed.
withdraw(AccountNumber, Amount) ->
	case check_balance(AccountNumber) of
		{balance, Balance} when Balance >= Amount -> set_balance(AccountNumber, Balance - Amount);
		{balance, _Balance} -> overdrawn;
		no_such_account -> no_such_account;
		atm_closed -> atm_closed
	end.

stop() ->
	try_call(stop).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	DB_id = atm_db:new(),
	{ok, DB_id}.

handle_call({check_balance, AccountNumber}, _From, State) ->
	case atm_db:get_balance(State, AccountNumber) of
		{error, not_found} -> {reply, no_such_account, State};
		Amount -> {reply, {balance, Amount}, State}
	end;

handle_call({set_balance, AccountNumber, Amount}, _From, State) ->
	atm_db:set_balance(State, AccountNumber, Amount),
	{reply, {new_balance, Amount}, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.

terminate(_Reason, State) ->
	atm_db:delete(State),
    ok.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

try_call(Message) ->
	try	gen_server:call(?SERVER, Message) of
		Result -> Result
	catch
		_:{noproc,_} -> atm_closed
	end.

set_balance(AccountNumber, NewBalance) ->
	try_call({set_balance, AccountNumber, NewBalance}),
	{new_balance, NewBalance}.


%% ------------------------------------------------------------------
%% Testing Functions
%% ------------------------------------------------------------------

-ifdef(TEST).

start_stop_test() ->
	?assertEqual(started, start_link()),
	?assertEqual(stopped, stop()).

check_balance_on_nonexistent_account_test() ->
	start_link(),
	?assertEqual(no_such_account, check_balance(1)),
	stop().

atm_closed_test() ->
	?assertEqual(atm_closed, check_balance(1)),
	?assertEqual(atm_closed, deposit(1, 100)),
	?assertEqual(atm_closed, withdraw(1, 100)),
	?assertEqual(atm_closed, stop()).

deposit_test() ->
	start_link(),
	?assertEqual({new_balance, 100.00}, deposit(1, 100.00)),
	?assertEqual({balance, 100.00}, check_balance(1)),
	?assertEqual({new_balance, 200.00}, deposit(1, 100.00)),
	?assertEqual({balance, 200.00}, check_balance(1)),
	stop().

withdrawal_test() ->
	start_link(),
	?assertEqual({new_balance, 100.00}, deposit(1, 100.00)),
	?assertEqual({new_balance, 50.00}, withdraw(1, 50.00)),
	?assertEqual(overdrawn, withdraw(1, 100.00)),
	?assertEqual(no_such_account, withdraw(2, 100.00)),
	stop().

-endif.
