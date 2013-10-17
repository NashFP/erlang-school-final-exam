%%% atm_db.erl
%%% @doc Data backend for atm module - currently uses ets
%%% @author Justin Gregory

-module(atm_db).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/0, set_balance/3, get_balance/2, delete/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new() ->
	ets:new(atm_db, [private]).

set_balance(DB, Account, Amount) ->
	ets:insert(DB, {Account, Amount}).

-spec get_balance(term(), integer()) -> number() | {error, not_found}.
get_balance(DB, Account) ->
	try ets:lookup_element(DB, Account, 2) of
		Result -> Result
	catch
		error:badarg -> {error, not_found}
	end.

delete(DB) ->
	ets:delete(DB).


%% ------------------------------------------------------------------
%% Testing Functions
%% ------------------------------------------------------------------

-ifdef(TEST).

create_and_delete_test() ->
	DB = new(),
	delete(DB).

set_and_lookup_test() ->
	DB = new(),
	set_balance(DB, 1, 100.00),
	?assertEqual(100.00, get_balance(DB, 1)),
	delete(DB).

check_nonexistent_account_test() ->
	DB = new(),
	?assertEqual({error, not_found}, get_balance(DB, 1)),
	delete(DB).

-endif.
