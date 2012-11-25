-module(native_process_test).
-on_load(init/0).

-export([start/0, start_link/0]).
-export([spawn_native/0, send_native/2]).

start() ->
    native_process:start(?MODULE).

start_link() ->
    native_process:start_link(?MODULE).

%% Native process NIFs
spawn_native() ->
    erlang:nif_error({error, not_loaded}).

send_native(_Ref, _Mail) ->
    erlang:nif_error({error, not_loaded}).

init() ->
    SoName = native_process:find_nif(?MODULE),
    erlang:load_nif(SoName, 0).

