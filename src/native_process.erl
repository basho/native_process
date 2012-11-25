%% Rewrite to not be a gen_server but a simple process w/ receive loop.
-module(native_process).
-behaviour(gen_server).

-export([start_link/1, start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([find_nif/1]).

-record(state, {mod,
                native,
                native_busy,
                mailbox}).

start_link(Mod) ->
    gen_server:start_link(?MODULE, Mod, []).

start(Mod) ->
    gen_server:start(?MODULE, Mod, []).

init(Mod) ->
    Native = Mod:spawn_native(),
    {ok, #state{mod=Mod,
                native=Native,
                native_busy=false,
                mailbox=[]}}.

handle_call(Request, From, State) ->
    Mailbox = [{call, From, Request} | State#state.mailbox],
    State2 = State#state{mailbox=Mailbox},
    State3 = maybe_sendmail(State2),
    {noreply, State3}.

handle_cast(Msg, State) ->
    Mailbox = [{cast, Msg} | State#state.mailbox],
    State2 = State#state{mailbox=Mailbox},
    State3 = maybe_sendmail(State2),
    {noreply, State3}.

handle_info(sendmail, State) ->
    State2 = State#state{native_busy=false},
    State3 = maybe_sendmail(State2),
    {noreply, State3};
handle_info(Msg, State) ->
    Mailbox = [Msg | State#state.mailbox],
    State2 = State#state{mailbox=Mailbox},
    State3 = maybe_sendmail(State2),
    {noreply, State3}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

maybe_sendmail(State=#state{mod=Mod, native=NP, native_busy=Busy, mailbox=Mail}) ->
    case {Mail, Busy} of
        {[], _} ->
            State;
        {_, true} ->
            State;
        {_, false} ->
            case Mod:send_native(NP, lists:reverse(Mail)) of
                ok ->
                    State#state{native_busy=true, mailbox=[]};
                _ ->
                    State
            end
    end.

%% NIF loading helper
find_nif(Mod) ->
    Name = atom_to_list(Mod),
    SoName = case code:priv_dir(Mod) of
                 {error, bad_name} ->
                     case code:which(Mod) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename),"../priv", Name]);
                         _ ->
                             filename:join("../priv", Name)
                     end;
                 Dir ->
                     filename:join(Dir, Name)
             end,
    SoName.
