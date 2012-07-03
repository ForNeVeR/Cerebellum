%% Copyright (C) 2011-2012 by ForNeVeR
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
-module(cerebellum_server).
-export([start_link/1, stop/0]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-record(state, {clients = []}).

-define(SERVER, ?MODULE).

%% == Public functions ==

start_link(_Config) ->
    gen_server:start_link({local, ?SERVER}, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

register_client(PID) ->
    gen_server:cast(?SERVER, {register_client, PID}).

get_tasks(UserID) ->
    gen_server:call(?SERVER, {get_tasks, UserID}).

%% == gen_server behavior ==

init(_Args) ->
    State = #state{},
    {ok, State}.

handle_call(stop, From, State) ->
    {stop, normal, ok, State};
handle_call({get_tasks, UserID}, From, State) ->
    Reply = cerebellum_db:fetch_tasks(UserID, []),
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({register_client, PID}, State) ->
    Clients = State#state.clients,
    NewState = State#state{clients = [PID | Clients]},
    {noreply, NewState}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% == Private functions ==

handle_message(State, Request) ->
    State.
