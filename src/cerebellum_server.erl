%% Copyright (C) 2011 by ForNeVeR
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
-export([start_link/0, stop/1]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-include("log.hrl").

%% == Public functions ==

start_link() ->
    ?LOG("cerebellum_server:start_link()~n"),
    gen_server:start_link(?MODULE, [], []).

stop(PID) ->
    ?LOG("cerebellum_server:stop~n)"),
    gen_server:call(PID, stop).

%% == gen_server behavior ==

init(Args) ->
    ?LOG("cerebellum_server:init(~p)~n", [Args]),
    State = {},
    {ok, State}.

handle_call(stop, From, State) ->
    ?LOG("cerebellum_server:handle_call(stop, ~p, ~p)~n", [From, State]),
    {stop, normal, ok, State};
handle_call(Request, From, State) ->
    ?LOG("cerebellum_server:handle_call(~p, ~p, ~p)~n", [Request, From, State]),
    {noreply, State}.

handle_cast(Request, State) ->
    ?LOG("cerebellum_server:handle_cast(~p, ~p)~n", [Request, State]),
    NewState = handle_message(State, Request),
    {noreply, NewState}.

handle_info(Info, State) ->
    ?LOG("cerebellum_server:handle_info(~p, ~p)~n", [Info, State]),
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG("cerebellum_server:terminate(~p, ~p)~n", [Reason, State]),
    ok.

code_change(OldVsn, State, Extra) ->
    ?LOG("cerebellum_server:code_change(~p, ~p, ~p)~n", [OldVsn, State, Extra]),
    {ok, State}.

%% == Private functions ==

handle_message(State, Request) ->
    ?LOG("cerebellum_server:handle_message(~p, ~p)~n", [State, Request]),
    State.
