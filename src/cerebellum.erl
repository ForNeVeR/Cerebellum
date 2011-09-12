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
-module(cerebellum).
-export([start/0, stop/0]).

-include("log.hrl").

-behavior(application).
-export([start/2, start_phase/2, prep_stop/1, stop/1, config_change/3]).

%% === Public functions ===

start() ->
    application:start(cerebellum).

stop() ->
    application:stop(cerebellum).

%% === application behavior ===

start(normal, Args=[Port]) ->
    ?LOG("cerebellum:start(normal, ~p)~n", [Args]),
    {ok, PID} = cerebellum_server:start_link(Port),
    State = [PID],
    {ok, PID, State}.

start_phase(_, _) ->
    {error, not_implemented}.

prep_stop(State=[PID]) ->
    ?LOG("cerebellum:stop(~p)~n", [State]),
    ok = cerebellum_server:stop(PID),
    State = [],
    State.

stop(_) ->
    ?LOG("cerebellum:stop~n"),
    ok.

config_change(_, _, _) ->
    ?LOG("cerebellum:config_change~n"),
    ok. 
