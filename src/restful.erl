%% Copyright (C) 2011 by Hagane
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

-module (restful).
-export ([run/0, worker/1]).

-include("../include/log.hrl").

%% TODO This belongs in config file, not here.
-define(TCP_OPTIONS, [{packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false}]).
-define(PORT_NO, 5000).

-behaviour(application).
-export([start/2, stop/1]).

receive_loop(ListenSocket) ->
	spawn(restful,worker,[gen_tcp:accept(ListenSocket)]),
	receive_loop(ListenSocket).

run() ->
	case gen_tcp:listen(?PORT_NO,?TCP_OPTIONS) of
		{ok, ListenSocket} ->
			?LOG("Listening on ~p...~n",[?PORT_NO]),
			receive_loop(ListenSocket),
			?LOG("Listening stopped.~n"),
			gen_tcp:close(ListenSocket);
		{error, Reason} ->
			?LOG("Failed to listen on ~p: ~p~n",[?PORT_NO,Reason])
	end.

parse_request(Request)->
	{ok,TopHeader,Rest} = erlang:decode_packet(http,Request,[]),
	ParsedHeaders = parse_headers(erlang:decode_packet(httph,Rest,[]),[]),
	{TopHeader,ParsedHeaders}.

parse_headers({ok, http_eoh, _},ParsedHeaders)->
	ParsedHeaders;
parse_headers({ok,Header,Rest},ParsedHeaders) ->
	parse_headers(erlang:decode_packet(httph,Rest,[]),[Header | ParsedHeaders]).		

worker({error, Reason}) ->
	{error, Reason};
worker({ok, TcpSocket}) ->
	{ok,Request} = gen_tcp:recv(TcpSocket,0),
	{{http_request, Method,{abs_path, Path},Version},Headers} = parse_request(list_to_binary(Request)),
	case Method of
		'PUT' ->
			{ok,Data} = gen_tcp:recv(TcpSocket,0);
		'POST' ->
			{ok,Data} = gen_tcp:recv(TcpSocket,0);
		'GET' ->
			Data = [];
		'DELETE' ->
			Data = []
	end,
	RequestObject = {{http_request, Method,{abs_path, Path},Version},Headers,Data},
	?LOG("Worker ~p started for ~p request on ~p.~n",[self(),Method,Path]),
	Response = restful_logic:process_request(RequestObject),
	gen_tcp:send(TcpSocket,Response),
	gen_tcp:close(TcpSocket).

%% application behaviour

start(normal,_) ->
    PID = spawn(restful, run, []),
    {ok, PID, [PID]}.

stop(_) ->
    ok.
	
