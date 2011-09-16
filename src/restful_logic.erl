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

-module(restful_logic).
-export([process_request/1]).
-include_lib("xmerl/include/xmerl.hrl").

get_tasks(Session,User) ->
    string:concat(io_lib:format("HTTP/1.1 200 OK~nContent-Length: 100500~nContent-Type: text/xml~nConnection: close~n~n",[]),
		  io_lib:format("<tasks user=\"~s\">~n~s</tasks>",[User,cerebellum_db:fetch_tasks_xml(User,[])])).
    
get_request({{http_request, 'GET',{abs_path, "/"},Version},Headers,[]}) ->
    %% don't know what this should return, probably something
    %% like own&friends' tasks
    io_lib:format("HTTP/1.1 501 Not Implemented~n~n");
get_request({{http_request, 'GET',{abs_path, "/sessions/"},Version},Headers,[]}) ->
    %% obv, we do not want someone to get a list of all sessions
    io_lib:format("HTTP/1.1 403 Forbidden~n~n");
get_request({{http_request, 'GET',{abs_path, Path},Version},Headers,[]}) ->
    ["",User,""] = re:split(Path,"/",[{return,list}]),
    get_tasks(foo,User);
get_request({{http_request, 'GET',{abs_path, Path},Version},Headers,Data}) ->
    %% this should never ever be matched. but, just in case...
    io_lib:format("HTTP/1.1 500 Internal Server Error~n~n").

put_request({{http_request, 'PUT',{abs_path, Path},Version},Headers,Data}) ->
    %% TODO Implement this
    io_lib:format("HTTP/1.1 501 Not Implemented~n~n").

post_request({{http_request, 'POST',{abs_path, "/"},Version},Headers,Data}) -> %%add new user
    %% TODO Implement this
    io_lib:format("HTTP/1.1 501 Not Implemented~n~n");
post_request({{http_request, 'POST',{abs_path, "/sessions/"},Version},Headers,Data}) -> %%login request
    %% TODO Implement this
    io_lib:format("HTTP/1.1 501 Not Implemented~n~n");
post_request({{http_request, 'POST',{abs_path, Path},Version},Headers,Data}) -> %%new task
    ["",User,Parent] = re:split(Path,"/",[{return,list}]),
    {XMLDocument, _} = xmerl_scan:string(Data),
    Attributes = XMLDocument#xmlElement.attributes,
    Mode = hd(lists:filter(fun(A) -> A#xmlAttribute.name == mode end, Attributes)),
    State = hd(lists:filter(fun(A) -> A#xmlAttribute.name == state end, Attributes)),
    Name = hd(lists:filter(fun(A) -> A#xmlAttribute.name == name end, Attributes)),
    case
	catch cerebellum_db:write_task(
		cerebellum_db:next_id(task),
		cerebellum_db:user_id(User),
		Name#xmlAttribute.value,
		Mode#xmlAttribute.value,
		State#xmlAttribute.value)
    of
	{atomic, ok} -> %%no error
	    ResponseData = io_lib:format("<task id=\"1\" mode=\"~s\" state=\"~s\" name=\"~s\" />",
					 [util:utf8(Mode#xmlAttribute.value),
					  util:utf8(State#xmlAttribute.value),
					  util:utf8(Name#xmlAttribute.value)]),
	    string:concat(io_lib:format(
			    "HTTP/1.1 200 OK~nContent-Length: ~b~nContent-Type: text/xml~nConnection: close~n~n",
			    [length(ResponseData)*4]),
			  ResponseData);
	_ -> %%some error
	    "HTTP/1.1 500 Internal Server Error"
    end.

delete_request({{http_request, 'DELETE',{abs_path, Path},Version},Headers,[]}) ->
    %% TODO Implement this
    io_lib:format("HTTP/1.1 501 Not Implemented~n~n");
delete_request({{http_request, 'DELETE',{abs_path, Path},Version},Headers,Data}) ->
    %% this should never ever be matched. but, just in case...
    io_lib:format("HTTP/1.1 500 Internal Server Error~n~n").

process_request({{http_request, 'GET',{abs_path, Path},Version},Headers,Data}) ->
    get_request({{http_request, 'GET',{abs_path, Path},Version},Headers,Data});
process_request({{http_request, 'PUT',{abs_path, Path},Version},Headers,Data}) ->
    put_request({{http_request, 'PUT',{abs_path, Path},Version},Headers,Data});
process_request({{http_request, 'POST',{abs_path, Path},Version},Headers,Data}) ->
    post_request({{http_request, 'POST',{abs_path, Path},Version},Headers,Data});
process_request({{http_request, 'DELETE',{abs_path, Path},Version},Headers,Data}) ->
    delete_request({{http_request, 'DELETE',{abs_path, Path},Version},Headers,Data}).
