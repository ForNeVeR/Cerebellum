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
-include("../include/cerebellum_db.hrl").

get_tasks(Session,User) ->
    case Session#session.user_id == cerebellum_db:user_id(User) of
	true ->
	    string:concat(io_lib:format("HTTP/1.1 200 OK~nContent-Length: 100500~nContent-Type: text/xml~nConnection: close~n~n",[]),
			  io_lib:format("<tasks user=\"~s\">~n~s</tasks>",[User,cerebellum_db:fetch_tasks_xml(User,["public","protected","private"])]));
	false ->
	    string:concat(io_lib:format("HTTP/1.1 200 OK~nContent-Length: 100500~nContent-Type: text/xml~nConnection: close~n~n",[]),
			  io_lib:format("<tasks user=\"~s\">~n~s</tasks>",[User,cerebellum_db:fetch_tasks_xml(User,["public"])]))
    end.

get_header(Headers,FieldName) ->
    case lists:filter(fun(Header) ->
			      {http_header,_,HttpField,_,_} = Header,
			      HttpField == FieldName
		      end,
		      Headers) of
	[{http_header,_,_,_,Value}] ->
	    Value;
	_ -> []
    end.

with_session(User,Headers,Action) ->
    case re:split(get_header(Headers,"Cookie"),"=",[{return,list}]) of
	[[]] -> io_lib:format("HTTP/1.1 403 Forbidden~n~n");
	["Session",SessionID] ->
	    case catch cerebellum_db:fetch_session(SessionID) of
		{'EXIT',_} -> %% Session not found
		    io_lib:format("HTTP/1.1 403 Forbidden~n~n");
		Session -> %% Session found
		    case catch cerebellum_db:user_id(User) of
			{'EXIT',_} -> %% user not found
			    io_lib:format("HTTP/1.1 404 Not Found~n~n");
			UserID ->
			    case UserID == Session#session.user_id of
				true ->
				    Action();
				false ->
				    io_lib:format("HTTP/1.1 403 Forbidden~n~n")
			    end
		    end
	    end;
	_ -> io_lib:format("HTTP/1.1 403 Forbidden~n~n") %% may be this should return "bad request" or something like it
    end.

get_request({{http_request, 'GET',{abs_path, "/"},Version},Headers,[]}) ->
    %% don't know what this should return, probably something
    %% like own&friends' tasks
    io_lib:format("HTTP/1.1 501 Not Implemented~n~n");
get_request({{http_request, 'GET',{abs_path, "/sessions/"},Version},Headers,[]}) ->
    %% obv, we do not want someone to get a list of all sessions
    io_lib:format("HTTP/1.1 403 Forbidden~n~n");
get_request({{http_request, 'GET',{abs_path, Path},Version},Headers,[]}) ->
    ["",User,""] = re:split(Path,"/",[{return,list}]),
    case catch cerebellum_db:user_id(User) of
	{'EXIT',_} -> %% user not found
	    io_lib:format("HTTP/1.1 404 Not Found~n~n");
	_ -> %% user found
	    ["Session",SessionID] = re:split(get_header(Headers,"Cookie"),"=",[{return,list}]),
	    case catch cerebellum_db:fetch_session(SessionID) of
		{'EXIT',_} -> %% Session not found
		    io_lib:format("HTTP/1.1 403 Forbidden~n~n");
		Session -> %% Session found
		    get_tasks(Session,User)
	    end
    end;
get_request({{http_request, 'GET',{abs_path, Path},Version},Headers,Data}) ->
    %% this should never ever be matched. but, just in case...
    io_lib:format("HTTP/1.1 500 Internal Server Error~n~n").

put_request({{http_request, 'PUT',{abs_path, "/"},Version},Headers,Data}) ->
    %% NOTE We don't have any user info yet, so this is forbidden
    io_lib:format("HTTP/1.1 403 Forbidden~n~n");
put_request({{http_request, 'PUT',{abs_path, "/sessions/"},Version},Headers,Data}) ->
    %% NOTE obv, we do not want someone to replace all sessions
    io_lib:format("HTTP/1.1 403 Forbidden~n~n");
put_request({{http_request, 'PUT',{abs_path, Path},Version},Headers,Data}) ->
    ["",User,TaskID] = re:split(Path,"/",[{return,list}]),
    
    with_session(User,Headers,
		 fun()->
			 case catch parser:task_xml(Data) of
			     {task,_,_,Name,Mode,State} -> %% data parsed ok
				 %% here we don't care if user is nonexistent, with_session would handle that
				 UserID = cerebellum_db:user_id(User),
				 cerebellum_db:write_task(TaskID,UserID,Name,Mode,State),
				 ResponseData = io_lib:format("<task id=\"~s\" mode=\"~s\" state=\"~s\" name=\"~s\" />",
							      [TaskID,
							       Mode,
							       State,
							       Name]),
				 string:concat(io_lib:format(
						 "HTTP/1.1 200 OK~nContent-Length: ~b~nContent-Type: text/xml~nConnection: close~n~n",
						 [length(ResponseData)*4]),
					       ResponseData);
			     {'EXIT',_} -> io_lib:format("HTTP/1.1 500 Internal Server Error~n~n") %% could not parse
			 end

		 end).

post_request({{http_request, 'POST',{abs_path, "/"},Version},Headers,Data}) -> %%add new user
    %% Same data syntax as auth
    case catch parser:auth_xml(Data) of
	{ok,Username,Password} -> %% parsed data
	    case catch cerebellum_db:write_user(cerebellum_db:next_id(user),Username,Password) of
		{atomic,ok} -> %% successful
		    io_lib:format("HTTP/1.1 200 OK~n~n");
		{'EXIT',_} -> %% failed
		    io_lib:format("HTTP/1.1 500 Internal Server Error~n~n") %% that's most probable cause of write_user failure 
	    end;
	{'EXIT',_} -> io_lib:format("HTTP/1.1 500 Internal Server Error~n~n") %% could not parse
    end;
post_request({{http_request, 'POST',{abs_path, "/sessions/"},Version},Headers,Data}) -> %%login request
    case catch parser:auth_xml(Data) of
	{ok,Username,Password} -> %% parsed data
	    case catch cerebellum_db:create_session(Username,Password) of
		{session,SessionID,UserID} -> %% auth successful
		    io_lib:format("HTTP/1.1 200 OK~nContent-Length: 35~nContent-Type: text/xml~nConnection: close
Set-Cookie: Session=~s~n~n<session>~s</session>",
				  [SessionID,SessionID]);
		{'EXIT',_} -> io_lib:format("HTTP/1.1 403 Forbidden~n~n") %%auth failed
	    end;
	{'EXIT',_} -> io_lib:format("HTTP/1.1 500 Internal Server Error~n~n") %% could not parse
    end;
post_request({{http_request, 'POST',{abs_path, Path},Version},Headers,Data}) -> %%new task
    ["",User,Parent] = re:split(Path,"/",[{return,list}]),
    with_session(User,Headers,
		 fun() ->
			 Task = parser:task_xml(Data),
			 TaskID = cerebellum_db:next_id(task),
			 case
			     catch cerebellum_db:write_task(
				     TaskID,
				     cerebellum_db:user_id(User),
				     Task#task.name,
				     Task#task.mode,
				     Task#task.state)
			 of
			     {atomic, ok} -> %%no error
				 ResponseData = io_lib:format("<task id=\"~s\" mode=\"~s\" state=\"~s\" name=\"~s\" />",
							      [TaskID,
							       util:utf8(Task#task.mode),
							       util:utf8(Task#task.state),
							       util:utf8(Task#task.name)]),
				 string:concat(io_lib:format(
						 "HTTP/1.1 200 OK~nContent-Length: ~b~nContent-Type: text/xml~nConnection: close~n~n",
						 [length(ResponseData)*4]),
					       ResponseData);
			     _ -> %%some error
				 "HTTP/1.1 500 Internal Server Error"
			 end
		 end).

delete_request({{http_request, 'DELETE',{abs_path, Path},Version},Headers,[]}) ->
    ["",User,TaskIDString] = re:split(Path,"/",[{return,list}]),
    with_session(User,Headers,
		 fun() ->
			 case string:to_integer(TaskIDString) of
			     {TaskID, []} -> %% all data was parsed
				 Task = cerebellum_db:fetch_task(TaskID), %% reading task from db to ensure that user owns it
				 UserID = cerebellum_db:fetch_user(User),
				 case Task#task.user_id == UserID of
				     true -> %% yes, the user is logged in and is the owner
					 case catch mnesia:delete({task, TaskID}) of %% NOTE maybe write a function for that in cerebellum_db
					     {atomic,ok} ->
						 io_lib:format("HTTP/1.1 200 OK~nConnection: close~n~n");
					     {'EXIT',_} -> %% failed
						 io_lib:format("HTTP/1.1 500 Internal Server Error~n~n") %% that's most probable cause of failure
					 end;
				     false -> %% foo, user is trying to delete someone other's task
					 io_lib:format("HTTP/1.1 403 Forbidden~n~n")
				 end
			 end
		 end);
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
