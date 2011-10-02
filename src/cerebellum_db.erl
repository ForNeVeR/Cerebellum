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

-module(cerebellum_db).
-export([start/0, init_schema/0, testdata/0, fetch_tasks_xml/2, write_user/3, write_task/5,
	next_id/1, user_id/1, create_session/2, fetch_session/1]).
-include_lib("stdlib/include/qlc.hrl").
-include("../include/cerebellum_db.hrl").

start()->
    mnesia:start().    

init_schema() ->
    start(),
    mnesia:create_schema([node()]),
    mnesia:create_table(user,
			[{attributes, record_info(fields,user)}]),
    mnesia:create_table(task,
			[{attributes, record_info(fields,task)}]),
    mnesia:create_table(friend,
			[{attributes, record_info(fields,friend)}]),
    mnesia:create_table(session,
			[{attributes, record_info(fields,session)}]),
    mnesia:create_table(sequence,
			[{attributes, record_info(fields,sequence)}]).

user_id(UserName)-> %%returns id# of user with given name
    case catch mnesia:transaction(fun() ->
					  qlc:e(qlc:q([U || U <-mnesia:table(user), U#user.name == UserName]))
				  end) of
	       {atomic,[]} -> %% not found
		 exit({error,notfound});
	       {atomic,[User]} -> 
		 User#user.user_id
	 end.

next_id(Table) -> %% returns next free id for given table
    {atomic, ID} = mnesia:transaction(
      fun() ->
	      case mnesia:read(sequence, Table, write) of
		  [] ->
		      mnesia:write(#sequence{
				      name = Table,
				      next = 2}),
		      1;
		  [#sequence{name=Table,next=Next}] ->
		      mnesia:write(#sequence{
				      name = Table,
				      next = Next + 1}),
		      Next
	      end
      end),
    ID.
    

write_user(UserID,Name,Password) -> %% writes new or updates existing user
    Hash = crypto:sha(list_to_binary(Password)),
    Trans = fun() ->
		   mnesia:write(#user{user_id = UserID,
				      name = Name,
				      password_hash = Hash})
	   end,
    mnesia:transaction(Trans).

write_task(TaskID,UserID,TaskName,Mode,State) -> %% writes new or updates existing task
    Trans = fun() ->
		    mnesia:write(#task{task_id = TaskID,
				       user_id = UserID,
				       name = TaskName,
				       mode = Mode,
				       state = State})
	    end,
    mnesia:transaction(Trans).

fetch_tasks_xml(UserName,Modes) -> 
    %% returns XML representation of user's tasks
    %% UserName -- obv
    %% Modes -- list of task modes (public, private, etc) to fetch
    %% NOTE This should die horribly. Since we decided to be somewhat
    %%      presentation agnostic, we should have some "unparser"
    %%      that takes records, and produces XML, YAML, S-Exp,
    %%      or even Brainfuck program representing these records.
    UserID = user_id(UserName),
    {atomic,Tasks} = mnesia:transaction(
		       fun() ->
			       qlc:e(qlc:q([T || T <-mnesia:table(task),
						 T#task.user_id == UserID]))
		       end),
    TasksXML = lists:map(
		 fun(Task)->
			 io_lib:format("<task handle=\"~w\" mode=\"~s\" state=\"~s\" name=\"~s\" />~n",
				       [Task#task.task_id,Task#task.mode,Task#task.state,Task#task.name])
		 end,
		 Tasks),
    lists:foldl(fun(Task,AccIn) ->
			string:concat(AccIn,Task)
		end, "", TasksXML).

testdata()->
    TestUserID = next_id(user),
    write_user(TestUserID,"Hagane","gfhjkm"),
    write_task(next_id(task),TestUserID,"Вкурить эрланга","public","done"),
    write_task(next_id(task),TestUserID,"Показать микелю протокол","protected","todo"),
    write_task(next_id(task),TestUserID,"Забухать","private","todo").

create_session(UserName,Password) ->
    case catch mnesia:transaction(fun() ->
					  qlc:e(qlc:q([U || U <-mnesia:table(user),
							    U#user.name == UserName,
							    U#user.password_hash == crypto:sha(list_to_binary(Password))]))
				  end) of
	       {atomic,[]} -> %% not found
		 exit({error,notfound});
	       {atomic,[User]} -> 
		 SessionID = [lists:nth(random:uniform(16),"1234567890ABCDEF") || _ <- lists:seq(1,16)],
		 Session = #session{session_id = SessionID,
				    user_id = User#user.user_id},
		 mnesia:transaction(fun() ->
					   mnesia:write(Session)
				   end),
		 Session    
	 end.

fetch_session(SessionID)-> %%returns id# of user with given name
    case catch mnesia:transaction(fun() ->
					  qlc:e(qlc:q([S || S <-mnesia:table(session), S#session.session_id == SessionID]))
				  end) of
	       {atomic,[]} -> %% not found
		 exit({error,notfound});
	       {atomic,[Session]} -> 
		 Session
	 end.
