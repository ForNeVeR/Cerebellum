-module(cerebellum_db).
-export([start/0, init_schema/0, testdata/0, fetch_tasks_xml/2, write_user/3, write_task/5,
	next_id/1, user_id/1]).
-include_lib("stdlib/include/qlc.hrl").
-include("cerebellum_db.hrl").

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
    {atomic,User} =
	mnesia:transaction(fun() ->
				   qlc:e(qlc:q([U || U <-mnesia:table(user), U#user.name == UserName])) end),
    (hd(User))#user.user_id.

next_id(Table) ->
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
    

write_user(UserID,Name,Password)->
    Hash = crypto:sha(list_to_binary(Password)),
    Trans = fun() ->
		   mnesia:write(#user{user_id = UserID,
				      name = Name,
				      password_hash = Hash})
	   end,
    mnesia:transaction(Trans).

write_task(TaskID,UserID,TaskName,Mode,State) ->
    Trans = fun() ->
		    mnesia:write(#task{task_id = TaskID,
				       user_id = UserID,
				       name = TaskName,
				       mode = Mode,
				       state = State})
	    end,
    mnesia:transaction(Trans).

fetch_tasks_xml(UserName,Modes) ->
    UserID = user_id(UserName),
    {atomic,Tasks} = mnesia:transaction(fun() -> qlc:e(qlc:q([T || T <-mnesia:table(task), T#task.user_id == UserID])) end),
    TasksXML = lists:map(fun(Task)-> io_lib:format("<task handle=\"~w\" mode=\"~s\" state=\"~s\" name=\"~s\" />~n",
					[Task#task.task_id,Task#task.mode,Task#task.state,Task#task.name]) end,
	      Tasks),
    lists:foldl(fun(Task,AccIn) ->
			string:concat(AccIn,Task)
		end, "", TasksXML).

testdata()->
    TestUserID = next_id(user),
    write_user(TestUserID,"Hagane","gfhjkm"),
    write_task(next_id(task),TestUserID,"Вкурить эрланга",public,done),
    write_task(next_id(task),TestUserID,"Показать микелю протокол",protected,todo),
    write_task(next_id(task),TestUserID,"Забухать",private,todo).
    
