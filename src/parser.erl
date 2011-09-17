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

%% Unified parsing module.
%% Should export only wrappers, that decide whether data is in xml, json, yaml
%% or whatever and call appropriate functions.

-module(parser).
-export([task_xml/1]).
-include("../include/cerebellum_db.hrl").
-include("../include/log.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% task xml representation^
%% <task [id="0"] mode="mode" state="state" name="Task" />
%% 'id' attribute may be omitted if the task is not in db yet
%% or it can be inferred from other parts of request
task_xml(XMLString)->
    case catch xmerl_scan:string(XMLString) of
	{fatal, {Error,_}} -> %% Something is terribly wrong, we can't continue.
	    ?LOG("error: parser: ~p on:~n~p~n",[Error,XMLString]),
	    exit({error,badarg});
	{XMLElement,Rest} -> %% Everything shiny so far...
	    if 
		XMLElement#xmlElement.name == task -> %% and we actually got xml representing task!
		    %% warn if not all string was parsed
		    if not(Rest == []) ->
			    ?LOG("warning: parser: unparsed remainder: ~p~n",[Rest]);
		       true -> ok
		    end,

		    Attributes = XMLElement#xmlElement.attributes,
		    Mode = hd(lists:filter(fun(A) -> A#xmlAttribute.name == mode end, Attributes)),
		    State = hd(lists:filter(fun(A) -> A#xmlAttribute.name == state end, Attributes)),
		    Name = hd(lists:filter(fun(A) -> A#xmlAttribute.name == name end, Attributes)),
		    %% let's check if this task has an attribute named 'id'
		    case lists:filter(fun(A) -> A#xmlAttribute.name == id end, Attributes) of
			[] -> %% no id attribute
			    #task{name = Name#xmlAttribute.value,
				  mode = Mode#xmlAttribute.value,
				  state = State#xmlAttribute.value};
			[ID|_] -> %% one or more id attribute, we use only first one
			    #task{task_id = ID#xmlAttribute.value,
				  name = Name#xmlAttribute.value,
				  mode = Mode#xmlAttribute.value,
				  state = State#xmlAttribute.value}
		    end;
		true -> %% that's not task, that's something else
		    ?LOG("error: parser: expected: task; received: ~p~n",[XMLElement#xmlElement.name]),
		    exit({error,badarg})
	    end
    end.
