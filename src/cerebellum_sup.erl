%% Copyright (C) 2012 by ForNeVeR
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
-module(cerebellum_sup).

-behaviour(supervisor).

%% --------------------------------------------------------------------
%% API exports
%% --------------------------------------------------------------------
-export([start_link/0]).

%% --------------------------------------------------------------------
%% Behavior exports
%% --------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(config, {adapters = []}).

%% ====================================================================
%% API functions
%% ====================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% ====================================================================
%% Behavior functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    Config = get_config(),
    Adapters = get_adapters(Config),
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Adapters}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
get_config() ->
    % TODO: Read real config.
    #config{adapters = [cerebellum_db,
                        cerebellum_restful]}.

get_adapters(#config{adapters = Adapters}) ->
    lists:map(fun(Adapter) ->
                  {Adapter, {Adapter, start_link, []},
                   permanent, 2000, worker, [Adapter]}
              end, Adapters).
