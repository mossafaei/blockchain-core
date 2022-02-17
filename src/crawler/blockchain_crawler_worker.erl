%%
%% Blockchain Crawler Worker
%% 
-module(blockchain_crawler_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%%
%% API export functions.
%% 
-export([
    start_link/1
]).


%%
%% gen_server export functions.
%% 
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


%%
%% API functions
%% 

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, Args).



%%
%% gen_server callback functions
%% 

init(Args) ->
    SwarmTID = blockchain_swarm:tid(),
    blockchain_crawler_logic:dfs_on_peers(SwarmTID, mark, ip, address).

handle_call([], [], []) ->
    to_do.

handle_cast([], []) ->
    to_do.

handle_info([], []) ->
    to_do.

terminate([], []) ->
    to_do.

code_change([], [], []) ->
    to_do.


%%
%% Internal functions
%% 