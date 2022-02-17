%%
%% Blockchain Crawler Supervisor
%% 
-module(blockchain_crawler_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


%%
%% API functions
%% 
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%
%% Supervisor callbacks
%% 
init(Args) ->
    to_do.