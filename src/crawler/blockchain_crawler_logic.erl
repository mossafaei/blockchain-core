%%
%% Blockchain Crawler Logic 
%%

-module(blockchain_crawler_logic).

-export([
    dfs_on_peers/5,
    refresh_peerbook/2,
    get_connected_peers/2,
    find_peer/2,
    check_dfs_condition/4,
    for/5
]).

- spec refresh_peerbook(ets:tab(), string()) -> ok.
refresh_peerbook(SwarmTID, P2PAddress) ->
    PeerBook = libp2p_swarm:peerbook(SwarmTID),
    libp2p_peerbook:refresh(PeerBook, libp2p_crypto:p2p_to_pubkey_bin(P2PAddress)),
    ok.

get_connected_peers(SwarmTID, P2PAddress) ->
    case find_peer(SwarmTID, P2PAddress) of
        {ok, Peer} -> 
                Connections = [libp2p_crypto:pubkey_bin_to_p2p(P)
                                    || P <- libp2p_peer:connected_peers(Peer)],
                Connections;
        _ -> ok
    end.

find_peer(SwarmTID, P2PAddress) -> 
    PeerBook = libp2p_swarm:peerbook(SwarmTID),
    libp2p_peerbook:get(PeerBook, libp2p_crypto:p2p_to_pubkey_bin(P2PAddress)).


peer_connect(SwarmTID, P2PAddress) ->
    libp2p_swarm:connect(SwarmTID, P2PAddress),
    ok.

check_dfs_condition(SwarmTID, MarkTID, IpTID, P2PAddress) ->
    case ets:lookup(MarkTID, P2PAddress) of
        %% If the address is not available in Mark table
        [] -> dfs_on_peers(SwarmTID, MarkTID, IpTID, P2PAddress, 10);
        
        %% If the address is available in Mark table
         _ -> ok
    end,
    ok.

for([], _, _, _, _) -> ok;
for(LST, SwarmTID, MarkTID, IpTID, P2PAddress) ->
    [Item| Other] = LST,
    check_dfs_condition(SwarmTID, MarkTID, IpTID, Item),
    for(Other, SwarmTID, MarkTID, IpTID, P2PAddress).

- spec dfs_on_peers(ets:tab(), ets:tab(), ets:tab(), string(), integer()) -> ok.
dfs_on_peers(_, _, _, _, 0) -> ok;
dfs_on_peers(SwarmTID, MarkTID, IpTID, P2PAddress, MaxTry) ->
    io:format("Start DFS on the node: " ++ P2PAddress ++ "\n"),
    timer:sleep(7000),
    refresh_peerbook(SwarmTID, P2PAddress),
    timer:sleep(1000),
    case find_peer(SwarmTID, P2PAddress) of
        {ok, Peer} -> 
                    [BestAddress| _] = libp2p_transport:sort_addrs(SwarmTID, libp2p_peer:listen_addrs(Peer)),
                    ets:insert(IpTID, {P2PAddress, BestAddress}),
                    ets:insert(MarkTID, {P2PAddress, true}),
                    PeerList = get_connected_peers(SwarmTID, P2PAddress),
                    for(PeerList, SwarmTID, MarkTID, IpTID, P2PAddress);
        _ -> dfs_on_peers(SwarmTID, MarkTID, IpTID, P2PAddress, MaxTry - 1)
    end,
    ok.