%% simulation.pl — Multi-replica HotStuff simulation
%%
%% Runs the HotStuff protocol across N replicas with a round-robin leader.
%% Messages are synchronous (no network partition modeling yet).
%%
%% The simulation loop:
%%   1. Leader for the round proposes a block
%%   2. All replicas process the proposal and vote (or don't)
%%   3. Votes are collected; if quorum is reached, a QC forms
%%   4. Next round begins with the QC passed to the new leader
%%
%% Supports both the happy path (all honest) and faulty leaders (skipped
%% rounds where no proposal is made, triggering view change).

:- use_module(hotstuff).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(assoc)).

%%%=========================================================================
%%% Network state
%%%=========================================================================

%% net{
%%   n:        Number of replicas,
%%   replicas: Assoc from ReplicaId -> ReplicaState,
%%   round:    Current round,
%%   last_qc:  QC from the previous round (justifies the next proposal),
%%   log:      List of events (newest first) for inspection
%% }

fresh_network(N, Net) :-
    numlist(1, N, Ids),
    maplist(make_replica_pair, Ids, Pairs),
    list_to_assoc(Pairs, Replicas),
    genesis_qc(GQC),
    Net = net{
        n: N,
        replicas: Replicas,
        round: 1,
        last_qc: GQC,
        log: []
    }.

make_replica_pair(Id, Id-State) :-
    fresh_replica(Id, State).

%%%=========================================================================
%%% Leader rotation
%%%=========================================================================

%% leader(+Round, +N, -LeaderId) — round-robin.
leader(Round, N, Leader) :-
    Leader is ((Round - 1) mod N) + 1.

%%%=========================================================================
%%% Simulation step: one round
%%%=========================================================================

%% run_round(+Command, +NetIn, -NetOut)
%%
%% Execute one round of HotStuff:
%%   1. Leader proposes block(Round, Parent, Command) justified by last_qc
%%   2. Each replica calls on_proposal, producing a vote (or not)
%%   3. Votes collected into a QC if quorum reached
%%   4. Network state updated for next round

run_round(Command, Net0, NetOut) :-
    Round = Net0.round,
    N = Net0.n,
    LastQC = Net0.last_qc,
    qc_block(LastQC, ParentBlock),

    %% 1. Leader proposes
    Block = block(Round, ParentBlock, Command),
    leader(Round, N, LeaderId),

    %% 2. Broadcast: each replica processes the proposal
    assoc_to_keys(Net0.replicas, Ids),
    foldl(replica_on_proposal(Block, LastQC, N), Ids,
          Net0.replicas-[], Replicas1-VotesRev),
    reverse(VotesRev, Votes),

    %% 3. Collect votes into a QC (if quorum)
    (   length(Votes, VoteCount),
        Quorum is (2 * N + 1) // 3,
        VoteCount >= Quorum
    ->  NewQC = qc(Block, Votes)
    ;   NewQC = LastQC
    ),

    %% 4. Advance
    Event = event(Round, LeaderId, Block, Votes, NewQC),
    NextRound is Round + 1,
    NetOut = Net0.put(replicas, Replicas1)
                  .put(round, NextRound)
                  .put(last_qc, NewQC)
                  .put(log, [Event | Net0.log]).

%% replica_on_proposal(+Block, +QC, +N, +Id, +Acc, -AccOut)
%% Fold helper: process proposal on one replica, collect vote if safe.

replica_on_proposal(Block, QC, _N, Id, Replicas0-Votes0, Replicas1-Votes1) :-
    get_assoc(Id, Replicas0, S0),
    on_proposal(Block, QC, S0, S1),
    put_assoc(Id, Replicas0, S1, Replicas1),
    (   S1.last_voted > S0.last_voted
    ->  Votes1 = [Id | Votes0]
    ;   Votes1 = Votes0
    ).

%%%=========================================================================
%%% Faulty round (view change)
%%%=========================================================================

%% skip_round(+NetIn, -NetOut)
%%
%% The leader for this round is faulty — no proposal is made.
%% Replicas timeout and advance to the next round. Each replica sends
%% its qc_high to the next leader. The next leader picks the highest
%% QC among all received and uses it as the justify QC for the next
%% proposal.
%%
%% In HotStuff, this is the "new-view" message flow. We model it as:
%% the network advances round, last_qc becomes the highest qc_high
%% across all replicas.

skip_round(Net0, NetOut) :-
    Round = Net0.round,
    N = Net0.n,
    leader(Round, N, LeaderId),
    %% Collect all replicas' qc_high, pick the highest
    assoc_to_keys(Net0.replicas, Ids),
    maplist(replica_qc_high(Net0.replicas), Ids, QCs),
    highest_qc(QCs, BestQC),
    Event = event(Round, LeaderId, fault, [], BestQC),
    NextRound is Round + 1,
    NetOut = Net0.put(round, NextRound)
                  .put(last_qc, BestQC)
                  .put(log, [Event | Net0.log]),
    !.

replica_qc_high(Replicas, Id, QC) :-
    get_assoc(Id, Replicas, S),
    QC = S.qc_high.

highest_qc([QC], QC).
highest_qc([QC1, QC2 | Rest], Best) :-
    qc_round(QC1, R1),
    qc_round(QC2, R2),
    (R1 >= R2 -> Higher = QC1 ; Higher = QC2),
    highest_qc([Higher | Rest], Best).

%%%=========================================================================
%%% Run multiple rounds
%%%=========================================================================

%% run_rounds(+Commands, +NetIn, -NetOut)
run_rounds([], Net, Net).
run_rounds([Cmd | Cmds], Net0, NetOut) :-
    run_round(Cmd, Net0, Net1),
    run_rounds(Cmds, Net1, NetOut).

%% run_scenario(+Actions, +NetIn, -NetOut)
%% Actions are: cmd(X) for a normal round, or fault for a skipped round.
run_scenario([], Net, Net) :- !.
run_scenario([cmd(Cmd) | Rest], Net0, NetOut) :- !,
    run_round(Cmd, Net0, Net1),
    run_scenario(Rest, Net1, NetOut).
run_scenario([fault | Rest], Net0, NetOut) :-
    skip_round(Net0, Net1),
    run_scenario(Rest, Net1, NetOut).

%%%=========================================================================
%%% Inspection helpers
%%%=========================================================================

%% committed_commands(+Net, +ReplicaId, -Commands)
committed_commands(Net, Id, Cmds) :-
    get_assoc(Id, Net.replicas, S),
    Cmds = S.committed.

%% all_committed(+Net, -Pairs)
%% Returns Id-Commands for each replica.
all_committed(Net, Pairs) :-
    assoc_to_keys(Net.replicas, Ids),
    maplist(committed_pair(Net), Ids, Pairs).

committed_pair(Net, Id, Id-Cmds) :-
    committed_commands(Net, Id, Cmds).

%% print_log(+Net)
print_log(Net) :-
    reverse(Net.log, Chronological),
    maplist(print_event, Chronological).

print_event(event(Round, Leader, block(_, _, Cmd), Votes, _QC)) :-
    length(Votes, NVotes),
    format("Round ~w: leader=~w cmd=~w votes=~w~n",
           [Round, Leader, Cmd, NVotes]).

%%%=========================================================================
%%% Tests
%%%=========================================================================

:- begin_tests(simulation).

%% 4 replicas, happy path: 5 rounds, first command commits after round 4.
test(happy_path_commits, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d, tx_e], Net0, Net),
    committed_commands(Net, 1, Cmds1),
    committed_commands(Net, 2, Cmds2),
    committed_commands(Net, 3, Cmds3),
    committed_commands(Net, 4, Cmds4),
    %% All replicas agree on committed commands
    Cmds1 = Cmds2,
    Cmds2 = Cmds3,
    Cmds3 = Cmds4,
    %% tx_a should be committed (3 consecutive rounds after it)
    member(tx_a, Cmds1).

%% After exactly 3 rounds, nothing is committed yet (need the 4th to trigger).
test(three_rounds_no_commit, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c], Net0, Net),
    committed_commands(Net, 1, []).

%% After 4 rounds, exactly tx_a is committed.
test(four_rounds_one_commit, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d], Net0, Net),
    committed_commands(Net, 1, [tx_a]).

%% Commits accumulate in order: after 6 rounds, tx_a, tx_b, tx_c committed.
test(commits_accumulate, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d, tx_e, tx_f], Net0, Net),
    committed_commands(Net, 1, Cmds),
    %% Committed list is newest-first
    msort(Cmds, Sorted),
    msort([tx_a, tx_b, tx_c], Sorted).

%% Leader rotates: different leaders for each round.
test(leader_rotation) :-
    leader(1, 4, 1),
    leader(2, 4, 2),
    leader(3, 4, 3),
    leader(4, 4, 4),
    leader(5, 4, 1).

%% All replicas agree — the safety property.
test(agreement, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d, tx_e, tx_f, tx_g], Net0, Net),
    all_committed(Net, [_-Expected | Rest]),
    Expected \= [],
    maplist([_-Cmds]>>(Cmds = Expected), Rest).

%% A fault breaks the consecutive-round chain, delaying commits.
test(fault_delays_commit, [true]) :-
    fresh_network(4, Net0),
    %% Normal: cmd, cmd, cmd, cmd -> tx_a committed after round 4
    %% With fault at round 2: rounds 1,3,4,5 aren't consecutive
    run_scenario([cmd(tx_a), fault, cmd(tx_b), cmd(tx_c), cmd(tx_d)], Net0, Net),
    committed_commands(Net, 1, Cmds),
    \+ member(tx_a, Cmds).

%% But recovery after fault still produces commits eventually.
test(recovery_after_fault, [true]) :-
    fresh_network(4, Net0),
    %% Fault at round 2, then 5 good rounds: should eventually commit
    run_scenario([cmd(tx_a), fault, cmd(tx_b), cmd(tx_c), cmd(tx_d),
                  cmd(tx_e), cmd(tx_f), cmd(tx_g)], Net0, Net),
    committed_commands(Net, 1, Cmds),
    Cmds \= [].

%% Agreement holds even with faults.
test(agreement_with_faults, [true]) :-
    fresh_network(4, Net0),
    run_scenario([cmd(tx_a), fault, cmd(tx_b), cmd(tx_c), cmd(tx_d),
                  cmd(tx_e), cmd(tx_f), cmd(tx_g)], Net0, Net),
    all_committed(Net, [_-Expected | Rest]),
    maplist([_-Cmds]>>(Cmds = Expected), Rest).

%% Works with different network sizes (7 replicas, f=2).
test(seven_replicas, [true]) :-
    fresh_network(7, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d, tx_e, tx_f], Net0, Net),
    committed_commands(Net, 1, Cmds),
    member(tx_a, Cmds).

%% Quorum with 7 replicas is 5.
test(quorum_7_replicas) :-
    genesis(G),
    qc_valid(qc(G, [1,2,3,4,5]), 7),
    \+ qc_valid(qc(G, [1,2,3,4]), 7).

:- end_tests(simulation).
