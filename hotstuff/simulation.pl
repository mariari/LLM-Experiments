%% simulation.pl — Multi-replica HotStuff simulation
%%
%% Runs the HotStuff protocol across N replicas with a round-robin leader.
%%
%% The simulation loop:
%%   1. Leader for the round proposes a block
%%   2. All replicas process the proposal and vote (or don't)
%%   3. Votes are collected; if quorum is reached, a QC forms
%%   4. Next round begins with the QC passed to the new leader
%%
%% Supports happy path, faulty leaders (skipped rounds / view change),
%% network partitions, and step-by-step tracing for exploration.
%%
%% Example-driven: load this file in swipl and call example/1 to explore.
%%   ?- example(happy_path).
%%   ?- example(fork_attempt).
%%   ?- example(liveness).
%%   ?- example(partition).
%%   ?- example(view_change).

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

leader(Round, N, Leader) :-
    Leader is ((Round - 1) mod N) + 1.

%%%=========================================================================
%%% Simulation step
%%%=========================================================================

%% run_round(+Command, +NetIn, -NetOut)
%% All replicas active.
run_round(Command, Net0, NetOut) :-
    run_round(Command, [], Net0, NetOut).

%% run_round(+Command, +Excluded, +NetIn, -NetOut)
%% Excluded replicas don't receive the proposal (partition / crash).
run_round(Command, Excluded, Net0, NetOut) :-
    Round = Net0.round,
    N = Net0.n,
    LastQC = Net0.last_qc,
    qc_block(LastQC, ParentBlock),

    Block = block(Round, ParentBlock, Command, LastQC),
    leader(Round, N, LeaderId),

    assoc_to_keys(Net0.replicas, AllIds),
    subtract(AllIds, Excluded, ActiveIds),
    foldl(replica_on_proposal(Block, LastQC), ActiveIds,
          Net0.replicas-[], Replicas1-VotesRev),
    reverse(VotesRev, Votes),

    (   length(Votes, VoteCount),
        Quorum is (2 * N + 1) // 3,
        VoteCount >= Quorum
    ->  NewQC = qc(Block, Votes)
    ;   NewQC = LastQC
    ),

    Event = event(Round, LeaderId, Block, Votes, NewQC),
    NextRound is Round + 1,
    NetOut = Net0.put(replicas, Replicas1)
                  .put(round, NextRound)
                  .put(last_qc, NewQC)
                  .put(log, [Event | Net0.log]),
    !.

replica_on_proposal(Block, QC, Id, Replicas0-Votes0, Replicas1-Votes1) :-
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
%% The leader is faulty — no proposal. Replicas timeout and send their
%% qc_high to the next leader, who picks the highest.
%%
%% Note: QCs formed in the previous round but not yet delivered as a
%% justify QC are lost. This is correct per the paper — replicas only
%% learn about QCs through proposals.

skip_round(Net0, NetOut) :-
    Round = Net0.round,
    N = Net0.n,
    leader(Round, N, LeaderId),
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

run_rounds([], Net, Net).
run_rounds([Cmd | Cmds], Net0, NetOut) :-
    run_round(Cmd, Net0, Net1),
    run_rounds(Cmds, Net1, NetOut).

%% run_scenario(+Actions, +NetIn, -NetOut)
%% Actions: cmd(X) for a normal round, fault for a skipped round.
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

committed_commands(Net, Id, Cmds) :-
    get_assoc(Id, Net.replicas, S),
    Cmds = S.committed.

all_committed(Net, Pairs) :-
    assoc_to_keys(Net.replicas, Ids),
    maplist(committed_pair(Net), Ids, Pairs).

committed_pair(Net, Id, Id-Cmds) :-
    committed_commands(Net, Id, Cmds).

%% show_replica(+Net, +Id) — print one replica's state.
show_replica(Net, Id) :-
    get_assoc(Id, Net.replicas, S),
    qc_round(S.qc_high, QHR),
    qc_round(S.locked_qc, LR),
    reverse(S.committed, Chron),
    format("Replica ~w:~n", [Id]),
    format("  last_voted: ~w~n", [S.last_voted]),
    format("  qc_high:    round ~w~n", [QHR]),
    format("  locked_qc:  round ~w~n", [LR]),
    format("  committed:  ~w~n", [Chron]).

%% show_network(+Net) — print all replicas.
show_network(Net) :-
    assoc_to_keys(Net.replicas, Ids),
    format("--- Network (round ~w, ~w replicas) ---~n", [Net.round, Net.n]),
    maplist(show_replica(Net), Ids),
    format("---~n").

%%%=========================================================================
%%% Tracing — step-by-step round execution with output
%%%=========================================================================

%% trace_round(+Command, +NetIn, -NetOut)
trace_round(Command, Net0, NetOut) :-
    trace_round(Command, [], Net0, NetOut).

%% trace_round(+Command, +Excluded, +NetIn, -NetOut)
trace_round(Command, Excluded, Net0, NetOut) :-
    Round = Net0.round,
    N = Net0.n,
    leader(Round, N, LeaderId),
    (   Excluded = []
    ->  format("~n== Round ~w (leader=~w, cmd=~w) ==~n",
               [Round, LeaderId, Command])
    ;   format("~n== Round ~w (leader=~w, cmd=~w, partitioned=~w) ==~n",
               [Round, LeaderId, Command, Excluded])
    ),
    run_round(Command, Excluded, Net0, NetOut),
    NetOut.log = [event(_, _, _, Votes, NewQC) | _],
    length(Votes, NVotes),
    format("   votes: ~w (~w/~w)~n", [Votes, NVotes, N]),
    Quorum is (2 * N + 1) // 3,
    (   NVotes >= Quorum
    ->  qc_round(NewQC, QCR),
        format("   QC formed for round ~w~n", [QCR])
    ;   format("   no QC (insufficient votes)~n")
    ),
    all_committed(NetOut, Pairs),
    include([_-C]>>(C \= []), Pairs, NonEmpty),
    (   NonEmpty = [_-Cmds | _]
    ->  reverse(Cmds, Chron),
        format("   committed so far: ~w~n", [Chron])
    ;   true
    ).

%% trace_fault(+NetIn, -NetOut)
trace_fault(Net0, NetOut) :-
    Round = Net0.round,
    Net0.n = N,
    leader(Round, N, LeaderId),
    format("~n== Round ~w (leader=~w, FAULT) ==~n", [Round, LeaderId]),
    skip_round(Net0, NetOut),
    NetOut.log = [event(_, _, _, _, BestQC) | _],
    qc_round(BestQC, BQR),
    format("   view change: best QC at round ~w~n", [BQR]).

%% trace_scenario(+Actions, +NetIn, -NetOut)
trace_scenario([], Net, Net) :- !.
trace_scenario([cmd(Cmd) | Rest], Net0, NetOut) :- !,
    trace_round(Cmd, Net0, Net1),
    trace_scenario(Rest, Net1, NetOut).
trace_scenario([fault | Rest], Net0, NetOut) :-
    trace_fault(Net0, Net1),
    trace_scenario(Rest, Net1, NetOut).

%%%=========================================================================
%%% Print helpers
%%%=========================================================================

print_log(Net) :-
    reverse(Net.log, Chronological),
    maplist(print_event, Chronological).

print_event(event(Round, Leader, fault, _, _QC)) :- !,
    format("Round ~w: leader=~w FAULT (no proposal)~n", [Round, Leader]).
print_event(event(Round, Leader, block(_, _, Cmd, _), Votes, _QC)) :-
    length(Votes, NVotes),
    format("Round ~w: leader=~w cmd=~w votes=~w~n",
           [Round, Leader, Cmd, NVotes]).

print_committed(Net) :-
    all_committed(Net, Pairs),
    maplist(print_replica_committed, Pairs).

print_replica_committed(Id-Cmds) :-
    reverse(Cmds, Chronological),
    format("  Replica ~w committed: ~w~n", [Id, Chronological]).

%%%=========================================================================
%%% Examples — call example/2 in the REPL to explore and chain
%%%=========================================================================

%% example(+Name, -Net) — run a named scenario, return final network.
%%
%%   ?- example(happy_path, Net), trace_round(tx_g, Net, Net2).
%%   ?- example(partition, Net), show_replica(Net, 4).
%%   ?- example(fork_attempt, Net), trace_round(tx_d, Net, Net2),
%%      show_network(Net2).

example(Name) :- example(Name, _).

example(happy_path, Net) :-
    format("~n=== Happy Path: 4 replicas, 6 honest rounds ===~n"),
    format("Expect: tx_a committed after round 4, tx_b after 5, tx_c after 6~n"),
    fresh_network(4, N0),
    trace_scenario([cmd(tx_a), cmd(tx_b), cmd(tx_c),
                    cmd(tx_d), cmd(tx_e), cmd(tx_f)], N0, Net),
    format("~n--- Final state ---~n"),
    show_network(Net).

example(fork_attempt, Net) :-
    format("~n=== Fork Attempt: safety rejects conflicting branch ===~n"),
    format("Build a 3-round chain, then try to fork from genesis.~n"),
    format("The replicas are locked — the fork's low justify QC is rejected.~n"),
    fresh_network(4, N0),
    trace_scenario([cmd(tx_a), cmd(tx_b), cmd(tx_c)], N0, Net),
    get_assoc(1, Net.replicas, S),
    qc_round(S.locked_qc, LR),
    qc_round(S.qc_high, HR),
    format("~nReplica 1 state:~n"),
    format("  locked_qc round: ~w~n", [LR]),
    format("  qc_high round:   ~w~n", [HR]),
    format("  last_voted:      ~w~n~n", [S.last_voted]),
    genesis(G),
    genesis_qc(GQC),
    B_fork = block(4, G, tx_evil, GQC),
    format("Fork block: round 4, parent=genesis, justify=genesis_qc (round 0)~n"),
    (   safe_to_vote(B_fork, GQC, S)
    ->  format("  VOTED (unexpected!)~n")
    ;   format("  REJECTED — safety holds!~n"),
        format("  Reason: fork doesn't extend locked branch,~n"),
        format("          and justify round 0 <= locked round ~w~n", [LR])
    ).

example(liveness, Net) :-
    format("~n=== Liveness: higher justify QC unlocks a stuck replica ===~n"),
    format("Run 2 rounds, then show that a proposal with a higher justify~n"),
    format("QC overrides the lock even on a different branch.~n"),
    fresh_network(4, N0),
    trace_scenario([cmd(tx_a), cmd(tx_b)], N0, Net),
    get_assoc(1, Net.replicas, S),
    qc_round(S.locked_qc, LR),
    format("~nReplica 1: locked_qc round = ~w, last_voted = ~w~n", [LR, S.last_voted]),
    Net.last_qc = LastQC,
    qc_block(LastQC, B2),
    B3 = block(3, B2, tx_c, LastQC),
    QC3 = qc(B3, [2, 3, 4]),
    B5 = block(5, B3, tx_d, QC3),
    format("Hypothetical proposal: B5 at round 5, justify QC at round 3~n"),
    format("  locked_qc round: ~w~n", [LR]),
    (   safe_to_vote(B5, QC3, S)
    ->  format("  VOTED — liveness: justify round 3 > locked round ~w~n", [LR])
    ;   format("  REJECTED (unexpected!)~n")
    ).

example(partition, Net) :-
    format("~n=== Partition: one replica cut off, others commit ===~n"),
    format("Replica 4 is partitioned. 3 of 4 vote — still a quorum.~n"),
    format("Replica 4 sees nothing and commits nothing.~n"),
    fresh_network(4, N0),
    trace_round(tx_a, [4], N0, N1),
    trace_round(tx_b, [4], N1, N2),
    trace_round(tx_c, [4], N2, N3),
    trace_round(tx_d, [4], N3, N4),
    trace_round(tx_e, [4], N4, Net),
    format("~n--- Final state ---~n"),
    show_network(Net).

example(view_change, Net) :-
    format("~n=== View Change: faulty leader, then recovery ===~n"),
    format("Round 1 is honest, round 2 leader is faulty (no proposal),~n"),
    format("rounds 3-8 are honest. The fault breaks the consecutive chain~n"),
    format("so commit is delayed, but eventually resumes.~n"),
    fresh_network(4, N0),
    trace_scenario([cmd(tx_a), fault, cmd(tx_b), cmd(tx_c),
                    cmd(tx_d), cmd(tx_e), cmd(tx_f), cmd(tx_g)], N0, Net),
    format("~n--- Final state ---~n"),
    show_network(Net).

%%%=========================================================================
%%% Demo
%%%=========================================================================

demo :-
    example(happy_path),
    example(view_change),
    example(partition),
    example(fork_attempt),
    example(liveness).

%%%=========================================================================
%%% Tests
%%%=========================================================================

:- begin_tests(simulation).

test(happy_path_commits, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d, tx_e], Net0, Net),
    committed_commands(Net, 1, Cmds1),
    committed_commands(Net, 2, Cmds2),
    committed_commands(Net, 3, Cmds3),
    committed_commands(Net, 4, Cmds4),
    Cmds1 = Cmds2, Cmds2 = Cmds3, Cmds3 = Cmds4,
    member(tx_a, Cmds1).

test(three_rounds_no_commit, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c], Net0, Net),
    committed_commands(Net, 1, []).

test(four_rounds_one_commit, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d], Net0, Net),
    committed_commands(Net, 1, [tx_a]).

test(commits_accumulate, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d, tx_e, tx_f], Net0, Net),
    committed_commands(Net, 1, Cmds),
    msort(Cmds, Sorted),
    msort([tx_a, tx_b, tx_c], Sorted).

test(leader_rotation) :-
    leader(1, 4, 1),
    leader(2, 4, 2),
    leader(3, 4, 3),
    leader(4, 4, 4),
    leader(5, 4, 1).

test(agreement, [true]) :-
    fresh_network(4, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d, tx_e, tx_f, tx_g], Net0, Net),
    all_committed(Net, [_-Expected | Rest]),
    Expected \= [],
    maplist([_-Cmds]>>(Cmds = Expected), Rest).

test(fault_delays_commit, [true]) :-
    fresh_network(4, Net0),
    run_scenario([cmd(tx_a), fault, cmd(tx_b), cmd(tx_c), cmd(tx_d)], Net0, Net),
    committed_commands(Net, 1, Cmds),
    \+ member(tx_a, Cmds).

test(recovery_after_fault, [true]) :-
    fresh_network(4, Net0),
    run_scenario([cmd(tx_a), fault, cmd(tx_b), cmd(tx_c), cmd(tx_d),
                  cmd(tx_e), cmd(tx_f), cmd(tx_g)], Net0, Net),
    committed_commands(Net, 1, Cmds),
    Cmds \= [].

test(agreement_with_faults, [true]) :-
    fresh_network(4, Net0),
    run_scenario([cmd(tx_a), fault, cmd(tx_b), cmd(tx_c), cmd(tx_d),
                  cmd(tx_e), cmd(tx_f), cmd(tx_g)], Net0, Net),
    all_committed(Net, [_-Expected | Rest]),
    maplist([_-Cmds]>>(Cmds = Expected), Rest).

test(partition_one_replica, [true]) :-
    fresh_network(4, Net0),
    run_round(tx_a, [4], Net0, N1),
    run_round(tx_b, [4], N1, N2),
    run_round(tx_c, [4], N2, N3),
    run_round(tx_d, [4], N3, N4),
    run_round(tx_e, [4], N4, Net),
    committed_commands(Net, 1, C1),
    committed_commands(Net, 2, C2),
    committed_commands(Net, 3, C3),
    C1 = C2, C2 = C3,
    member(tx_a, C1),
    committed_commands(Net, 4, []).

test(no_quorum_no_commit, [true]) :-
    fresh_network(4, Net0),
    run_round(tx_a, [3, 4], Net0, N1),
    run_round(tx_b, [3, 4], N1, N2),
    run_round(tx_c, [3, 4], N2, N3),
    run_round(tx_d, [3, 4], N3, Net),
    committed_commands(Net, 1, []).

test(seven_replicas, [true]) :-
    fresh_network(7, Net0),
    run_rounds([tx_a, tx_b, tx_c, tx_d, tx_e, tx_f], Net0, Net),
    committed_commands(Net, 1, Cmds),
    member(tx_a, Cmds).

test(quorum_7_replicas) :-
    genesis(G),
    qc_valid(qc(G, [1,2,3,4,5]), 7),
    \+ qc_valid(qc(G, [1,2,3,4]), 7).

:- end_tests(simulation).
