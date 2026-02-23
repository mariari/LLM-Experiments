%% hotstuff.pl — HotStuff BFT consensus in Prolog
%%
%% Based on Yin et al. 2019 "HotStuff: BFT Consensus with Linearity
%% and Responsiveness," Algorithm 6 (Chained HotStuff).
%%
%% HotStuff is a three-phase voting protocol where:
%%   - A leader proposes blocks extending a tree
%%   - Replicas vote if the proposal is safe (extends their locked branch)
%%   - A quorum certificate (QC) over 2f+1 votes advances the phase
%%   - Three consecutive QCs on the same branch trigger a commit
%%
%% We model the protocol relationally:
%%   - Blocks carry their justify QC (the QC the leader used when proposing)
%%   - Replica state is a dict threaded through the protocol
%%   - The locking and commit rules follow the justify-QC chain exactly
%%     as described in the paper's update() function
%%
%% Requires SWI-Prolog 8.0+ (dict syntax, plunit).

:- module(hotstuff, [
    %% Block tree
    genesis/1,
    extends/2,
    block_round/2,
    block_parent/2,
    block_cmd/2,
    block_justify/2,
    ancestor/2,
    conflicting/2,
    %% Quorum certificates
    qc_block/2,
    qc_round/2,
    qc_valid/2,
    genesis_qc/1,
    %% Replica state
    fresh_replica/2,
    %% Safety
    safe_to_vote/3,
    %% Protocol steps
    on_proposal/4,
    update_state/3,
    try_commit/3
]).

:- use_module(library(lists)).

%%%=========================================================================
%%% Blocks
%%%=========================================================================

%% block(Round, Parent, Command, JustifyQC)
%%
%% Each block carries the QC that justifies its proposal. By construction,
%% the parent IS the block certified by the justify QC (ensured in
%% simulation.pl's run_round). This means walking the parent chain is
%% equivalent to walking the justify-QC chain as in the paper's update().

genesis(block(0, none, none, none)).

block_round(block(Round, _, _, _), Round).
block_parent(block(_, Parent, _, _), Parent).
block_cmd(block(_, _, Cmd, _), Cmd).
block_justify(block(_, _, _, JQC), JQC).

%% extends(Block, Parent) — Block's parent is Parent.
extends(block(_, Parent, _, _), Parent) :- Parent \= none.

%% ancestor(Block, Ancestor) — transitive parent chain.
ancestor(Block, Anc) :-
    extends(Block, Anc).
ancestor(Block, Anc) :-
    extends(Block, Mid),
    ancestor(Mid, Anc).

%% conflicting(A, B) — neither is an ancestor of the other.
conflicting(A, B) :-
    A \= B,
    \+ ancestor(A, B),
    \+ ancestor(B, A).

%%%=========================================================================
%%% Quorum Certificates
%%%=========================================================================

%% qc(Block, Votes) — a quorum certificate is a block and the set of
%% replica IDs that voted for it.

qc_block(qc(Block, _), Block).
qc_round(QC, Round) :- qc_block(QC, Block), block_round(Block, Round).

%% qc_valid(+QC, +N) — the QC has enough votes for N replicas.
%% Quorum is 2f+1 where N = 3f+1, so quorum = (2*N + 1) // 3
qc_valid(qc(_, Votes), N) :-
    length(Votes, Count),
    Quorum is (2 * N + 1) // 3,
    Count >= Quorum.

genesis_qc(qc(Genesis, [])) :- genesis(Genesis).

%%%=========================================================================
%%% Replica State
%%%=========================================================================

%% Replica state is a dict:
%%   replica{
%%     id:          ReplicaId,
%%     locked_qc:   QC that replica is locked on,
%%     qc_high:     Highest QC seen,
%%     last_voted:  Last round voted in,
%%     committed:   List of committed commands (newest first)
%%   }

fresh_replica(Id, State) :-
    genesis_qc(GQC),
    State = replica{
        id: Id,
        locked_qc: GQC,
        qc_high: GQC,
        last_voted: 0,
        committed: []
    }.

%%%=========================================================================
%%% Safety — the core rule
%%%=========================================================================

%% safe_to_vote(+Block, +JustifyQC, +State)
%%
%% Succeeds when the replica should vote for Block (proposed with JustifyQC).
%%
%% Two conditions (disjunction — either suffices):
%%   1. Block extends the locked branch (safety)
%%   2. JustifyQC's round > locked QC's round (liveness)
%%
%% Plus: the block's round must be greater than last_voted (no double vote).

safe_to_vote(Block, JustifyQC, State) :-
    block_round(Block, Round),
    Round > State.last_voted,
    qc_block(State.locked_qc, LockedBlock),
    once((
        ancestor(Block, LockedBlock)
    ;   qc_round(JustifyQC, JRound),
        qc_round(State.locked_qc, LRound),
        JRound > LRound
    )).

%%%=========================================================================
%%% Protocol Steps
%%%=========================================================================

%% on_proposal(+Block, +JustifyQC, +StateIn, -StateOut)
%%
%% A replica receives a proposal (Block justified by JustifyQC).
%% It updates qc_high and locked_qc, tries to commit, then votes if safe.

on_proposal(Block, JustifyQC, S0, S3) :-
    update_state(JustifyQC, S0, S1),
    try_commit(JustifyQC, S1, S2),
    (   safe_to_vote(Block, JustifyQC, S2)
    ->  block_round(Block, Round),
        S3 = S2.put(last_voted, Round)
    ;   S3 = S2
    ),
    !.

%% update_state(+JustifyQC, +StateIn, -StateOut)
%%
%% The paper's update() function (Algorithm 6), two steps:
%%
%% Step 1 — update qc_high:
%%   If the justify QC certifies a higher round than qc_high, adopt it.
%%
%% Step 2 — update locked_qc (two-chain lock):
%%   b''  = justify.node
%%   b'   = b''.justify.node (one hop back via the QC chain)
%%   If b'.round > locked_qc.round, lock on b''.justify.
%%
%% Step 2 is a no-op when b'' is genesis (its justify is none, so there
%% is no chain to lock on).

update_state(JustifyQC, S, SOut) :-
    %% Step 1: update qc_high
    qc_round(JustifyQC, JR),
    qc_round(S.qc_high, HighR),
    (JR > HighR -> S1 = S.put(qc_high, JustifyQC) ; S1 = S),
    %% Step 2: update locked_qc via two-chain
    qc_block(JustifyQC, B2),
    block_justify(B2, B2Justify),
    (   B2Justify \= none,
        qc_block(B2Justify, B1),
        block_round(B1, B1Round),
        qc_round(S1.locked_qc, LockedR),
        B1Round > LockedR
    ->  SOut = S1.put(locked_qc, B2Justify)
    ;   SOut = S1
    ).

%% try_commit(+QC, +StateIn, -StateOut)
%%
%% The commit rule: three blocks in a direct parent chain with
%% consecutive rounds trigger a commit. The paper's onCommit(b) means
%% commit b and all its uncommitted ancestors — not just b alone.
%% This matters when a replica missed earlier proposals (e.g. partition).
%%
%% We walk parent links, which is equivalent to walking the justify-QC
%% chain because our blocks enforce parent = justify.node by construction.

try_commit(QC, S, SOut) :-
    qc_block(QC, B2),
    (   extends(B2, B1),
        extends(B1, B0),
        block_round(B2, R2),
        block_round(B1, R1),
        block_round(B0, R0),
        R2 =:= R1 + 1,
        R1 =:= R0 + 1
    ->  commit_up_to(B0, S, SOut)
    ;   SOut = S
    ).

%% commit_up_to(+Block, +StateIn, -StateOut)
%% Commit all uncommitted commands from genesis up to Block.
%% Walks the parent chain, collects new commands (newest-first),
%% then prepends them to the committed list.
commit_up_to(Block, S, SOut) :-
    uncommitted_cmds(Block, S.committed, NewCmds),
    append(NewCmds, S.committed, AllCommitted),
    SOut = S.put(committed, AllCommitted).

uncommitted_cmds(Block, Already, Cmds) :-
    (   extends(Block, Parent)
    ->  uncommitted_cmds(Parent, Already, AncCmds)
    ;   AncCmds = []
    ),
    block_cmd(Block, Cmd),
    (   Cmd \= none, \+ member(Cmd, Already), \+ member(Cmd, AncCmds)
    ->  Cmds = [Cmd | AncCmds]
    ;   Cmds = AncCmds
    ).

%%%=========================================================================
%%% Tests
%%%=========================================================================

:- begin_tests(hotstuff).

test(genesis_is_root) :-
    genesis(G),
    block_round(G, 0),
    \+ extends(G, _).

test(block_tree) :-
    genesis(G),
    genesis_qc(GQC),
    B1 = block(1, G, tx_a, GQC),
    QC1 = qc(B1, [r1, r2, r3]),
    B2 = block(2, B1, tx_b, QC1),
    extends(B2, B1),
    extends(B1, G),
    once(ancestor(B2, G)).

test(quorum_4_replicas) :-
    genesis(G),
    qc_valid(qc(G, [r1, r2, r3]), 4),
    \+ qc_valid(qc(G, [r1, r2]), 4).

test(fresh_replica_votes, [true]) :-
    fresh_replica(r1, S),
    genesis(G),
    genesis_qc(GQC),
    B1 = block(1, G, tx_a, GQC),
    safe_to_vote(B1, GQC, S).

test(no_double_vote, [true]) :-
    fresh_replica(r1, S0),
    genesis(G),
    genesis_qc(GQC),
    B1 = block(1, G, tx_a, GQC),
    safe_to_vote(B1, GQC, S0),
    S1 = S0.put(last_voted, 1),
    B1b = block(1, G, tx_b, GQC),
    \+ safe_to_vote(B1b, GQC, S1).

test(commit_after_three_rounds, [true]) :-
    fresh_replica(r1, S0),
    genesis(G),
    genesis_qc(GQC),
    B1 = block(1, G, tx_a, GQC),
    QC1 = qc(B1, [r1, r2, r3]),
    B2 = block(2, B1, tx_b, QC1),
    QC2 = qc(B2, [r1, r2, r3]),
    B3 = block(3, B2, tx_c, QC2),
    QC3 = qc(B3, [r1, r2, r3]),
    on_proposal(B1, GQC, S0, S1),
    on_proposal(B2, QC1, S1, S2),
    on_proposal(B3, QC2, S2, S3),
    B4 = block(4, B3, tx_d, QC3),
    on_proposal(B4, QC3, S3, S4),
    member(tx_a, S4.committed).

test(conflicting_blocks) :-
    genesis(G),
    genesis_qc(GQC),
    B1 = block(1, G, tx_a, GQC),
    B1b = block(1, G, tx_b, GQC),
    conflicting(B1, B1b).

%% After a two-chain, a fork with a stale justify QC is rejected.
test(locking_rejects_conflicting_branch, [true]) :-
    fresh_replica(r1, S0),
    genesis(G),
    genesis_qc(GQC),
    B1 = block(1, G, tx_a, GQC),
    QC1 = qc(B1, [r1, r2, r3]),
    B2 = block(2, B1, tx_b, QC1),
    QC2 = qc(B2, [r1, r2, r3]),
    B3 = block(3, B2, tx_c, QC2),
    on_proposal(B1, GQC, S0, S1),
    on_proposal(B2, QC1, S1, S2),
    on_proposal(B3, QC2, S2, S3),
    B_fork = block(4, G, tx_evil, GQC),
    \+ safe_to_vote(B_fork, GQC, S3).

:- end_tests(hotstuff).
