%% hotstuff.pl — HotStuff BFT consensus in Prolog
%%
%% Based on Yin et al. 2019 "HotStuff: BFT Consensus with Linearity
%% and Responsiveness."
%%
%% HotStuff is a three-phase voting protocol where:
%%   - A leader proposes blocks extending a tree
%%   - Replicas vote if the proposal is safe (extends their locked branch)
%%   - A quorum certificate (QC) over 2f+1 votes advances the phase
%%   - Three consecutive QCs on the same branch trigger a commit
%%
%% The core insight: one message type (vote) and one rule (safe-to-vote)
%% give you both safety and liveness. Everything else is plumbing.
%%
%% We model the protocol relationally:
%%   - Blocks, QCs, and votes are terms (not mutable state)
%%   - Replica state is a dict threaded through the protocol
%%   - Quorum checks are pure predicates over vote sets
%%
%% Requires SWI-Prolog 8.0+ (dict syntax, plunit).

:- module(hotstuff, [
    %% Block tree
    genesis/1,
    extends/2,
    block_round/2,
    block_cmd/2,
    ancestor/2,
    conflicting/2,
    %% Quorum certificates
    qc/2,
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
    update_qc_high/3,
    try_commit/3
]).

:- use_module(library(lists)).
:- use_module(library(apply)).

%%%=========================================================================
%%% Blocks
%%%=========================================================================

%% block(Round, Parent, Command)
%% A block is a round number, a parent block, and a command payload.
%% The genesis block is the root — no parent, no command.

genesis(block(0, none, none)).

block_round(block(Round, _, _), Round).

block_parent(block(_, Parent, _), Parent).

block_cmd(block(_, _, Cmd), Cmd).

%% extends(Block, Parent) — Block's parent is Parent.
extends(block(_, Parent, _), Parent) :- Parent \= none.

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
%% replica IDs that voted for it. Valid when |Votes| >= quorum_size.

qc(Block, Votes) :-
    is_list(Votes),
    (Block = none ; block_round(Block, _)),
    !.

qc_block(qc(Block, _), Block).
qc_round(qc(Block, _), Round) :-
    (Block = none -> Round = 0 ; block_round(Block, Round)).

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
%%     round:       CurrentRound,
%%     locked_qc:   QC that replica is locked on,
%%     qc_high:     Highest QC seen,
%%     last_voted:  Last round voted in,
%%     committed:   List of committed commands (newest first)
%%   }

fresh_replica(Id, State) :-
    genesis_qc(GQC),
    State = replica{
        id: Id,
        round: 0,
        locked_qc: GQC,
        qc_high: GQC,
        last_voted: 0,
        committed: []
    }.

%%%=========================================================================
%%% Safety — the core rule
%%%=========================================================================

%% safe_to_vote(+Block, +State, +N) succeeds when the replica should vote.
%%
%% Two conditions (disjunction — either suffices):
%%   1. Block extends the locked branch (safety)
%%   2. Block's justify QC is higher than the locked QC (liveness)
%%
%% Plus: the block's round must be greater than last_voted (no double vote).

safe_to_vote(Block, State, _N) :-
    block_round(Block, Round),
    Round > State.last_voted,
    State.locked_qc = qc(LockedBlock, _),
    once((
        LockedBlock = none
    ;   ancestor(Block, LockedBlock)
    ;   qc_round(State.qc_high, HighRound),
        Round > HighRound
    )).

%%%=========================================================================
%%% Protocol Steps
%%%=========================================================================

%% on_proposal(+Block, +JustifyQC, +StateIn, -StateOut)
%%
%% A replica receives a proposal (Block justified by JustifyQC).
%% If safe, it votes (StateOut has updated last_voted).
%% It also updates qc_high and tries to commit.

on_proposal(Block, JustifyQC, S0, S3) :-
    N = 4,  % TODO: parameterize
    update_qc_high(JustifyQC, S0, S1),
    try_commit(JustifyQC, S1, S2),
    (   safe_to_vote(Block, S2, N)
    ->  block_round(Block, Round),
        S3 = S2.put(last_voted, Round)
    ;   S3 = S2
    ),
    !.

%% update_qc_high(+QC, +StateIn, -StateOut)
%% Update qc_high if the new QC is for a higher round.

update_qc_high(QC, S, SOut) :-
    qc_round(QC, R),
    qc_round(S.qc_high, HighR),
    (   R > HighR
    ->  SOut = S.put(qc_high, QC).put(locked_qc, S.qc_high)
    ;   SOut = S
    ).

%% try_commit(+QC, +StateIn, -StateOut)
%%
%% The commit rule: if we see a chain of 3 consecutive rounds
%% (b'' <- b' <- b where rounds are consecutive), commit b's command.
%%
%% In practice: QC justifies b'', whose parent b' has a QC (qc_high)
%% justifying b, and if b''.round = b'.round + 1 = b.round + 2, commit b.

try_commit(QC, S, SOut) :-
    qc_block(QC, B2),                     % b''
    (   B2 \= none,
        extends(B2, B1),                   % b' = parent of b''
        B1 \= none,
        extends(B1, B0),                   % b  = parent of b'
        block_round(B2, R2),
        block_round(B1, R1),
        block_round(B0, R0),
        R2 =:= R1 + 1,
        R1 =:= R0 + 1,
        block_cmd(B0, Cmd),
        Cmd \= none
    ->  SOut = S.put(committed, [Cmd | S.committed])
    ;   SOut = S
    ).

%%%=========================================================================
%%% Tests — each reveals a distinct property
%%%=========================================================================

:- begin_tests(hotstuff).

%% The block tree forms correctly.
test(genesis_is_root) :-
    genesis(G),
    block_round(G, 0),
    \+ extends(G, _).

test(block_tree) :-
    genesis(G),
    B1 = block(1, G, tx_a),
    B2 = block(2, B1, tx_b),
    extends(B2, B1),
    extends(B1, G),
    once(ancestor(B2, G)).

%% Quorum arithmetic: 4 replicas need 3 votes.
test(quorum_4_replicas) :-
    genesis(G),
    qc_valid(qc(G, [r1, r2, r3]), 4),
    \+ qc_valid(qc(G, [r1, r2]), 4).

%% A fresh replica votes for any block extending genesis.
test(fresh_replica_votes, [true]) :-
    fresh_replica(r1, S),
    genesis(G),
    B1 = block(1, G, tx_a),
    safe_to_vote(B1, S, 4).

%% A replica won't double-vote in the same round.
test(no_double_vote, [true]) :-
    fresh_replica(r1, S0),
    genesis(G),
    B1 = block(1, G, tx_a),
    safe_to_vote(B1, S0, 4),
    S1 = S0.put(last_voted, 1),
    B1b = block(1, G, tx_b),
    \+ safe_to_vote(B1b, S1, 4).

%% Three consecutive rounds trigger a commit.
test(commit_after_three_rounds, [true]) :-
    fresh_replica(r1, S0),
    genesis(G),
    B1 = block(1, G, tx_a),
    B2 = block(2, B1, tx_b),
    B3 = block(3, B2, tx_c),
    QC1 = qc(B1, [r1, r2, r3]),
    QC2 = qc(B2, [r1, r2, r3]),
    QC3 = qc(B3, [r1, r2, r3]),
    on_proposal(B1, qc(G, []), S0, S1),
    on_proposal(B2, QC1, S1, S2),
    on_proposal(B3, QC2, S2, S3),
    %% B3 justified by QC2: chain is B3<-B2<-B1, rounds 3,2,1
    %% so B1's command (tx_a) should be committed when we process
    %% a block justified by QC3
    B4 = block(4, B3, tx_d),
    on_proposal(B4, QC3, S3, S4),
    member(tx_a, S4.committed).

%% Conflicting blocks are detected.
test(conflicting_blocks) :-
    genesis(G),
    B1 = block(1, G, tx_a),
    B1b = block(1, G, tx_b),
    conflicting(B1, B1b).

:- end_tests(hotstuff).
