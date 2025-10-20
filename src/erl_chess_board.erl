%%% @doc Minimal chess board with move legality including check detection
%%
-module(erl_chess_board).
-export([
    new/0,
    from_fen/1,
    to_fen/1,
    move/3,
    move/4,
    legal_moves/2,
    legal_moves_with_specials/2,
    in_check/2
]).

-define(START_FEN, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").

%% -------------------------
%% Public API
%% -------------------------

new() ->
    from_fen(?START_FEN).

from_fen(Fen) when is_list(Fen) ->
    Parts = string:tokens(Fen, " "),
    case Parts of
        [BoardStr, TurnStr, CastlingStr, EpStr, HalfStr, FullStr] ->
            #{
                <<"board">> => parse_board(BoardStr),
                <<"turn">> =>
                    case TurnStr of
                        "w" -> white;
                        "b" -> black
                    end,
                <<"castling">> => parse_castling(CastlingStr),
                <<"ep">> => parse_ep(EpStr),
                <<"halfmove">> => list_to_integer(HalfStr),
                <<"fullmove">> => list_to_integer(FullStr)
            };
        _ ->
            error
    end.

to_fen(State) ->
    Board = maps:get(<<"board">>, State),
    BoardStr = serialize_board(Board),
    TurnStr =
        case maps:get(<<"turn">>, State) of
            white -> "w";
            black -> "b"
        end,
    CastlingStr = serialize_castling(maps:get(<<"castling">>, State)),
    EpStr = serialize_ep(maps:get(<<"ep">>, State)),
    HalfStr = integer_to_list(maps:get(<<"halfmove">>, State)),
    FullStr = integer_to_list(maps:get(<<"fullmove">>, State)),
    string:join([BoardStr, TurnStr, CastlingStr, EpStr, HalfStr, FullStr], " ").

move(State, From, To) ->
    %% Fallback wrapper that uses default auto-queen promotion
    move(State, From, To, fun(_Color) -> queen end).

move(State, From, To, PromoteFun) ->
    Board0 = maps:get(<<"board">>, State),
    case maps:find(From, Board0) of
        error ->
            {error, no_piece};
        {ok, Piece = {Color, _Kind}} ->
            case maps:get(<<"turn">>, State) of
                Color ->
                    Moves = legal_moves_with_specials(State, From),
                    case lists:member(To, Moves) of
                        false ->
                            {error, illegal_move};
                        true ->
                            {SimBoard, SimState_after_move} =
                                simulate_apply_move(
                                    State, From, To, Piece, PromoteFun
                                ),
                            case in_check(SimBoard, Color) of
                                true ->
                                    {error, would_leave_king_in_check};
                                false ->
                                    State1 = update_castling_rights(
                                        SimState_after_move, From, To
                                    ),
                                    State2 = maps:put(
                                        <<"board">>, SimBoard, State1
                                    ),
                                    State3 = maps:put(
                                        <<"turn">>, other(Color), State2
                                    ),
                                    State4 = maps:put(
                                        <<"halfmove">>,
                                        maps:get(<<"halfmove">>, State1) + 1,
                                        State3
                                    ),
                                    State5 =
                                        case Color of
                                            black ->
                                                maps:put(
                                                    <<"fullmove">>,
                                                    maps:get(
                                                        <<"fullmove">>, State1
                                                    ) + 1,
                                                    State4
                                                );
                                            _ ->
                                                State4
                                        end,
                                    State6 = update_ep(State5, From, To, Piece),
                                    State6
                            end
                    end;
                _ ->
                    {error, not_your_turn}
            end
    end.

%% -------------------------
%% Legal-move generation (piece motion rules only)
%% -------------------------

legal_moves_with_specials(State, From) ->
    Board = maps:get(<<"board">>, State),
    case maps:get(From, Board, none) of
        none ->
            [];
        {Color, Kind} ->
            Normal = legal_moves(Board, From),
            EP = maps:get(<<"ep">>, State, none),
            EpMoves =
                case {Kind, EP} of
                    {pawn, {Ef, Er}} when abs(Ef - element(1, From)) =:= 1 ->
                        %% ensure pawn is on correct rank to capture EP
                        case Color of
                            white when element(2, From) =:= 5 -> [{Ef, Er}];
                            black when element(2, From) =:= 4 -> [{Ef, Er}];
                            _ -> []
                        end;
                    _ ->
                        []
                end,
            Extra =
                case Kind of
                    king -> castle_moves(State, Color);
                    _ -> []
                end,
            %% include ep captures if any
            Normal ++ EpMoves ++ Extra
    end.

legal_moves(Board, {F, R}) ->
    case maps:get({F, R}, Board, none) of
        none ->
            [];
        {Color, Kind} ->
            case Kind of
                pawn ->
                    pawn_moves(Board, {F, R}, Color);
                knight ->
                    knight_moves(Board, {F, R}, Color);
                bishop ->
                    slide_moves(Board, {F, R}, Color, [
                        {1, 1}, {1, -1}, {-1, 1}, {-1, -1}
                    ]);
                rook ->
                    slide_moves(Board, {F, R}, Color, [
                        {1, 0}, {-1, 0}, {0, 1}, {0, -1}
                    ]);
                queen ->
                    slide_moves(Board, {F, R}, Color, [
                        {1, 1},
                        {1, -1},
                        {-1, 1},
                        {-1, -1},
                        {1, 0},
                        {-1, 0},
                        {0, 1},
                        {0, -1}
                    ]);
                king ->
                    king_moves(Board, {F, R}, Color)
            end
    end.

inside({F, R}) ->
    F >= 1 andalso F =< 8 andalso R >= 1 andalso R =< 8.

occupied_by(Board, {F, R}) ->
    case maps:get({F, R}, Board, none) of
        none -> none;
        {C, _} -> C
    end.

%% Pawn moves (non-EP captures)
pawn_moves(Board, {F, R}, white) ->
    Step1 = {F, R + 1},
    Moves =
        case occupied_by(Board, Step1) of
            none ->
                Extra =
                    case R of
                        2 ->
                            case occupied_by(Board, {F, R + 2}) of
                                none -> [{F, R + 2}];
                                _ -> []
                            end;
                        _ ->
                            []
                    end,
                [Step1 | Extra];
            _ ->
                []
        end,
    Captures = [
        {F + Dx, R + 1}
     || Dx <- [-1, 1],
        inside({F + Dx, R + 1}),
        occupied_by(Board, {F + Dx, R + 1}) =:= black
    ],
    Moves ++ Captures;
pawn_moves(Board, {F, R}, black) ->
    Step1 = {F, R - 1},
    Moves =
        case occupied_by(Board, Step1) of
            none ->
                Extra =
                    case R of
                        7 ->
                            case occupied_by(Board, {F, R - 2}) of
                                none -> [{F, R - 2}];
                                _ -> []
                            end;
                        _ ->
                            []
                    end,
                [Step1 | Extra];
            _ ->
                []
        end,
    Captures = [
        {F + Dx, R - 1}
     || Dx <- [-1, 1],
        inside({F + Dx, R - 1}),
        occupied_by(Board, {F + Dx, R - 1}) =:= white
    ],
    Moves ++ Captures.

knight_moves(Board, {F, R}, Color) ->
    D = [
        {1, 2}, {2, 1}, {2, -1}, {1, -2}, {-1, -2}, {-2, -1}, {-2, 1}, {-1, 2}
    ],
    [
        {F + Df, R + Dr}
     || {Df, Dr} <- D,
        inside({F + Df, R + Dr}),
        case occupied_by(Board, {F + Df, R + Dr}) of
            none -> true;
            Other -> Other =/= Color
        end
    ].

king_moves(Board, {F, R}, Color) ->
    [
        {F + Df, R + Dr}
     || Df <- [-1, 0, 1],
        Dr <- [-1, 0, 1],
        not (Df =:= 0 andalso Dr =:= 0),
        inside({F + Df, R + Dr}),
        case occupied_by(Board, {F + Df, R + Dr}) of
            none -> true;
            Other -> Other =/= Color
        end
    ].

slide_moves(Board, {F, R}, Color, Dirs) ->
    lists:flatten([slide_dir(Board, {F, R}, Color, D) || D <- Dirs]).

slide_dir(Board, {F, R}, Color, {Df, Dr}) ->
    Next = {F + Df, R + Dr},
    case inside(Next) of
        false ->
            [];
        true ->
            case maps:get(Next, Board, none) of
                none -> [Next | slide_dir(Board, Next, Color, {Df, Dr})];
                {C, _} when C =:= Color -> [];
                {_C, _} -> [Next]
            end
    end.

%% -------------------------
%% Castling (also filtered by check)
%% -------------------------

castle_moves(State, Color) ->
    Castling = maps:get(<<"castling">>, State),
    Board = maps:get(<<"board">>, State),
    Turn = maps:get(<<"turn">>, State),
    %% king starting pos depends on color (standard)
    case Color of
        white ->
            KingPos = {5, 1},
            case Turn =/= Color of
                true ->
                    [];
                false ->
                    Right = maps:get(white, Castling),
                    maybe_castle(Board, KingPos, Right, white)
            end;
        black ->
            KingPos = {5, 8},
            case Turn =/= Color of
                true ->
                    [];
                false ->
                    Right = maps:get(black, Castling),
                    maybe_castle(Board, KingPos, Right, black)
            end
    end.

maybe_castle(Board, KingPos, Rights, Color) ->
    %% We must ensure: king exists on KingPos, rook exists on corner, squares between empty, king not in check,
    %% king does not move through attacked square, and destination not attacked.
    Fun = fun(Side) ->
        case Side of
            kingside ->
                {RookPos, Path, Through, Dest} =
                    case Color of
                        white -> {{8, 1}, [{6, 1}, {7, 1}], [{6, 1}], {7, 1}};
                        black -> {{8, 8}, [{6, 8}, {7, 8}], [{6, 8}], {7, 8}}
                    end,
                can_castle(
                    Board, Color, KingPos, RookPos, Path, Through, Dest
                );
            queenside ->
                {RookPos, Path, Through, Dest} =
                    case Color of
                        white ->
                            {
                                {1, 1},
                                [{4, 1}, {3, 1}, {2, 1}],
                                [{4, 1}, {3, 1}],
                                {3, 1}
                            };
                        black ->
                            {
                                {1, 8},
                                [{4, 8}, {3, 8}, {2, 8}],
                                [{4, 8}, {3, 8}],
                                {3, 8}
                            }
                    end,
                can_castle(Board, Color, KingPos, RookPos, Path, Through, Dest)
        end
    end,
    Ks = lists:flatmap(
        fun({SideAtom, Bool}) ->
            case Bool of
                true ->
                    case SideAtom of
                        kingside ->
                            case Fun(kingside) of
                                true -> [{7, element(2, KingPos)}];
                                _ -> []
                            end;
                        queenside ->
                            case Fun(queenside) of
                                true -> [{3, element(2, KingPos)}];
                                _ -> []
                            end
                    end;
                false ->
                    []
            end
        end,
        [
            {kingside, maps:get(kingside, Rights)},
            {queenside, maps:get(queenside, Rights)}
        ]
    ),
    Ks.

can_castle(Board, Color, KingPos, RookPos, Path, Through, Dest) ->
    %% basic preconditions: king at KingPos, rook at RookPos, squares in Path empty
    case maps:get(KingPos, Board, none) of
        {Color, king} ->
            case maps:get(RookPos, Board, none) of
                {Color, rook} ->
                    EmptyOk = lists:all(
                        fun(S) -> maps:get(S, Board, none) =:= none end, Path
                    ),
                    case EmptyOk of
                        false ->
                            false;
                        true ->
                            %% king must not be in check now, nor in any 'through' square, nor on destination
                            case in_check(Board, Color) of
                                true ->
                                    false;
                                false ->
                                    %% simulate king on through squares and dest and ensure not attacked
                                    AllClear = lists:all(
                                        fun(Sq) ->
                                            SimB = maps:remove(KingPos, Board),
                                            SimB2 = maps:put(
                                                Sq, {Color, king}, SimB
                                            ),
                                            not in_check(SimB2, Color)
                                        end,
                                        Through ++ [Dest]
                                    ),
                                    AllClear
                            end
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% when castling executed, we produce board in do_castle simulation (used by simulate_apply_move)
do_castle_board(Board, Color, kingside) ->
    case Color of
        white ->
            B1 = maps:remove({5, 1}, Board),
            B2 = maps:remove({8, 1}, B1),
            maps:put(
                {7, 1}, {white, king}, maps:put({6, 1}, {white, rook}, B2)
            );
        black ->
            B1 = maps:remove({5, 8}, Board),
            B2 = maps:remove({8, 8}, B1),
            maps:put({7, 8}, {black, king}, maps:put({6, 8}, {black, rook}, B2))
    end;
do_castle_board(Board, Color, queenside) ->
    case Color of
        white ->
            B1 = maps:remove({5, 1}, Board),
            B2 = maps:remove({1, 1}, B1),
            maps:put(
                {3, 1}, {white, king}, maps:put({4, 1}, {white, rook}, B2)
            );
        black ->
            B1 = maps:remove({5, 8}, Board),
            B2 = maps:remove({1, 8}, B1),
            maps:put({3, 8}, {black, king}, maps:put({4, 8}, {black, rook}, B2))
    end.

%% -------------------------
%% Simulation helpers (apply a move to board / handle EP / promotion / castling)
%% -------------------------

simulate_apply_move(State, From, To, {Color, Kind}, PromoteFun) ->
    Board0 = maps:get(<<"board">>, State),
    EP = maps:get(<<"ep">>, State, none),
    case {Kind, From, To} of
        {king, {5, 1}, {7, 1}} ->
            {
                do_castle_board(Board0, white, kingside),
                maps:put(<<"ep">>, none, State)
            };
        {king, {5, 1}, {3, 1}} ->
            {
                do_castle_board(Board0, white, queenside),
                maps:put(<<"ep">>, none, State)
            };
        {king, {5, 8}, {7, 8}} ->
            {
                do_castle_board(Board0, black, kingside),
                maps:put(<<"ep">>, none, State)
            };
        {king, {5, 8}, {3, 8}} ->
            {
                do_castle_board(Board0, black, queenside),
                maps:put(<<"ep">>, none, State)
            };
        _ ->
            case {Kind, EP} of
                {pawn, EP} when EP =:= To ->
                    CapturedRank =
                        case Color of
                            white -> element(2, To) - 1;
                            black -> element(2, To) + 1
                        end,
                    CapturedPos = {element(1, To), CapturedRank},
                    B1 = maps:remove(From, Board0),
                    B2 = maps:remove(CapturedPos, B1),
                    B3 = maps:put(To, {Color, pawn}, B2),
                    {
                        maybe_promote(B3, To, Color, PromoteFun),
                        maps:put(<<"ep">>, none, State)
                    };
                _ ->
                    B1 = maps:remove(From, Board0),
                    B2 = maps:put(To, {Color, Kind}, B1),
                    {
                        maybe_promote(B2, To, Color, PromoteFun),
                        maps:put(<<"ep">>, none, State)
                    }
            end
    end.

maybe_promote(Board, {File, Rank}, Color, PromoteFun) ->
    case maps:get({File, Rank}, Board, none) of
        {white, pawn} when Rank =:= 8 ->
            Kind = safe_promotion(Color, PromoteFun),
            maps:put({File, Rank}, {Color, Kind}, Board);
        {black, pawn} when Rank =:= 1 ->
            Kind = safe_promotion(Color, PromoteFun),
            maps:put({File, Rank}, {Color, Kind}, Board);
        _ ->
            Board
    end.

safe_promotion(Color, Fun) ->
    try Fun(Color) of
        queen -> queen;
        rook -> rook;
        bishop -> bishop;
        knight -> knight;
        pawn -> pawn;
        _ -> queen
    catch
        _:_ -> queen
    end.

update_ep(State, {Fx, Fy}, {_Tx, Ty}, {Color, Kind}) ->
    case {Kind, Color, Fy, Ty} of
        {pawn, white, 2, 4} -> maps:put(<<"ep">>, {Fx, 3}, State);
        {pawn, black, 7, 5} -> maps:put(<<"ep">>, {Fx, 6}, State);
        _ -> maps:put(<<"ep">>, none, State)
    end.

%% -------------------------
%% Castling rights updates
%% -------------------------

update_castling_rights(State, From, _To) ->
    Board = maps:get(<<"board">>, State),
    Castling = maps:get(<<"castling">>, State),
    NewCastling =
        case maps:get(From, Board, none) of
            {white, king} ->
                Nc = maps:put(
                    white, #{kingside => false, queenside => false}, Castling
                ),
                Nc;
            {black, king} ->
                Nc = maps:put(
                    black, #{kingside => false, queenside => false}, Castling
                ),
                Nc;
            {white, rook} ->
                case From of
                    {1, 1} ->
                        put_in_castling(Castling, white, queenside, false);
                    {8, 1} ->
                        put_in_castling(Castling, white, kingside, false);
                    _ ->
                        Castling
                end;
            {black, rook} ->
                case From of
                    {1, 8} ->
                        put_in_castling(Castling, black, queenside, false);
                    {8, 8} ->
                        put_in_castling(Castling, black, kingside, false);
                    _ ->
                        Castling
                end;
            _ ->
                Castling
        end,
    maps:put(<<"castling">>, NewCastling, State).

put_in_castling(C, Color, Side, Val) ->
    Inner = maps:get(Color, C),
    Inner2 = maps:put(Side, Val, Inner),
    maps:put(Color, Inner2, C).

parse_castling("-") ->
    #{
        white => #{kingside => false, queenside => false},
        black => #{kingside => false, queenside => false}
    };
parse_castling(Str) ->
    #{
        white => #{
            kingside => lists:member($K, Str),
            queenside => lists:member($Q, Str)
        },
        black => #{
            kingside => lists:member($k, Str),
            queenside => lists:member($q, Str)
        }
    }.

%% -------------------------
%% Check detection & attack tests
%% -------------------------

%% in_check(Board, Color) -> true if Color's king is attacked on Board
in_check(Board, Color) ->
    %% find king pos
    KingPos = find_king(Board, Color),
    case KingPos of
        %% malformed position
        none -> false;
        Pos -> is_attacked(Board, Pos, other(Color))
    end.

find_king(Board, Color) ->
    Fun = fun(Key, Value, Acc) ->
        case Value of
            {C, king} when C =:= Color -> Key;
            _ -> Acc
        end
    end,
    maps:fold(Fun, none, Board).

is_attacked(Board, Pos, ByColor) ->
    (pawn_attacks(Board, Pos, ByColor) orelse
        knight_attacks(Board, Pos, ByColor) orelse
        sliding_attacks(Board, Pos, ByColor) orelse
        king_adjacent(Board, Pos, ByColor)).

pawn_attacks(Board, {F, R}, black) ->
    %% black pawns attack down: pawn at (F-1,R+1) or (F+1,R+1)
    A1 = {F - 1, R + 1},
    A2 = {F + 1, R + 1},
    (maps:get(A1, Board, none) =:= {black, pawn}) orelse
        (maps:get(A2, Board, none) =:= {black, pawn});
pawn_attacks(Board, {F, R}, white) ->
    %% white pawns attack up: pawn at (F-1,R-1) or (F+1,R-1)
    A1 = {F - 1, R - 1},
    A2 = {F + 1, R - 1},
    (maps:get(A1, Board, none) =:= {white, pawn}) orelse
        (maps:get(A2, Board, none) =:= {white, pawn});
pawn_attacks(_, _, _) ->
    false.

knight_attacks(Board, {F, R}, ByColor) ->
    D = [
        {1, 2}, {2, 1}, {2, -1}, {1, -2}, {-1, -2}, {-2, -1}, {-2, 1}, {-1, 2}
    ],
    lists:any(
        fun({Df, Dr}) ->
            P = {F + Df, R + Dr},
            inside(P) andalso maps:get(P, Board, none) =:= {ByColor, knight}
        end,
        D
    ).

king_adjacent(Board, {F, R}, ByColor) ->
    lists:any(
        fun(Df) ->
            lists:any(
                fun(Dr) ->
                    P = {F + Df, R + Dr},
                    inside(P) andalso
                        maps:get(P, Board, none) =:= {ByColor, king}
                end,
                [-1, 0, 1]
            )
        end,
        [-1, 0, 1]
    ).

sliding_attacks(Board, {F, R}, ByColor) ->
    Orth = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
    Diag = [{1, 1}, {1, -1}, {-1, 1}, {-1, -1}],
    (lists:any(
        fun(D) -> ray_attack(Board, {F, R}, D, ByColor, [rook, queen]) end, Orth
    )) orelse
        (lists:any(
            fun(D) ->
                ray_attack(Board, {F, R}, D, ByColor, [bishop, queen])
            end,
            Diag
        )).

ray_attack(Board, {F, R}, {Df, Dr}, ByColor, AcceptKinds) ->
    Next = {F + Df, R + Dr},
    case inside(Next) of
        false ->
            false;
        true ->
            case maps:get(Next, Board, none) of
                none ->
                    ray_attack(Board, Next, {Df, Dr}, ByColor, AcceptKinds);
                {C, Kind} when C =:= ByColor ->
                    lists:member(Kind, AcceptKinds);
                _ ->
                    false
            end
    end.

%% -------------------------
%% FEN parsing / serializing helpers
%% -------------------------

parse_board(Str) ->
    Ranks = string:tokens(Str, "/"),
    parse_ranks(Ranks, 8, #{}).

parse_ranks([], _Rank, Acc) ->
    Acc;
parse_ranks([R | Rs], Rank, Acc) ->
    RowMap = parse_rank_chars(R, 1, Rank, #{}),
    parse_ranks(Rs, Rank - 1, maps:merge(Acc, RowMap)).

parse_rank_chars([], _File, _Rank, Acc) ->
    Acc;
parse_rank_chars([C | Cs], File, Rank, Acc) when C >= $1, C =< $8 ->
    N = C - $0,
    parse_rank_chars(Cs, File + N, Rank, Acc);
parse_rank_chars([C | Cs], File, Rank, Acc) ->
    Piece = decode_piece(C),
    Acc1 = maps:put({File, Rank}, Piece, Acc),
    parse_rank_chars(Cs, File + 1, Rank, Acc1).

decode_piece(C) when C >= $A, C =< $Z ->
    {white, decode_kind(C)};
decode_piece(C) ->
    {black, decode_kind(C)}.

decode_kind($P) -> pawn;
decode_kind($N) -> knight;
decode_kind($B) -> bishop;
decode_kind($R) -> rook;
decode_kind($Q) -> queen;
decode_kind($K) -> king;
decode_kind(C) when C >= $a -> decode_kind(C - 32).

serialize_board(Board) ->
    Ranks = lists:map(
        fun(R) -> serialize_rank(Board, R) end, lists:seq(8, 1, -1)
    ),
    string:join(Ranks, "/").

serialize_rank(Board, Rank) ->
    serialize_rank_acc(Board, 1, Rank, 0, []).

serialize_rank_acc(_Board, 9, _Rank, 0, Acc) ->
    lists:flatten(lists:reverse(Acc));
serialize_rank_acc(_Board, 9, _Rank, Empty, Acc) ->
    lists:flatten(lists:reverse([integer_to_list(Empty) | Acc]));
serialize_rank_acc(Board, File, Rank, Empty, Acc) ->
    case maps:get({File, Rank}, Board, none) of
        none ->
            serialize_rank_acc(Board, File + 1, Rank, Empty + 1, Acc);
        {Color, Kind} ->
            NewAcc =
                case Empty of
                    0 -> Acc;
                    N -> [integer_to_list(N) | Acc]
                end,
            C = encode_piece({Color, Kind}),
            serialize_rank_acc(Board, File + 1, Rank, 0, [[C] | NewAcc])
    end.

encode_piece({white, pawn}) -> "P";
encode_piece({white, knight}) -> "N";
encode_piece({white, bishop}) -> "B";
encode_piece({white, rook}) -> "R";
encode_piece({white, queen}) -> "Q";
encode_piece({white, king}) -> "K";
encode_piece({black, pawn}) -> "p";
encode_piece({black, knight}) -> "n";
encode_piece({black, bishop}) -> "b";
encode_piece({black, rook}) -> "r";
encode_piece({black, queen}) -> "q";
encode_piece({black, king}) -> "k".

serialize_castling(#{white := W, black := B}) ->
    %% build explicitly
    WKing =
        case maps:get(kingside, W) of
            true -> "K";
            false -> ""
        end,
    WQueen =
        case maps:get(queenside, W) of
            true -> "Q";
            false -> ""
        end,
    BKing =
        case maps:get(kingside, B) of
            true -> "k";
            false -> ""
        end,
    BQueen =
        case maps:get(queenside, B) of
            true -> "q";
            false -> ""
        end,
    List = lists:concat([WKing, WQueen, BKing, BQueen]),
    case List of
        "" -> "-";
        _ -> List
    end.

serialize_ep(none) -> "-";
serialize_ep({File, Rank}) -> [File + $a - 1, Rank + $0].

parse_ep("-") ->
    none;
parse_ep(Chars) when is_list(Chars) ->
    case Chars of
        [Fc, Rc] -> {Fc - $a + 1, Rc - $0};
        _ -> none
    end.

%% -------------------------
%% Utilities
%% -------------------------

other(white) -> black;
other(black) -> white.
