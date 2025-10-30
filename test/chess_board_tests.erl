-module(chess_board_tests).

-include_lib("eunit/include/eunit.hrl").

%% Run with:
%% 1> c(chess_board).
%% 2> c(chess_board_tests).
%% 3> eunit:test(chess_board_tests, [verbose]).

-import(erl_chess_board, [
    new/0,
    from_fen/1,
    to_fen/1,
    move/3, move/4,
    is_checkmate/1,
    is_stalemate/1,
    is_game_over/1
]).

%% For the function that eunit introduces on compile time
-spec test() -> _.

%%--------------------------------------------------------------
%%  Test Groups
%%--------------------------------------------------------------

-spec fen_roundtrip_test() -> _.
fen_roundtrip_test() ->
    S0 = new(),
    F = to_fen(S0),
    S1 = from_fen(F),
    ?assertEqual(F, to_fen(S1)).

-spec simple_pawn_move_test() -> _.
simple_pawn_move_test() ->
    S0 = from_fen("8/8/8/3P4/8/8/8/8 w - - 0 1"),
    S1 = move(S0, {4, 5}, {4, 6}),
    ?assertEqual(black, erl_chess_board:turn(S1)),
    B = erl_chess_board:board(S1),
    ?assertEqual({white, pawn}, maps:get({4, 6}, B)).

-spec illegal_move_test() -> _.
illegal_move_test() ->
    S0 = from_fen("8/8/8/3P4/8/8/8/8 w - - 0 1"),
    {error, illegal_move} = move(S0, {4, 5}, {5, 7}).

-spec pawn_promotion_autoqueen_test() -> _.
pawn_promotion_autoqueen_test() ->
    S0 = from_fen("k7/7P/8/8/8/8/8/7K w - - 0 1"),
    S1 = move(S0, {8, 7}, {8, 8}),
    F = to_fen(S1),
    ?assertNotEqual(nomatch, string:prefix(F, "k6Q")).

-spec pawn_promotion_callback_test() -> _.
pawn_promotion_callback_test() ->
    Fun = fun(_Color) -> rook end,
    S0 = from_fen("k7/7P/8/8/8/8/8/7K w - - 0 1"),
    S1 = move(S0, {8, 7}, {8, 8}, Fun),
    F = to_fen(S1),
    ?assertNotEqual(nomatch, string:prefix(F, "k6R")).

-spec promotion_callback_fail_fallback_test() -> _.
promotion_callback_fail_fallback_test() ->
    Bad = fun(_) -> oops end,
    S0 = from_fen("k7/7P/8/8/8/8/8/7K w - - 0 1"),
    S1 = move(S0, {8, 7}, {8, 8}, Bad),
    F = to_fen(S1),
    ?assertNotEqual(nomatch, string:prefix(F, "k6Q")).

-spec en_passant_test() -> _.
en_passant_test() ->
    %% White pawn double-step enables EP for black
    S0 = from_fen("8/8/8/3p4/8/8/2P5/8 w - - 0 1"),
    S1 = move(S0, {3, 2}, {3, 4}),
    ?assertEqual({3, 3}, erl_chess_board:ep(S1)),
    %% black captures en passant
    S2 = move(S1, {4, 5}, {3, 4}),
    B = erl_chess_board:board(S2),
    ?assertEqual({black, pawn}, maps:get({3, 4}, B)),
    ?assertEqual(none, maps:get({4, 5}, B, none)).

-spec castling_kingside_white_test() -> _.
castling_kingside_white_test() ->
    %% King and rook only
    S0 = from_fen("4k3/8/8/8/8/8/8/R3K2R w K - 0 1"),
    S1 = move(S0, {5, 1}, {7, 1}),
    B = erl_chess_board:board(S1),
    ?assertEqual({white, king}, maps:get({7, 1}, B)),
    ?assertEqual({white, rook}, maps:get({6, 1}, B)).

-spec castling_queenside_black_test() -> _.
castling_queenside_black_test() ->
    S0 = from_fen("r3k2r/8/8/8/8/8/8/4K3 b kq - 0 1"),
    S1 = move(S0, {5, 8}, {3, 8}),
    B = erl_chess_board:board(S1),
    ?assertEqual({black, king}, maps:get({3, 8}, B)),
    ?assertEqual({black, rook}, maps:get({4, 8}, B)).

-spec in_check_simple_test() -> _.
in_check_simple_test() ->
    %% White king on e1, black rook on e8 => white in check
    S0 = from_fen("4r3/8/8/8/8/8/8/4K3 w - - 0 1"),
    B = erl_chess_board:board(S0),
    ?assert(erl_chess_board:in_check(B, white)).

-spec in_check_false_test() -> _.
in_check_false_test() ->
    %% White king not in check
    S0 = from_fen("4r3/8/8/8/8/8/8/3K4 w - - 0 1"),
    B = erl_chess_board:board(S0),
    ?assertNot(erl_chess_board:in_check(B, white)).

-spec illegal_due_to_self_check_test() -> _.
illegal_due_to_self_check_test() ->
    %% Move rook away exposes check
    S0 = from_fen("4r3/8/8/8/8/8/8/R3K3 w - - 0 1"),
    {error, would_leave_king_in_check} = move(S0, {1, 1}, {1, 2}).

%% Checkmate detection tests
-spec checkmate_fools_mate_test() -> _.
checkmate_fools_mate_test() ->
    %% Classic fool's mate position
    S0 = from_fen(
        "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3"
    ),
    ?assert(is_checkmate(S0)),
    ?assertEqual({checkmate, black}, is_game_over(S0)).

-spec checkmate_back_rank_test() -> _.
checkmate_back_rank_test() ->
    %% Back rank mate - rook on e1, white king trapped on g1
    S0 = from_fen("6k1/8/8/8/8/8/5PPP/4r1K1 w - - 0 1"),
    ?assert(is_checkmate(S0)),
    ?assertEqual({checkmate, black}, is_game_over(S0)).

-spec checkmate_scholars_mate_test() -> _.
checkmate_scholars_mate_test() ->
    %% Scholar's mate position
    S0 = from_fen(
        "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4"
    ),
    ?assert(is_checkmate(S0)),
    ?assertEqual({checkmate, white}, is_game_over(S0)).

-spec checkmate_from_user_example_test() -> _.
checkmate_from_user_example_test() ->
    %% The checkmate position from the user's example
    S0 = from_fen(
        "rnQ1kb1r/ppp1pp2/5n1p/3N2p1/8/8/PPPP1PPP/R1B1KBNR b KQkq - 11 6"
    ),
    ?assert(is_checkmate(S0)),
    ?assertEqual({checkmate, white}, is_game_over(S0)).

-spec not_checkmate_in_check_can_escape_test() -> _.
not_checkmate_in_check_can_escape_test() ->
    %% King in check but can move
    S0 = from_fen("4r3/8/8/8/8/8/8/4K3 w - - 0 1"),
    ?assertNot(is_checkmate(S0)),
    ?assertEqual(ongoing, is_game_over(S0)).

-spec not_checkmate_can_block_test() -> _.
not_checkmate_can_block_test() ->
    %% King in check but can be blocked
    S0 = from_fen("4r3/8/8/8/8/4B3/8/4K3 w - - 0 1"),
    ?assertNot(is_checkmate(S0)),
    ?assertEqual(ongoing, is_game_over(S0)).

-spec not_checkmate_can_capture_test() -> _.
not_checkmate_can_capture_test() ->
    %% King in check but can capture attacking piece
    S0 = from_fen("8/8/8/8/8/8/4r3/4K3 w - - 0 1"),
    ?assertNot(is_checkmate(S0)),
    ?assertEqual(ongoing, is_game_over(S0)).

%% Stalemate detection tests
-spec stalemate_king_only_test() -> _.
stalemate_king_only_test() ->
    %% Classic stalemate - queen on c7, white king on c6, black king on a8
    S0 = from_fen("k7/2Q5/2K5/8/8/8/8/8 b - - 0 1"),
    ?assert(is_stalemate(S0)),
    ?assertEqual(stalemate, is_game_over(S0)).

-spec stalemate_pawn_blocked_test() -> _.
stalemate_pawn_blocked_test() ->
    %% King and pawn, both blocked
    S0 = from_fen("k7/P7/K7/8/8/8/8/8 b - - 0 1"),
    ?assert(is_stalemate(S0)),
    ?assertEqual(stalemate, is_game_over(S0)).

-spec not_stalemate_has_moves_test() -> _.
not_stalemate_has_moves_test() ->
    %% Similar position but black has legal moves
    S0 = from_fen("k7/8/1K6/8/8/8/8/8 b - - 0 1"),
    ?assertNot(is_stalemate(S0)),
    ?assertEqual(ongoing, is_game_over(S0)).

%% Game status ongoing tests
-spec game_ongoing_opening_test() -> _.
game_ongoing_opening_test() ->
    %% Normal opening position
    S0 = new(),
    ?assertEqual(ongoing, is_game_over(S0)),
    ?assertNot(is_checkmate(S0)),
    ?assertNot(is_stalemate(S0)).

-spec game_ongoing_middlegame_test() -> _.
game_ongoing_middlegame_test() ->
    %% Random middlegame position
    S0 = from_fen(
        "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
    ),
    ?assertEqual(ongoing, is_game_over(S0)),
    ?assertNot(is_checkmate(S0)),
    ?assertNot(is_stalemate(S0)).

%% gather all tests in a suite
-spec all_test_() -> [{string(), fun(() -> _)}].
all_test_() ->
    [
        {"FEN roundtrip", fun fen_roundtrip_test/0},
        {"Simple pawn move", fun simple_pawn_move_test/0},
        {"Illegal pawn move", fun illegal_move_test/0},
        {"Promotion auto-queen", fun pawn_promotion_autoqueen_test/0},
        {"Promotion with callback", fun pawn_promotion_callback_test/0},
        {"Promotion fallback", fun promotion_callback_fail_fallback_test/0},
        {"En passant", fun en_passant_test/0},
        {"Castling kingside (white)", fun castling_kingside_white_test/0},
        {"Castling queenside (black)", fun castling_queenside_black_test/0},
        {"Check detection true", fun in_check_simple_test/0},
        {"Check detection false", fun in_check_false_test/0},
        {"Illegal self-check prevention", fun illegal_due_to_self_check_test/0},
        %% Checkmate tests
        {"Checkmate: Fool's mate", fun checkmate_fools_mate_test/0},
        {"Checkmate: Back rank mate", fun checkmate_back_rank_test/0},
        {"Checkmate: Scholar's mate", fun checkmate_scholars_mate_test/0},
        {"Checkmate: User example", fun checkmate_from_user_example_test/0},
        {"Not checkmate: Can escape",
            fun not_checkmate_in_check_can_escape_test/0},
        {"Not checkmate: Can block", fun not_checkmate_can_block_test/0},
        {"Not checkmate: Can capture", fun not_checkmate_can_capture_test/0},
        %% Stalemate tests
        {"Stalemate: King only", fun stalemate_king_only_test/0},
        {"Stalemate: Pawn blocked", fun stalemate_pawn_blocked_test/0},
        {"Not stalemate: Has moves", fun not_stalemate_has_moves_test/0},
        %% Game status tests
        {"Game ongoing: Opening", fun game_ongoing_opening_test/0},
        {"Game ongoing: Middlegame", fun game_ongoing_middlegame_test/0}
    ].

%% ---------------------------------------------------------------
%% Property-based tests would go here (disabled due to macro conflicts)
%% ---------------------------------------------------------------
