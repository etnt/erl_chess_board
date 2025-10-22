#!/usr/bin/env escript
%%% @doc Interactive terminal chess board using the erl_chess_board library.
%%%
%%% This script provides a full-featured chess game in the terminal with:
%%% <ul>
%%% <li>Visual board display with Unicode chess pieces and colored squares</li>
%%% <li>Move input in standard algebraic notation (e.g., e4, Nf3, O-O)</li>
%%% <li>Capture notation support (e.g., exd5, Nxe5)</li>
%%% <li>Full move validation and legal move checking</li>
%%% <li>Checkmate and stalemate detection</li>
%%% <li>Check indication</li>
%%% <li>FEN string display for position recreation</li>
%%% <li>Alternate screen mode (preserves terminal content)</li>
%%% </ul>
%%%
%%% == Usage ==
%%%
%%% Start a new game from the initial position:
%%% ```
%%% ./scripts/chess-board.es
%%% '''
%%%
%%% Start from a specific FEN position:
%%% ```
%%% ./scripts/chess-board.es "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
%%% '''
%%%
%%% Resume from a position shown in the FEN display:
%%% ```
%%% ./scripts/chess-board.es "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"
%%% '''
%%%
%%% == Move Notation ==
%%%
%%% The script accepts standard algebraic chess notation:
%%% <ul>
%%% <li>Pawn moves: e4, d5, a3</li>
%%% <li>Piece moves: Nf3, Bb5, Qd4 (N=knight, B=bishop, R=rook, Q=queen, K=king)</li>
%%% <li>Captures: exd5, Nxe5, Qxf7 (x indicates capture)</li>
%%% <li>Castling: O-O (kingside), O-O-O (queenside), also accepts 0-0 and 0-0-0</li>
%%% <li>Disambiguation: Nbd7 (knight from b-file), R1e2 (rook from rank 1)</li>
%%% </ul>
%%%
%%% == Display Features ==
%%%
%%% <ul>
%%% <li>Light squares: tan/wheat background (ANSI color 180)</li>
%%% <li>Dark squares: brown background (ANSI color 94)</li>
%%% <li>White pieces: bright white Unicode symbols</li>
%%% <li>Black pieces: black Unicode symbols</li>
%%% <li>Turn indicator: Shows whose turn it is to move</li>
%%% <li>Check alert: Red warning when king is in check</li>
%%% <li>FEN display: Shows current position in FEN notation (cyan)</li>
%%% </ul>
%%%
%%% == Game Ending ==
%%%
%%% The game automatically detects and announces:
%%% <ul>
%%% <li>Checkmate: "CHECKMATE! [Winner] wins!"</li>
%%% <li>Stalemate: "STALEMATE! Game is a draw."</li>
%%% </ul>
%%%
%%% To quit at any time, enter 'q' or 'Q'.
%%%
%%% == Dependencies ==
%%%
%%% Requires the erl_chess_board library to be compiled:
%%% ```
%%% rebar3 compile
%%% '''
%%%
%%% @author Torbjörn Törnkvist
%%% @version 1.0.0

-include("ansi.hrl").
-include("chess_symbols.hrl").

%%--------------------------------------------------------------------
%% Entry Point
%%--------------------------------------------------------------------
main(Args) ->
    %% Get script directory and add code paths
    ScriptDir = filename:dirname(escript:script_name()),
    RootDir = filename:dirname(ScriptDir),

    %% Add code paths for erl_chess_board module
    code:add_pathz(filename:join([RootDir, "ebin"])),
    code:add_pathz(
        filename:join([
            RootDir, "_build", "default", "lib", "erl_chess_board", "ebin"
        ])
    ),

    ok = shell:start_interactive({noshell, raw}),

    io:put_chars(?ALT_SCREEN_ON),
    io:put_chars(?HIDE_CURSOR),
    io:put_chars(?CLEAR_SCREEN),

    %% Get FEN string from args or use initial position
    InitialFEN =
        case Args of
            [FenString | _] -> FenString;
            %% Initial position
            [] -> "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        end,

    %% Create initial game state using erl_chess_board
    State = erl_chess_board:from_fen(InitialFEN),

    %% Start game loop
    game_loop(State),

    io:put_chars(?SHOW_CURSOR),
    io:put_chars(?ALT_SCREEN_OFF),
    ok.

%%--------------------------------------------------------------------
%% Read a line of input in raw mode
%%--------------------------------------------------------------------
read_line(Acc) ->
    case io:get_chars("", 1) of
        "\n" ->
            lists:reverse(Acc);
        "\r" ->
            %% Just return on carriage return, don't wait for more input
            lists:reverse(Acc);
        [Char] when Char =:= 127; Char =:= 8 ->
            %% Backspace or delete
            case Acc of
                [] ->
                    read_line(Acc);
                [_ | Rest] ->
                    %% Erase character from display
                    io:put_chars("\b \b"),
                    read_line(Rest)
            end;
        [Char] ->
            %% Echo the character
            io:put_chars([Char]),
            read_line([Char | Acc]);
        eof ->
            lists:reverse(Acc)
    end.

%%--------------------------------------------------------------------
%% Draw a single row of the chess board
%%--------------------------------------------------------------------
draw_chess_row(Board, Row) ->
    %% Row label
    io:put_chars("   " ++ integer_to_list(Row) ++ " "),

    %% Draw each square in the row
    lists:foreach(
        fun(Col) ->
            draw_square(Board, Row, Col)
        end,
        lists:seq(1, 8)
    ),

    %% Row label at end
    io:put_chars(" " ++ integer_to_list(Row)),
    io:put_chars("\r\n").

%%--------------------------------------------------------------------
%% Draw a single square
%% Chess board: light square when (Row + Col) is even, dark when odd
%% Note: Board uses {File, Rank} where File=1-8 (a-h), Rank=1-8 (row number)
%%       We draw Row (rank) from 8 to 1, Col (file) from 1 to 8
%%--------------------------------------------------------------------
draw_square(Board, Row, Col) ->
    IsLight = (Row + Col) rem 2 =:= 0,

    %% Get piece at this position - board uses {File, Rank} = {Col, Row}
    Piece = maps:get({Col, Row}, Board, empty),

    %% Get the symbol for the piece (without color codes)
    Symbol = get_piece_symbol(Piece),

    %% Draw with appropriate background - keep background on for entire cell
    Colored =
        case IsLight of
            true ->
                %% Light square (tan/wheat background - darker than before)
                "\e[48;5;180m" ++ Symbol ++ "\e[0m";
            false ->
                %% Dark square (brown background)
                "\e[48;5;94m" ++ Symbol ++ "\e[0m"
        end,

    io:put_chars(Colored).

%%--------------------------------------------------------------------
%% Get piece symbol with color
%%--------------------------------------------------------------------
get_piece_symbol(empty) ->
    "   ";
get_piece_symbol({Color, PieceType}) ->
    UnicodeSymbols = ?UNICODE_PIECE_SYMBOLS,
    %% Convert atom to string if necessary
    PieceStr =
        case PieceType of
            pawn -> "p";
            knight -> "n";
            bishop -> "b";
            rook -> "r";
            queen -> "q";
            king -> "k";
            Str when is_list(Str) -> Str
        end,
    Symbol = maps:get(PieceStr, UnicodeSymbols, " "),

    %% White pieces get bright color, black pieces get dark color
    %% Don't reset color here - let the square background handle it
    ColoredSymbol =
        case Color of
            white ->
                %% Bright white
                "\e[97m" ++ Symbol;
            black ->
                %% Black
                "\e[30m" ++ Symbol
        end,

    " " ++ ColoredSymbol ++ " ".

%%--------------------------------------------------------------------
%% Game Loop - display board and accept moves
%%--------------------------------------------------------------------
game_loop(State) ->
    %% Draw current position
    draw_board_with_state(State),

    %% Check game status
    case erl_chess_board:is_game_over(State) of
        {checkmate, Winner} ->
            %% Checkmate!
            io:put_chars(?FG_RED(?BOLD("\r\n   CHECKMATE! "))),
            WinnerStr =
                case Winner of
                    white -> "White";
                    black -> "Black"
                end,
            io:put_chars(?FG_GREEN(?BOLD(WinnerStr ++ " wins!\r\n"))),
            io:put_chars("\r\n   Press any key to exit..."),
            io:get_chars("", 1),
            %% Game over
            ok;
        stalemate ->
            %% Stalemate!
            io:put_chars(
                ?FG_YELLOW(?BOLD("\r\n   STALEMATE! Game is a draw.\r\n"))
            ),
            io:put_chars("\r\n   Press any key to exit..."),
            io:get_chars("", 1),
            %% Game over
            ok;
        ongoing ->
            %% Game continues - check if in check
            Turn = maps:get(<<"turn">>, State),
            case erl_chess_board:in_check(maps:get(<<"board">>, State), Turn) of
                true ->
                    io:put_chars(?FG_RED("   King is in check!\r\n"));
                false ->
                    ok
            end,

            %% Display FEN string using library function
            FenString = erl_chess_board:to_fen(State),
            io:put_chars(?FG_CYAN("   FEN: " ++ FenString ++ "\r\n")),

            %% Get move input
            io:put_chars(
                "\r\n   Enter move (e.g., 'e4', 'Nf3', 'O-O') or 'q' to quit: "
            ),

            %% Read input line by line in raw mode
            Input = read_line([]),
            %% Add newline after input
            io:put_chars("\r\n"),

            case string:trim(Input) of
                "" ->
                    %% Empty input, redraw
                    game_loop(State);
                "q" ->
                    ok;
                "Q" ->
                    ok;
                MoveStr ->
                    handle_move(State, MoveStr)
            end
    end.

%%--------------------------------------------------------------------
%% Handle move input
%%--------------------------------------------------------------------
handle_move(State, MoveStr) ->
    %% Handle castling notation
    NormalizedMove = normalize_move(MoveStr),

    %% Try to parse the move ourselves for simple cases
    Result =
        case parse_simple_move(State, NormalizedMove) of
            {ok, From, To} ->
                %% Use coordinate-based move
                erl_chess_board:move(State, From, To);
            error ->
                %% Fall back to library's string parser, but catch errors
                try erl_chess_board:move(State, NormalizedMove) of
                    MoveResult -> MoveResult
                catch
                    _:_ -> {error, invalid_move_string}
                end
        end,

    case Result of
        {error, Reason} ->
            %% Show error message
            draw_board_with_state(State),
            ErrorMsg = format_error(Reason),
            io:put_chars(
                ?FG_RED("\r\n   Error: " ++ ErrorMsg ++ "\r\n")
            ),
            io:put_chars(
                ?FG_YELLOW("   Move attempted: '" ++ MoveStr ++ "'\r\n")
            ),
            io:put_chars("\r\n   Press any key to continue..."),
            io:get_chars("", 1),
            game_loop(State);
        ResultState when is_map(ResultState) ->
            %% Move successful, continue with new state
            game_loop(ResultState);
        Other ->
            %% Unexpected result - show debug info
            draw_board_with_state(State),
            io:put_chars(
                ?FG_RED(
                    "\r\n   Unexpected result from move: " ++
                        io_lib:format("~p", [Other]) ++ "\r\n"
                )
            ),
            io:put_chars("\r\n   Press any key to continue..."),
            io:get_chars("", 1),
            game_loop(State)
    end.

%%--------------------------------------------------------------------
%% Normalize move notation (handle castling)
%%--------------------------------------------------------------------
normalize_move("O-O") -> "O-O";
normalize_move("O-O-O") -> "O-O-O";
normalize_move("0-0") -> "O-O";
normalize_move("0-0-0") -> "O-O-O";
normalize_move(Move) -> Move.

%%--------------------------------------------------------------------
%% Parse simple algebraic moves (e4, Nf3, exd5, e4xe5, etc.) to coordinates
%%--------------------------------------------------------------------
parse_simple_move(State, MoveStr) ->
    %% Try to match simple patterns (with optional capture notation)
    %% Patterns: e4, Nf3, exd5, e4xd5, e4xe5, Nf3xe5, etc.
    case
        re:run(MoveStr, "^([NBRQK])?([a-h])?([1-8])?x?([a-h])([1-8])$", [
            {capture, all_but_first, list}
        ])
    of
        {match, [PieceStr, FromFileStr, FromRankStr, ToFileStr, ToRankStr]} ->
            %% Destination square
            [ToFileChar] = ToFileStr,
            To = {ToFileChar - $a + 1, list_to_integer(ToRankStr)},

            %% Find the piece that can move there
            Board = maps:get(<<"board">>, State),
            Turn = maps:get(<<"turn">>, State),

            Piece =
                case PieceStr of
                    "" -> pawn;
                    "N" -> knight;
                    "B" -> bishop;
                    "R" -> rook;
                    "Q" -> queen;
                    "K" -> king
                end,

            %% Extract source file/rank if provided (for disambiguation or pawn captures)
            FromFile =
                case FromFileStr of
                    "" -> undefined;
                    [F] -> F - $a + 1
                end,
            FromRank =
                case FromRankStr of
                    "" -> undefined;
                    R -> list_to_integer(R)
                end,

            %% Find all pieces of this type that can move to To
            Candidates = find_pieces_that_can_move(
                State, Piece, Turn, To, FromFile, FromRank
            ),

            case Candidates of
                [From] -> {ok, From, To};
                [] -> error;
                %% Ambiguous
                _ -> error
            end;
        nomatch ->
            error
    end.

%%--------------------------------------------------------------------
%% Find all pieces of a type that can legally move to a square
%% Optionally filter by source file and/or rank for disambiguation
%%--------------------------------------------------------------------
find_pieces_that_can_move(State, Piece, Color, To, FromFile, FromRank) ->
    Board = maps:get(<<"board">>, State),
    AllSquares = [{F, R} || F <- lists:seq(1, 8), R <- lists:seq(1, 8)],

    lists:filter(
        fun(From = {F, R}) ->
            %% Check if source file/rank matches if specified
            FileMatch = FromFile =:= undefined orelse F =:= FromFile,
            RankMatch = FromRank =:= undefined orelse R =:= FromRank,

            case FileMatch andalso RankMatch of
                false ->
                    false;
                true ->
                    case maps:get(From, Board, none) of
                        {Color, Piece} ->
                            %% Check if this piece can legally move to To
                            LegalMoves = erl_chess_board:legal_moves_with_specials(
                                State, From
                            ),
                            lists:member(To, LegalMoves);
                        _ ->
                            false
                    end
            end
        end,
        AllSquares
    ).

%%--------------------------------------------------------------------
%% Format error messages
%%--------------------------------------------------------------------
format_error(no_piece) -> "No piece at source square";
format_error(not_your_turn) -> "Not your turn";
format_error(illegal_move) -> "Illegal move";
format_error(would_leave_king_in_check) -> "Move would leave king in check";
format_error(invalid_move_string) -> "Invalid move notation";
format_error(no_such_piece) -> "No such piece can make that move";
format_error(ambiguous_move) -> "Ambiguous move, specify file or rank";
format_error(Other) -> atom_to_list(Other).

%%--------------------------------------------------------------------
%% Draw board with turn indicator
%%--------------------------------------------------------------------
draw_board_with_state(State) ->
    Board = maps:get(<<"board">>, State),
    Turn = maps:get(<<"turn">>, State),

    io:put_chars(?CLEAR_SCREEN),
    io:put_chars(?MVTO_ROW_COL(2, 1)),

    TurnStr =
        case Turn of
            white -> ?FG_RED("WHITE");
            black -> ?FG_GREEN("BLACK")
        end,

    io:put_chars(
        ?FG_BLUE_BG_WHITE("      CHESS BOARD       ") ++ "   " ++
            ?BOLD(TurnStr ++ " to move") ++ "\r\n\r\n"
    ),

    %% Column labels (a-h)
    io:put_chars("      a  b  c  d  e  f  g  h\r\n"),

    %% Draw rows from 8 to 1 (chess notation)
    lists:foreach(
        fun(Row) ->
            draw_chess_row(Board, Row)
        end,
        lists:seq(8, 1, -1)
    ),

    ok.
