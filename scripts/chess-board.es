#!/usr/bin/env escript
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-include("ansi.hrl").
-include("chess_symbols.hrl").

%%--------------------------------------------------------------------
%% Entry Point
%%--------------------------------------------------------------------
main(Args) ->
    ok = shell:start_interactive({noshell, raw}),

    io:put_chars(?ALT_SCREEN_ON),
    io:put_chars(?HIDE_CURSOR),
    io:put_chars(?CLEAR_SCREEN),

    %% Get FEN string from args or use initial position
    FEN =
        case Args of
            [FenString | _] -> FenString;
            %% Initial position
            [] -> "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
        end,

    %% Parse FEN and create board
    Board = fen_to_board(FEN),
    draw_board(Board),

    %% Wait for 'q' to quit
    wait_for_quit(),

    io:put_chars(?SHOW_CURSOR),
    io:put_chars(?ALT_SCREEN_OFF),
    ok.

%%--------------------------------------------------------------------
%% Parse FEN string to board
%% FEN format: pieces are described rank by rank from 8 to 1
%% Example: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
%%--------------------------------------------------------------------
fen_to_board(FEN) ->
    %% Split FEN by '/' to get ranks (rows)
    Ranks = string:split(FEN, "/", all),

    %% Process each rank, starting from rank 8 (top) down to rank 1
    lists:foldl(
        fun({Rank, RankStr}, Board) ->
            parse_rank(RankStr, Rank, 1, Board)
        end,
        #{},
        lists:zip(lists:seq(8, 1, -1), Ranks)
    ).

%%--------------------------------------------------------------------
%% Parse a single rank (row) of the FEN string
%%--------------------------------------------------------------------
parse_rank([], _Row, _Col, Board) ->
    Board;
parse_rank([Char | Rest], Row, Col, Board) when Char >= $1, Char =< $8 ->
    %% Number means empty squares
    EmptySquares = Char - $0,
    parse_rank(Rest, Row, Col + EmptySquares, Board);
parse_rank([Char | Rest], Row, Col, Board) ->
    %% Letter means a piece
    {Color, PieceType} = char_to_piece(Char),
    NewBoard = Board#{{Row, Col} => {Color, PieceType}},
    parse_rank(Rest, Row, Col + 1, NewBoard).

%%--------------------------------------------------------------------
%% Convert FEN character to piece
%% Uppercase = white, lowercase = black
%%--------------------------------------------------------------------
char_to_piece($R) -> {white, "r"};
char_to_piece($N) -> {white, "n"};
char_to_piece($B) -> {white, "b"};
char_to_piece($Q) -> {white, "q"};
char_to_piece($K) -> {white, "k"};
char_to_piece($P) -> {white, "p"};
char_to_piece($r) -> {black, "r"};
char_to_piece($n) -> {black, "n"};
char_to_piece($b) -> {black, "b"};
char_to_piece($q) -> {black, "q"};
char_to_piece($k) -> {black, "k"};
char_to_piece($p) -> {black, "p"}.

%%--------------------------------------------------------------------
%% Initial Board Setup (kept for reference, now using FEN)
%% Board is represented as a map with keys {Row, Col} where Row is 1-8, Col is 1-8
%% Pieces are represented as {Color, Piece} where Color is white|black
%%--------------------------------------------------------------------
initial_board() ->
    #{
        %% Black pieces (top of board, row 8)
        {8, 1} => {black, "r"},
        {8, 2} => {black, "n"},
        {8, 3} => {black, "b"},
        {8, 4} => {black, "q"},
        {8, 5} => {black, "k"},
        {8, 6} => {black, "b"},
        {8, 7} => {black, "n"},
        {8, 8} => {black, "r"},

        %% Black pawns (row 7)
        {7, 1} => {black, "p"},
        {7, 2} => {black, "p"},
        {7, 3} => {black, "p"},
        {7, 4} => {black, "p"},
        {7, 5} => {black, "p"},
        {7, 6} => {black, "p"},
        {7, 7} => {black, "p"},
        {7, 8} => {black, "p"},

        %% White pawns (row 2)
        {2, 1} => {white, "p"},
        {2, 2} => {white, "p"},
        {2, 3} => {white, "p"},
        {2, 4} => {white, "p"},
        {2, 5} => {white, "p"},
        {2, 6} => {white, "p"},
        {2, 7} => {white, "p"},
        {2, 8} => {white, "p"},

        %% White pieces (bottom of board, row 1)
        {1, 1} => {white, "r"},
        {1, 2} => {white, "n"},
        {1, 3} => {white, "b"},
        {1, 4} => {white, "q"},
        {1, 5} => {white, "k"},
        {1, 6} => {white, "b"},
        {1, 7} => {white, "n"},
        {1, 8} => {white, "r"}
    }.

%%--------------------------------------------------------------------
%% Draw Chess Board
%%--------------------------------------------------------------------
draw_board(Board) ->
    io:put_chars(?CLEAR_SCREEN),
    io:put_chars(?MVTO_ROW_COL(2, 1)),
    io:put_chars(?FG_BLUE_BG_WHITE("      CHESS BOARD       \r\n\r\n")),

    %% Column labels (a-h)
    io:put_chars("      a  b  c  d  e  f  g  h\r\n"),

    %% Draw rows from 8 to 1 (chess notation)
    lists:foreach(
        fun(Row) ->
            draw_chess_row(Board, Row)
        end,
        lists:seq(8, 1, -1)
    ),

    io:put_chars("\r\n   Press 'q' to quit\r\n").

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
%%--------------------------------------------------------------------
draw_square(Board, Row, Col) ->
    IsLight = (Row + Col) rem 2 =:= 0,

    %% Get piece at this position
    Piece = maps:get({Row, Col}, Board, empty),

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
    Symbol = maps:get(PieceType, UnicodeSymbols, " "),

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
%% Wait for 'q' key to quit
%%--------------------------------------------------------------------
wait_for_quit() ->
    case io:get_chars("", 1) of
        "q" ->
            ok;
        _ ->
            wait_for_quit()
    end.
