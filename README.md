# erl_chess_board

A minimal chess board library for Erlang with move legality validation and check detection.

## Features

- Parse and generate FEN (Forsyth-Edwards Notation) strings
- Validate move legality according to chess rules
- Handle special moves like castling, en passant, and pawn promotion  
- Detect check and prevent moves that would leave the king in check
- Detect checkmate and stalemate conditions
- Interactive terminal chess game with algebraic notation support
- Comprehensive test suite with 48 test cases

## Interactive Chess Game

An example interactive chess game is included in `scripts/chess-board.es`. It provides:

- Visual board display with Unicode chess pieces and colored squares
- Move input in standard algebraic notation (e.g., `e4`, `Nf3`, `exd5`)
- Full move validation and legal move checking
- Checkmate and stalemate detection
- Check indication
- FEN string display for position recreation
- Alternate screen mode (preserves terminal content)

### Playing the Game

Start a new game from the initial position:
```bash
./scripts/chess-board.es
```

Start from a specific FEN position:
```bash
./scripts/chess-board.es "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
```

The game accepts standard algebraic notation including:
- Pawn moves: `e4`, `d5`, `a3`
- Piece moves: `Nf3`, `Bb5`, `Qd4`
- Captures: `exd5`, `Nxe5`, `Qxf7`
- Castling: `O-O` (kingside), `O-O-O` (queenside)
- Disambiguation: `Nbd7` (knight from b-file), `R1e2` (rook from rank 1)

## Documentation

📚 **[Complete API Documentation](https://etnt.github.io/erl_chess_board/)**

For local documentation generation:
```bash
rebar3 ex_doc
# Open doc/index.html in your browser
```

## Build
```bash
    $ rebar3 compile
```

## Test
```bash
    $ rebar3 eunit
```

## Usage

```erlang
%% Create a new board in starting position
State = erl_chess_board:new().

%% Make some moves (e2-e4, e7-e5)
State2 = erl_chess_board:move(State, {5,2}, {5,4}),
State3 = erl_chess_board:move(State2, {5,7}, {5,5}).

%% Convert to FEN notation
FEN = erl_chess_board:to_fen(State3).

%% Load from FEN
State4 = erl_chess_board:from_fen("r1bqkbnr/pppp1ppp/2n5/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 4 3").

%% Check if king is in check
IsInCheck = erl_chess_board:in_check(maps:get(<<"board">>, State4), white).

%% Get legal moves for a piece
Moves = erl_chess_board:legal_moves_with_specials(State4, {6,1}).
```

## FEN Format Explained

FEN (Forsyth-Edwards Notation) is a standard notation for describing a
particular board position in chess. A FEN string consists of six
space-separated fields:

### Field 1: Piece Placement

The piece placement field describes the position of all pieces on the board,
from white's perspective. It starts with rank 8 (black's back rank) and goes 
down to rank 1 (white's back rank), with ranks separated by forward slashes (`/`).

**Piece symbols:**
- Uppercase letters = White pieces: `K` (King), `Q` (Queen), `R` (Rook), `B` (Bishop), `N` (Knight), `P` (Pawn)
- Lowercase letters = Black pieces: `k`, `q`, `r`, `b`, `n`, `p`
- Numbers (1-8) = Consecutive empty squares (e.g., `3` means three empty squares in a row)

**Important:** Numbers represent consecutive empty squares on that rank:
- `8` = entire rank is empty
- `pppp1ppp` = 4 pawns, 1 empty square, 3 pawns (e.g., black's 7th rank after 1.e4 e5, where e7 is now empty)
- `5N2` = 5 empty squares, a knight, 2 empty squares

**Example:** `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR`
- Rank 8: `rnbqkbnr` (black's back rank with all pieces)
- Ranks 7-2: pawns and empty squares
- Rank 1: `RNBQKBNR` (white's back rank with all pieces)

### Field 2: Active Color

Indicates whose turn it is to move:
- `w` = White to move
- `b` = Black to move

### Field 3: Castling Availability

Indicates which castling moves are still possible:
- `K` = White can castle kingside (O-O)
- `Q` = White can castle queenside (O-O-O)
- `k` = Black can castle kingside
- `q` = Black can castle queenside
- `-` = No castling is available

Castling rights are lost when the king or relevant rook moves, even if they move back.

**Examples:**
- `KQkq` = All castling options available
- `Kq` = Only white kingside and black queenside available
- `-` = No castling available

### Field 4: En Passant Target Square

The square where an en passant capture can be made, if applicable:
- If the last move was a pawn moving two squares forward, this field contains the square "behind" the pawn (where the capturing pawn would land)
- Uses algebraic notation: `e3`, `d6`, etc.
- `-` if no en passant capture is possible

**Example:** After white plays e2-e4, the en passant square is `e3`

### Field 5: Halfmove Clock

The number of halfmoves (ply) since the last pawn advance or piece capture. Used for the fifty-move rule:
- Resets to 0 after a pawn move or capture
- Increments by 1 after each move otherwise
- Game can be claimed as a draw when this reaches 100 (50 full moves)

### Field 6: Fullmove Number

The number of full moves in the game:
- Starts at 1
- Increments after Black's move

### Complete Examples

**Starting position:**
```
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
```
- All pieces in starting positions
- White to move
- All castling available
- No en passant possible
- 0 halfmoves since capture/pawn move
- Move 1

**After 1.e4 e5 2.Nf3:**
```
rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2
```
- Black to move
- All castling still available
- No en passant (last move was knight, not pawn double-move)
- 1 halfmove since last pawn move
- Move 2

**After 1.e4 d5 (en passant possible):**
```
rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2
```
- White to move
- En passant capture available on d6
- Black pawn on d5 can be captured by white pawn on e4



