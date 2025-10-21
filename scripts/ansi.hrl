-ifndef(_ANSI_HRL).
-define(_ANSI_HRL, true).

%% ANSI Escape Sequences
-define(ESC, "\e").

%% Screen Control
-define(CLEAR_SCREEN, "\e[2J").
-define(CLEAR_LINE, "\e[2K").
-define(ALT_SCREEN_ON, "\e[?1049h").
-define(ALT_SCREEN_OFF, "\e[?1049l").

%% Cursor Control
-define(HIDE_CURSOR, "\e[?25l").
-define(SHOW_CURSOR, "\e[?25h").
-define(MVTO_ROW_COL(Row, Col),
    "\e[" ++ integer_to_list(Row) ++ ";" ++ integer_to_list(Col) ++ "H"
).

%% Text Formatting
-define(RESET, "\e[0m").
-define(BOLD(X), "\e[1m" ++ X ++ "\e[0m").
-define(DIM(X), "\e[2m" ++ X ++ "\e[0m").
-define(UNDERLINE(X), "\e[4m" ++ X ++ "\e[0m").
-define(BLINK(X), "\e[5m" ++ X ++ "\e[0m").
-define(REVERSE(X), "\e[7m" ++ X ++ "\e[0m").

%% Foreground Colors
-define(FG_BLACK(X), "\e[30m" ++ X ++ "\e[0m").
-define(FG_RED(X), "\e[31m" ++ X ++ "\e[0m").
-define(FG_GREEN(X), "\e[32m" ++ X ++ "\e[0m").
-define(FG_YELLOW(X), "\e[33m" ++ X ++ "\e[0m").
-define(FG_BLUE(X), "\e[34m" ++ X ++ "\e[0m").
-define(FG_MAGENTA(X), "\e[35m" ++ X ++ "\e[0m").
-define(FG_CYAN(X), "\e[36m" ++ X ++ "\e[0m").
-define(FG_WHITE(X), "\e[37m" ++ X ++ "\e[0m").

%% Background Colors
-define(BG_BLACK(X), "\e[40m" ++ X ++ "\e[0m").
-define(BG_RED(X), "\e[41m" ++ X ++ "\e[0m").
-define(BG_GREEN(X), "\e[42m" ++ X ++ "\e[0m").
-define(BG_YELLOW(X), "\e[43m" ++ X ++ "\e[0m").
-define(BG_BLUE(X), "\e[44m" ++ X ++ "\e[0m").
-define(BG_MAGENTA(X), "\e[45m" ++ X ++ "\e[0m").
-define(BG_CYAN(X), "\e[46m" ++ X ++ "\e[0m").
-define(BG_WHITE(X), "\e[47m" ++ X ++ "\e[0m").

%% Combined Foreground + Background
-define(FG_BLACK_BG_WHITE(X), "\e[30;47m" ++ X ++ "\e[0m").
-define(FG_BLUE_BG_WHITE(X), "\e[34;47m" ++ X ++ "\e[0m").
-define(FG_BLACK_BG_YELLOW(X), "\e[30;43m" ++ X ++ "\e[0m").

-endif.
