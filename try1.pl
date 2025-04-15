% Coding by YUXUAN LI -1510866

% -------------------- SUMMARY -----------------------------------------------
% This Prolog program solves fill-in puzzles: when given an empty puzzle
% and a word list, it can automatically fill the words into the correct slots.

% The planning idea is:
%   - First extract horizontal slots;
%   - Then transpose the puzzle and extract vertical slots 
%     (actually repeating horizontal extraction);
%   - Keep slots length ≥ 2;
%   - Combine all slots, then use backtracking to match slots and words;
%   - After a word is matched, remove it and continue to fill the rest;
%   - Repeat until all blanks are filled;
%   - Finally, print the completed puzzle.
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------


% ---------------Module definition and library imports-------------------------
%  library clpfd for transpose/2 and ensure intersecting slots share letters.
%  library lists provides commonly used predicates needed in this project.

:- module(proj1, [puzzle_solution/2]).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------

% --------------------Main part------------------------------------------------

puzzle_solution(Puzzle, WordList) :-
% extract horizontal slots
    extract_slots(Puzzle, HorizontalSlots),
% transpose and extract vertical slots
    transpose(Puzzle, TransposedPuzzle),
    extract_slots(TransposedPuzzle, VerticalSlots),
% Remove slots with length less than 2
    filter_slots(HorizontalSlots, ValidHorizontal),
    filter_slots(VerticalSlots, ValidVertical),
% Combine row and column slots
    append(ValidHorizontal, ValidVertical, AllSlots),
% Match each slot with a word.
    assign_words(AllSlots, WordList),!,
 % Print result
  print_puzzle(Puzzle).

% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------


% -------------------------------Get slot--------------------------------------
% This part mainly extracts all the slots of the puzzle and 
% Ends when it encounters '#' or is empty. 
% Then discard the slots less than 2 and append them into a complete list.

% Given a list of rows, extract all slots
extract_slots([], []).
extract_slots([Row|RestRows], Slots) :-
    extract_row_slots(Row, RowSlots),
    extract_slots(RestRows, OtherSlots),
    append(RowSlots, OtherSlots, Slots).

extract_row_slots(Row, Slots) :-
    extract_row_slots(Row, [], Slots).

% When meet '#' , end the slot, the following also use if else ; means or.
extract_row_slots([], Current, Slots) :-
    (   Current = [] -> Slots = []
    ;   Slots = [Current]
    ).

extract_row_slots([Cell|Rest], Current, Slots) :-
    (   Cell == '#' ->
        (   Current = [] ->
            extract_row_slots(Rest, [], Slots)
        ;   Slots = [Current|RestSlots],
            extract_row_slots(Rest, [], RestSlots)
        )
    ;   append(Current, [Cell], UpdatedCurrent),
        extract_row_slots(Rest, UpdatedCurrent, Slots)
    ).

% filter the slots which lease than 2.
filter_slots([], []).
filter_slots([Slot|Rest], ValidSlots) :-
    length(Slot, Len),
    (   Len < 2 ->
        filter_slots(Rest, ValidSlots)
    ;   filter_slots(Rest, OtherValid),
        ValidSlots = [Slot|OtherValid]
    ).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------


% ------------------------Matching slot and WordList---------------------------
% Use backtracking search to continuously match slot and wordlist.
% Base case : All lists are empty and return successfully.
% Recursively find the words that meet the requirements and use select/3 to
  % delete them.
% Continuously call recursively to match and try all combinations.
% If the branch fails, backtrack.

% Pick a word for slot
assign_words([], _).
assign_words([Slot|Slots], WordList) :-
    select(Word, WordList, RemainingWords),
    word_matches_slot(Slot, Word),
    assign_words(Slots, RemainingWords).

% Check whether word match slot.
word_matches_slot(Slot, Word) :-
    length(Slot, L),
    length(Word, L),
    maplist(match_letter, Slot, Word).

% If Cell is empty, fill it; if already filled, must match the letter
match_letter(Cell, Letter) :-
    (   var(Cell) -> Cell = Letter ; Cell == Letter).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------


% ------------------------Print Puzzle-----------------------------------------
% Print in row order

% Print each row 
print_puzzle([]).
print_puzzle([Row|Rows]) :-
    print_row(Row),
    nl,
    print_puzzle(Rows).

% Print all cells in one row, no spaces between
print_row([]).
print_row([Cell|Cells]) :-
    write(Cell), write(''),
    print_row(Cells).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------
