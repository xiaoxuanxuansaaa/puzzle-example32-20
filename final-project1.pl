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
    assign_words(AllSlots, WordList),
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


% ------------Matching slot and WordList, and choose the best one--------------

% assign_words/2: Use backtracking to assign words to each slot 
%                 and ensure that each word is used only once.
% word_matches_slot/2 and match_letter/2: Match slots and wordlists.
% candidate_count/3, slot_candidate_count/3 and choose_best_slot/4: 
%   Count the number of slots and letters and choose the best solution.


% match slot and wordlist
assign_words([], _).
assign_words(Slots, WordList) :-
% Select the best word to insert into the slot,
    choose_best_slot(Slots, WordList, BestSlot, OtherSlots),
    % Count the number of matches,must count>0.
    candidate_count(BestSlot, WordList, Count),
    Count > 0,    
    % use select(), delete the word from word list if fill the slot.
    select(Word, WordList, NewWordList),
    % Verify match
    word_matches_slot(BestSlot, Word),
    % Recurse it!!!.
    assign_words(OtherSlots, NewWordList).

% check word match slot.
word_matches_slot(Slot, Word) :-
    length(Slot, Len),
    length(Word, Len),
    maplist(match_letter, Slot, Word).

% If Cell is empty, fill it; if already filled, must match the letter
match_letter(Cell, Letter) :-
    ( var(Cell) -> Cell = Letter ; Cell == Letter ).


% Counts the number of words.
candidate_count(Slot, WordList, Count) :-
    findall(Word, (member(Word, WordList), word_matches_slot(Slot, Word)), 
    Matches),
    length(Matches, Count).

% calculates the candidate count for Slot.
slot_candidate_count(WordList, Slot, Count) :-
    candidate_count(Slot, WordList, Count).

% choose the best slot match wordlist.
choose_best_slot(Slots, WordList, BestSlot, OtherSlots) :-
    map_list_to_pairs(slot_candidate_count(WordList), Slots, Pairs),
    % use keysort to pair number and slot.
    keysort(Pairs, SortedPairs),
    SortedPairs = [_-BestSlot | _],
    select(BestSlot, Slots, OtherSlots).
% -----------------------------------------------------------------------------
% -----------------------------------------------------------------------------


% ------------------------Print Puzzle-----------------------------------------
% Print in row order

% Print each row 

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

