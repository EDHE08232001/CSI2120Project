% Reads a histogram from a file into a list.
% read_hist_file(+Filename, -Histogram)
read_hist_file(Filename, Histogram) :-
    open(Filename, read, Stream),
    read_line_to_codes(Stream, Line),
    close(Stream),
    string_codes(Str, Line),
    atomic_list_concat(Bins, ',', Str),
    maplist(atom_number, Bins, Histogram).

% Compares a query histogram with all dataset histograms.
% compare_histograms(+QueryHistogram, +DatasetDirectory, +DatasetFiles, -Scores)
compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, Scores) :-
    findall(
        (File, Score),
        (
            member(File, DatasetFiles),
            atom_concat(DatasetDirectory, File, Path),
            read_hist_file(Path, Histo),
            histogram_intersection(QueryHisto, Histo, Score)
        ),
        Scores).

% Calculates the histogram intersection (similarity score) between two histograms.
% histogram_intersection(+Histogram1, +Histogram2, -Score)
histogram_intersection(Histo1, Histo2, Score) :-
    maplist(min, Histo1, Histo2, MinList),
    sumlist(MinList, Score).

% Helper predicate to find the minimum of two values.
% min(+Value1, +Value2, -Min)
min(X, Y, X) :- X =< Y, !.
min(_, Y, Y).

% Retrieves the dataset text files from a directory.
% directory_textfiles(+Directory, -TextFiles)
directory_textfiles(Directory, TextFiles) :-
    directory_files(Directory, AllFiles),
    include(is_text_file, AllFiles, TextFiles).

% Checks if a file is a text file based on its extension.
% is_text_file(+Filename)
is_text_file(Filename) :-
    sub_atom(Filename, _, _, 0, '.txt').

% High-level similarity search predicate.
% similarity_search(+QueryFile, +DatasetDirectory, -BestMatches)
similarity_search(QueryFile, DatasetDirectory, BestMatches) :-
    read_hist_file(QueryFile, QueryHisto),
    directory_textfiles(DatasetDirectory, DatasetFiles),
    compare_histograms(QueryHisto, DatasetDirectory, DatasetFiles, Scores),
    sort(2, @>=, Scores, Sorted),
    take(5, Sorted, BestMatches).

% Extracts the first N elements from a list.
% take(+N, +List, -FirstN)
take(N, List, FirstN) :-
    findall(
        X,
        (nth1(Index, List, X), Index =< N),
        FirstN).

% Entry point for the similarity search.
% similarity_search(+QueryFile, -BestMatches)
similarity_search(QueryFile, BestMatches) :-
    dataset(DatasetDirectory), % Assuming dataset/1 is provided and gets the directory path
    similarity_search(QueryFile, DatasetDirectory, BestMatches).

% Placeholder for dataset directory retrieval
% dataset(-DatasetDirectory)
dataset('./queryImages/').
