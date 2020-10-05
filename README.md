# Hefti - convert scores into parts of books

Hefti, named after "Neal Hefti"[1] the prolific composer and arranger is a tool for producing "books" of parts musicians out of a directory of "score" files. My use case for it is preparing parts for my band "Stompin' at Summerhall", where given a set of tunes in head form (be that transposed or not), I want to quickly produce a set of "books" for each part or key with the relevant transposition of each tune. MuseScore *used to* have a feature called "books" that would be perfect for this use case, however it was removed in a previous version. This tool attempts to replace that feature, and go further - allowing the user to provide a single (C) transposition of a tune, and automatically produce Eb and Bb transposed parts as well.

# Requirements, building and installing

Hefti is implemented in the Haskell programming language, and uses the `stack` build tool for building and dependency management. It uses MuseScore for producing part pdf files, and either `xelatex` or `pdftk` for concatenating pdf files together (depending on option passed).

Hefti does not require any particular magic at compile time. Simply cloning hefti and calling `stack build --copy-bins` should be sufficient to install it on your path (assuming that you have set up stack correctly).

# Usage

To use hefti, invoke it in a directory with a `src` directory containing musescore files. For example, given a directory structure:

    ./
    ../
    src/

invoking `hefti` should produce:

    ./
    ../
    src/
    build/
    books/

with concatenated books of parts in the `books` directory.

To customise the source, build, and books directory, please see the extended options in hefti, found by calling `hefti --help`


[1]: I chose Hefti as he did some amazing work arranging for William "Count" Basie, and I already have a tool named "Strayhorn" and one called "Basie". Obviously, the latter two are significantly more important musicians, but `hefti' is a nice enough command, and thematically relevant.