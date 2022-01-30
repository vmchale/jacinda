# from mawk manpages

{ chars += length($0) + 1  # add one for the \n
    words += NF
}

END{ print NR, words, chars }
