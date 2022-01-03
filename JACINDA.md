- Jacinda: awk-like language, but expression oriented
    - regex + "guards"
  - better lexical scoping!
  - map ("¨)
  - TODO: do I want pattern matching (I think no/commercial)
  - # 'tally'
  - .~ filter (:~) ... scan!
  - split field, sub, gsub
  - csv?

  - sum types: `Maybe` pair type?

  - commercial (luajit?) style interpreter
  - 8LENGTH (length of the pattern)
  - format strings
  - parse (digits &c., DATES)

# IDEA
- nfa->dfa conversion using lazy tying the knot trick?
# Examples
- lambdas like k: [x+1]
- currying! (-) prior (for instance, k) or `(2*) ¨` (map 2x)
  - "dyadic infix"/clump
- /usr/share/vim/vim82/tools/mve.awk
- http://www.awklang.org (volcano example)
- https://adamdrake.com/command-line-tools-can-be-235x-faster-than-your-hadoop-cluster.html
<!-- table calculus? like ADTs... sql? -->
