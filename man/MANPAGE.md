% ja (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

ja - Jacinda: data filtering, processing, reporting

# SYNOPSIS

  ja run src.jac -i data.txt

  cat FILE1 FILE2 | ja \'#\"$0'

  ja tc script.jac

  ja e \'11.67\*1.2'

# DESCRIPTION

**Jacinda** is a data stream processing language à la AWK.

# SUBCOMMANDS

**run** - Run a program from file

**tc** - Typecheck a program

**e** - Evaluate an expression (without reference to a file)

# OPTIONS

**-h** **-\-help**
:   Display help

**-V** **-\-version**
:   Display version information

**-I** **-\-include**
:   Include directory for imports

# LANGUAGE

## REGEX

Regular expressions follow Rust's regex library: https://docs.rs/regex/

## BUILTINS

**:i** Postfix operator: parse integer

**:f** Postfix operator: parse float

**:** Postfix operator: parse, inferring type

**#** Prefix operator: tally (count bytes in string)

**#\*** Prefix operator: list length
:   List a  ->  a

**,** Ternary operator: zip with
:   (a -> b -> c) -> Stream a -> Stream b -> Stream c

**|** Ternary operator: fold
:   Foldable f :=> (b -> a -> b) -> b -> f a -> b

**|>** Fold without seed
:   Foldable f :=> (a -> a -> a) -> f a -> a

**^** Ternary operator: scan
:   (b -> a -> b) -> b -> Stream a -> Stream b

**\"**,  **¨** Binary operator: map
:   Functor f :=> a -> b -> f a -> f b

**[:** Unary operator: const
:   a -> b -> a

**#.** Binary operator: filter
:   Witherable f :=> (a -> Bool) -> f a -> f a

**\\.** Binary operator: prior
:   (a -> a -> b) -> Stream a -> Stream b

**~.** Unary deduplication (stream)
:   Eq a :=> Stream a -> Stream a

**~.\*** Deduplicate on (stream)
:   Eq b :=> (a -> b) -> Stream a -> Stream a

**max** Maximum of two values
:   Ord a :=> a -> a -> a

**min** Minimum of two values
:   Ord a :=> a -> a -> a

**&** Boolean and

**||** Boolean or

**!** Prefix boolean not

**~** Matches regex
:   Str -> Regex -> Bool

**!~** Does not match
:   Str -> Regex -> Bool

**ix**, **⍳** Line number

**substr** Extract substring
:   Str -> Int -> Int -> Str

**split** Split a string by regex
:   Str -> Regex -> List Str

**splitc** Split a string on a single character
:   Str -> Str -> List Str

**⌊**, **|.** Floor function
:   Float -> Int

**⌈**, **|`** Ceiling function
:   Float -> Int

**-.** Unary negate

**sprintf** Convert an expression to a string using the format string
:   **%f** float **%i** integer **%s** string

**option** Option eliminator
:   b -> (a -> b) -> Option a -> b

**match**
:   Str -> Regex -> Option (Int . Int)

**~\*** Match, returning nth capture group
:   Str -> Int -> Regex -> Option Str

**captures** Return all captures
:   Str -> Int -> Regex -> List Str

**:?** mapMaybe
:   Witherable f :=> (a -> Option b) -> f a -> f b

**.?** catMaybes
:   Witherable f :=> f (Option a) -> f a

**fp** Filename

**nf** Number of fields

## SYNTAX

**`n** nth field

**`\*** last field

**$n** nth column

**{%\<pattern>}{\<expr>}** Filtered stream on lines matching \<pattern>, defined by \<expr>

**{\<expr>}{\<expr>}** Filtered stream defined by \<expr>, on lines satisfying
a boolean expression.

**{|\<expr>}** Stream defined by \<expr>

**#t** Boolean literal

**\_n** Negative number

**.n**  Extract the nth value
:   List a -> a

**->n** Get the nth element of a tuple

**{.** Line comment

**@include\'/path/file.jac'** File include

## DECLARATIONS

**:set fs=/REGEX/;** Set field separator

**:flush;** Flush stdout for every line

# INFLUENTIAL ENVIRONMENT VARIABLES

`JAC_PATH` - colon-separated list of directories to search

# EXAMPLES

[#x>72] #. $0
:   Print lines longer than 72 bytes

{#\`0>72}{\`0}
:   Print lines longer than 72 bytes


{ix=3}{`0}
:   Select only the third line

{| sprintf \'%i %i\' (\`2 . \`1)}
:   Print the first two fields in opposite order

:set fs := /,[ \\t]*|[ \\t]+/; {| sprintf \'%i %i\' (\`2 . \`1)}
:   Same, with input fields separated by comma and/or blanks and tabs.

(+)|0 $1:i
:   Sum first column

(+)|0 [:1\"$0
:   Count lines

[y]|> {|ix}
:   Count lines

(+)|0 [#x+1]\"$0
:   Count bytes (+1 for newlines)

(+)|0 {|#`0+1}
:   Count bytes

{|sprintf \'%i: %s\' (ix.`0)}
:   Display with line numbers

(&)|#t (>)\\. {|`1:f}
:   Is the first column strictly increasing?

[y]|> {|`0~/^$/}
:   Is the last line blank?

.?{|`1 ~* 1 /([^\?]*)/}
:   Trim URL

# BUGS

Please report any bugs you may come across to
https://github.com/vmchale/jacinda/issues

# COPYRIGHT

Copyright 2021-2022. Vanessa McHale. All Rights Reserved.
