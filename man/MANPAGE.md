% ja (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

ja - Jacinda: data filtering, processing, reporting

# SYNOPSIS

  ja run src.jac -i data.txt

  cat FILE1 FILE2 | ja '#"$0'

  ja tc script.jac

  ja e '11.67\*1.2'

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

# LANGUAGE

## REGEX

Regular expressions follow Rust's regex library: https://docs.rs/regex/

## BUILTINS

**:i** Postfix operator: parse integer

**:f** Postfix operator: parse float

**:** Postfix operator: parse, inferring type

**#** Prefix operator: tally (count bytes in string)

**,** Ternary operator: zip with
:   (a -> b -> c) -> Stream a -> Stream b -> Stream c

**|** Ternary operator: fold
:   Foldable f :=> (b -> a -> b) -> b -> f a -> b

**^** Ternary operator: scan
:   (b -> a -> b) -> b -> Stream a -> Stream b

**"** Binary operator: map
:   Functor f :=> a -> b -> f a -> f b

**[:** Unary operator: const 
:   a -> b -> a

**#.** Binary operator: filter
:   (a -> Bool) -> Stream a -> Stream a

**\\.** Binary operator: prior
:   (a -> a -> a) -> Stream a -> Stream a

**max** Maximum of two values

**min** Minimum of two values

**&** Boolean and

**||** Boolean or

**!** Prefix boolean not

**~** Matches regex

**!~** Does not match

**ix** Line number

**substr** Extract substring
:   Str -> Int -> Int -> Str

**split** Split a string by regex
:   Str -> Regex -> List Str

**splitc** Split a string on a single character
:   Str -> Str -> List Str

**|.** Floor function
:   Float -> Int

**|`** Ceiling function
:   Float -> Int

**sprintf** Convert an expression to a string using the format string

**option** Option eliminator
:   b -> (a -> b) -> Option a -> b

**match**
:   Str -> Regex -> Option (Int . Int)

## SYNTAX

**`n** nth field

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

# BUGS

Please report any bugs you may come across to
https://github.com/vmchale/jacinda/issues

## Limitations

Note that `Option` is not implemented as a functor.

# COPYRIGHT

Copyright 2021-2022. Vanessa McHale. All Rights Reserved.
