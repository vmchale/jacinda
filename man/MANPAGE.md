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

**#** Prefix operator: tally (count bytes in string)

**,** Ternary operator: zip with
:   (a -> b -> c) -> Stream a -> Stream b -> Stream c

**|** Ternary operator: fold
:   (b -> a -> b) -> b -> Stream a -> b

**^** Ternary operator: scan
:   (b -> a -> b) -> b -> Stream a -> Stream b

**"** Binary operator: map
:   a -> b -> Stream a -> Stream b

**[:** Unary operator: const 
:   a -> b -> a

**#.** Binary operator: filter
:   (a -> Bool) -> Stream a -> Stream a

**max** Maximum of two values

**min** Minimum of two values

**&** Boolean and

**||** Boolean or

**!** Prefix boolean not

**~** Matches regex

**!~** Does not match

**ix** Line number

## SYNTAX

**`n** nth field

**$n** nth column

**{%\<pattern>}{\<expr>}** Filtered stream on lines matching \<pattern>, defined by \<expr>

**{\<expr>}{\<expr>}** Filtered stream defined by \<expr>, on lines satisfying
a boolean expression.

**#t** Boolean literal

**\_n** Negative number

# BUGS

Please report any bugs you may come across to
https://hub.darcs.net/vmchale/jacinda/issues

# COPYRIGHT

Copyright 2021. Vanessa McHale. All Rights Reserved.
