#!/usr/bin/env -S ja run
fn path(x) :=
  ([x+'\n'+y]) |> (splitc x ':');

path¨$0
