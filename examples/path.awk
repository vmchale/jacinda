BEGIN { FS = ":" ;OFS = "\n" }
{$1=$1 ; print $0}
