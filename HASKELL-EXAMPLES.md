# Get Latest Version from Hackage

```zsh
cabal info splitmix | \
    ja --header -R'\n[^:\n]*:' -F'\s*,\s*' '[x ~* 1 /(\d+(\.\d+)*)/]:?{%/Versions available:/}{[y]|>`$}'
```

# Extract Default Extensions

```
ja --header -R'[a-z\-]+:' "{%/default-extensions/}{[x+'\n'+y]|>(drop# 1 \`$)}" -i jacinda.cabal | sort -u
```

# Get Flags

```
cabal info zlib | ja -F'[\s,]+' "{%/Flags:/}{[x+'\n'+y]|>drop# 1 \`\$}"
```

# Format Fixity Declarations for HLint

```console
echo 'import Control.Monad\nimport Data.Functor\n:info ($>)\n:info (<=<)' | ghci | ja "{%/infix/}{sprintf '- %s' \`0}"
```
