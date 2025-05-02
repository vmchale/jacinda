# Count Lines

```
(+)|0 [:1"$0
```

```
[y]|>{|ix}
```

# Present `PATH`

```
echo $PATH | ja -F: "{|[x+'\n'+y]|>\`$}"
```

```
echo $PATH | ja "(λl. ([x+'\n'+y]|>splitc l ':'))¨\$0"
```
