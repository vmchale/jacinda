{. strings $(which ghcup) | ja run ...
:set fs:=/:\s*/;

(λl. option (sprintf '%s (distributed with GHC)' l) [x] (l ~* 1 /(.*)-[a-f0-9]{8}/))
  ¨(.?{%/"id"/}{`2 ~* 1 /"(.*)"/})
