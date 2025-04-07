{. ja run examples/tastyStackTrace.jac -Dm='/Ty/' -i test/data/tasty-prof
:set rs:=/\*\*\* Exception \(reporting due to \+RTS \-xc\): \(THUNK(_\d)*\), stack trace:/;
:set fs:=/\n/;

{%m}{[x+'\n'+y]|>[x !~ /Test\.Tasty/] #. `$}
