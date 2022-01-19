{. ja run examples/glibc/haveversions.jac -i examples/data/Versions.all | sort -u
fn printHaveVersion(s) :=
  sprintf 'have-%s = yes' s;

printHaveVersion"{nf = 1 & `0 != '}'}{`1}
