{. ja run pubmed2tex.jac -i citation.nbib
:set header;
:set rs:=/\r\n[A-Z]{2,4}/;

{. 10.1016/j.tig.2011.10.004 [doi]
fn doi(record) :=
  record ~* 1 /([^ ]*) \[doi/;

fn year(dd) :=
  dd ~* 1 /(\d{4})/;

fn pfield(label,r) :=
  '    ' + label + '={' + r + '},';

fn collateAu(r) :=
  r ~* 1 /FAU - (.*)$/;

fn bind(f,x) :=
  option None f x;

fn texpaginate() := sub1 /-/ '--';

fn field(r) :=
  let 
    val key := r ~* 1 /([A-Z ]{4})-/
    val value := r ~* 2 /([A-Z ]{4})-\s*(.*)/
  in
     ?key=Some 'TI  ';(pfield 'title')¨value
    ;?key=Some 'AID ';(pfield 'doi')¨bind doi value
    ;?key=Some 'DP  ';(pfield 'year')¨bind year value
    ;?key=Some 'JT  ';(pfield 'journal')¨value
    ;?key=Some 'VI  ';(pfield 'volume')¨value
    ;?key=Some 'IP  ';(pfield 'number')¨value
    ;?key=Some 'PG  ';(λr. pfield 'pages' (texpaginate r))¨value
    ;None
  end;

let
  val au := ',\n    author={'+([x+' and '+y]|>(collateAu:?$0))+'},\n'
  val rec := [x+'\n'+y]|>(field:?$0)
in '@article{pubmed'+au+rec+'\n}' end
