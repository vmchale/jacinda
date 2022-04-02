{. naive
fn presentLines(l) ≔
  [x+'\n'+y]|>(splitc l',');

fn cols() ≔
  presentLines"{⍳=1}{`0};
