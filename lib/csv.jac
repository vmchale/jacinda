{. naive
fn presentLines(l) ≔
  [x+'\n'+y]|>(splitc l',');

fn cols() ≔ 
  presentLines"{ix=1}{`0};
