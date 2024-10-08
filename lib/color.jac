{. https://stackoverflow.com/a/33206814/11296354
{. https://stackoverflow.com/a/75985833/11296354
fn code(begin, str) :=
  '\ESC[' + begin + 'm' + str + '\ESC[0m';

fn underline := 
  code '4';
fn bold := 
  code '1';

fn black := code '30';
fn red := code '31';
fn green := code '32';
fn yellow := code '33';
fn blue := code '34';
fn magenta := code '35';
fn cyan := code '36';
fn white := code '37'

fn boldred := code '31;1';
