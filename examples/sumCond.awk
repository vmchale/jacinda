# from mawk manual
$1 ~ /credit|gain/ { sum += $2 }
$1 ~ /debit|loss/  { sum -= $2 }

END { print sum }
