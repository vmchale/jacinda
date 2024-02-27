sprintf '<table>%s</table>' 
  ([x+'\n'+y]|>{|sprintf '<tr>%s</tr>' ((+)|>((if ix=1 then sprintf '<th>%s</th>' else sprintf '<td>%s</td>')¨`$))})
