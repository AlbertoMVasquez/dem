pro franja_lineal,y,salidafit,error,fraccion
;+ 
;Chequea cuanto porcentaje de datos esta dentro de la franja
;dada entre la recta fiteada +- los errores
;-  
 
;  p=where(y ge rmin and y le rmax)
;  y = y(p)
  if n_elements(y) ne n_elements(salidafit) then stop
  n = n_elements(y)
  yru = salidafit + error
  yrd = salidafit - error
  
  Yin = fltarr(n)-100.
  for i=0,n-1 do begin
     if y(i) ge yrd(i) and y(i) le yru(i) then  Yin(i) = 1. 
     if y(i) lt yrd(i) or  y(i) gt yru(i) then  Yin(i) = 0.
  endfor
  nok = where(Yin eq 1.)
  fraccion = float(n_elements(Nok))/float(n)
  return
end
