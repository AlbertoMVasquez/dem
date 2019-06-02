function theil_sen2, x, y
  x = float(x)
  y = float(y)
  num = n_elements(x)
  n=float(num)*(num-1)/2
  theil=fltarr(n)
  t=0
  for i=0,num-2 do begin
     for j=i+1,num-1 do begin
        theil[t]=float(y[j]-y[i])/(x[j]-x[i])
        if abs(theil[t]) eq !values.f_infinity then print,'estas dividiendo por infinito, chequea el vector x'
        t=t+1
     endfor
  endfor
  theil = theil (where(theil ne !values.f_infinity));dejamos de lado los valores infinitos
  slope=median(theil ,/even)
  intercept=median(y-slope*x, /even)
  results=[intercept, slope]
  return, results
end


