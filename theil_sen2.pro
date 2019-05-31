function theil_sen2, x, y
  num = n_elements(x)
  n=float(num)*(num-1)/2
  theil=fltarr(n)
  t=0
  for i=0,num-2 do begin
     for j=i+1,num-1 do begin
        theil[t]=float(y[j]-y[i])/(x[j]-x[i])
        t=t+1
     endfor
  endfor
  infin = !values.f_infinity
  ok = where(theil eq infin)   
  if ok(0) ne -1. then stop
  slope=median(theil ,/even)
  intercept=median(y-slope*x, /even)
  results=[intercept, slope]
  return, results
end


