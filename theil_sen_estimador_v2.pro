function theil_sen, x, y
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
  slope=median(theil ,/even)
  intercept=median(y-slope*x, /even)
  results=[intercept, slope]
  return, results
end


function theil_sen, x, y
num = n_elements(x)
n = long64(num)*(num-1)/2
theil = replicate(y[0], n)
t = 0ll
; Loop over every pair of points:
for i = 0l, num-2 do begin
; Vectorized inner loop:
j = lindgen(num-i-1)+i+1
theil[t] = (y[j]-y[i]) / (x[j]-x[i])
t += num-i-1
endfor
slope = median(theil, /even)
intercept = median(y – slope*x, /even)
results = [intercept, slope]
return, results
end

;VER COMO METER P_VALUE ACA
