function theil_sen,x,y
  n=n_elements(x)
  nn=n*(n-1)*0.5
  xx1=x#replicate(1,n)
  yy1=y#replicate(1,n)
  xx=xx1
  yy=yy1
  yb=fltarr(n,n)-100.
  xb=fltarr(n,n)-100.
  ts=fltarr(n,n)-100.
  bb=fltarr(n)-100.
  for j=0,n-1 do begin
     for i=0,n-1 do begin
        if j gt i then begin
           xb(j,i)=xx(j,i)-x(i)
           yb(j,i)=yy(j,i)-y(i)
           ts(j,i)=yb(j,i)/xb(j,i);floating divide by 0 chequear!!
           stop;quizas deba hacerlos float?
        endif else begin
           if j lt i then ts(j,i)=0
        endelse
     endfor
  endfor
  for j=0,n-1 do begin
     for i=0,n-1 do begin
        if j gt i then begin
           ts(j,i)=ts(j,i)
        endif else begin
           if j le i then ts(j,i)=0
        endelse
     endfor
  endfor
  ts1=ts[where(ts ne 0)]
  infin = !values.f_infinity
  print, n_elemets(ts1);chequeando si hay valores nulos
  ok = where(ts1 ne infin)
  print, n_elements(ok) ;chequeando si hay valores infinitos
  a1=median(ts1,/even) 
  for i=0,n-1 do begin
     bb(i)=y(i)-a1*x(i)
  endfor
  b1=median(bb,/even) 
  stop
  return,[b1,a1]
end
