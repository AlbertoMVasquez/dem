pro test

;x=[1,2,3,4,5,6,7,8,9,10,11,3,12,14,5,15,4,2,7,25,28,35,27,32,29,36,25,32,27,30]
;y=[12,15,19,24,65,23,75,13,35,16,34,23,65,24,34,29,30,36,46,38,45,67,46,42,33,26,44,56,50,30]

x1=randomu(seed,50)
y1=randomu(seed,50);,GAMMA=2)

x2=randomu(seed,50)
y2=randomu(seed,50,GAMMA=2)

x3=randomu(seed,50)
y3=randomu(seed,50,GAMMA=2)

x=x1+x2+x3
y=y1+y2+y3

x=[1,2,3,4,5,6,7,8,9]
y=[1,2,3,4,5,6,7,8,20]

y=[20,12,14,16.5,18,20,23]                 
x=findgen(7)   

;y=[1,3,2,4,5]
;x=[1,2,3,4,6]

;x=[1,2,3,4,5]
;y=[5,1,2,3,4]

ts2,x,y,a1,b1
fitTemp,x,y,A,r2

plot,x,y,psym=4,th=5;,xr=[0,6],yr=[-1,6]
oplot,x,a1*x+b1,th=2
oplot,x,A[1]*x+A[0],linestyle=2

stop

return
end

pro ts2,x,y,a1,b1,rn,yrd,yru
 n = n_elements(x)
nn = n*(n-1)*0.5 

xx1 = x#replicate(1,n)
yy1 = y#replicate(1,n)

xx = xx1
yy = yy1


yb = fltarr(n,n)-100.
xb = fltarr(n,n)-100.
ts = fltarr(n,n)-100.
bb = fltarr(n)-100.

for j=0,n-1 do begin
   for i=0,n-1 do begin
      if j gt i then begin
      xb(j,i) = xx(j,i)-x(i)
      yb(j,i) = yy(j,i)-y(i)
      ts(j,i) = yb(j,i)/xb(j,i)
      endif else begin
         if j lt i then ts(j,i)=0
      endelse 
   endfor
endfor

for j=0,n-1 do begin
   for i=0,n-1 do begin
      if j gt i then begin
         ts(j,i) = ts(j,i)
      endif else begin
         if j le i then ts(j,i)=0
      endelse 
   endfor
endfor

;stop
ts1 = ts[where(ts ne 0)]
a1 = median(ts1) 

for i=0,n-1 do begin
   bb(i)=y(i)-a1*x(i)
endfor

b1 = median(bb) 

;Distances
  Dy = fltarr(n)-100.
yfit = fltarr(n)-100.

for i=0,n-1 do begin
   yfit(i) = a1*x(i)+b1
     Dy(i) = y(i)-yfit(i)
endfor
Dm = 1-abs(median(Dy))
;print,Dy
;print,Dm

;range
yru = fltarr(n)-100.
yrd = fltarr(n)-100.

for i=0,n-1 do begin
   yru(i) = yfit(i) + yfit(i)/10  
   yrd(i) = yfit(i) - yfit(i)/10  
endfor

;in and out
 Yin = fltarr(n)-100.
Yout = fltarr(n)-100.

for i=0,n-1 do begin
   if y(i) gt yrd(i) and y(i) lt yru(i) then begin
       Yin(i) = y(i)
   endif else begin
      if y(i) lt yrd(i) or y(i) gt yru(i) then Yout(i) = y(i)
   endelse
endfor

;elements
 nin = n_elements(where( yin ne -100))
nout = n_elements(where(yout ne -100))

 nin = float(nin)
nout = float(nout)
   n = float(n)

rn = 1-(nout/n)

print,nin,nout,rn

stop

return
end 

pro fitTemp,rr,yy,A,r2
rr=rr
yy=yy
A = linfit(rr,yy)
fit =  A[0]+A[1]*rr
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot
return
end
