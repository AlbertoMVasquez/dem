pro tests,gradT=gradT,Tb=Tb,Nd=Nd,rmin=rmin,rmax=rmax,Ar=Ar,AT=AT,AO=AO,Nout=Nout

if not keyword_set(gradT) then gradT =  0.   ; MK / Rsun
if not keyword_set(Tb)    then Tb    =  1.   ; MK
if not keyword_set(Nd)    then Nd    = 10
if not keyword_set(rmin)  then rmin  = 1.035 ; Rsun
if not keyword_set(rmax)  then rmax  = 1.200 ; Rsun
if not keyword_set(Ar)    then Ar    = 0.
if not keyword_set(AT)    then AT    = 0.
if not keyword_set(AO)    then AO    = 0.
if not keyword_set(Nout)  then Nout  = 0

N=Nd

m=gradT
b=Tb-m*1.025 ; MK

r = rmin + (rmax-rmin) * findgen(N)/float(N-1)
T = m * r + b

ruido_r = randomn(seed,N) * Ar
ruido_T = randomn(seed,N) * AT

r = r + ruido_r
T = T + ruido_T

if Nout ne 0 then begin
iout = fix(abs(randomn(seed,Nout)*N/2))
T(iout) = T(iout) + randomn(seed,Nout) * AO  > 0.25 < 3.
endif

theil_sen,r,T,a1,b1,r2
fitTemp,r,T,A,r22

Tfit = a1*r+b1

MAXY= max([max(T),2.0])

 plot,r,T,psym=4,ystyle=3,xstyle=3,charsize=2,th=2,yr=[0.,maxy]
oplot,r,Tfit
oplot,r,A[0]+A[1]*r,linestyle=2

eps=0.2

loadct,12
oplot,r,Tfit+eps,color=200
oplot,r,Tfit-eps,color=200
loadct,0

DT = abs(T-Tfit)
Nok=where(DT le eps)

meanyy= mean(T)
SStot = total( (T-meanyy)^2 )
SSerr = total( (T-Tfit  )^2 )
r2    = 1.-SSerr/SStot

xyouts,[0.75],[0.2],['F  ='+strmid(string(float(n_elements(Nok))/N),4,5)],/normal,charsize=2
xyouts,[0.75],[0.3],['r2 ='+strmid(string(float(r2 )),3,5)],/normal,charsize=2
xyouts,[0.75],[0.4],['r22='+strmid(string(float(r22)),3,5)],/normal,charsize=2

return
end


pro fitTemp,rr,yy,A,r22
rr=rr
yy=yy
A = linfit(rr,yy)
fit =  A[0]+A[1]*rr
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r22   = 1.-SSerr/SStot
return
end
