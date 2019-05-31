pro tests,gradT=gradT,Tb=Tb,N=N,rmin=rmin,rmax=rmax,Ar=Ar,AT=AT,AO=AO,Nout=Nout

if not keyword_set(gradT) then gradT =  0.   ; MK / Rsun
if not keyword_set(Tb)    then Tb    =  1.   ; MK / Rsun 
if not keyword_set(N)     then N     = 10
if not keyword_set(rmin)  then rmin  = 1.035 ; Rsun
if not keyword_set(rmax)  then rmax  = 1.200 ; Rsun
if not keyword_set(Ar)    then Ar    = 0.
if not keyword_set(AT)    then AT    = 0.
if not keyword_set(AO)    then AO    = 0.
if not keyword_set(Nout)  then Nout  = 0

print,gradT,Tb,N,rmin,rmax,Ar,AT,Nout

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

A=theil_sen(r,T)
stop
MAXY= max([max(T),2.0])

 plot,r,T,psym=4,ystyle=3,xstyle=3,charsize=2,th=2,yr=[0.,maxy]
oplot,r,a1*r+b1


return
end
