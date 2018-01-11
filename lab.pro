pro lab,irad

Nph=180
Nth= 90

ilat=indgen(Nth)
ilon=indgen(Nph)

lab =fltarr(nth,nph)
lab2=fltarr(nth,nph)

; crear el LAB 2D correcto en irad
Np=180
for il=0,nph-1 do for it=0,nth-1 do lab (it,il) = (Nth*Np)*irad+(Np)*ilat(it)+ilon(il)+1

; crear el LAB 2D IN-correcto (con bug) en irad
Np=  2
for il=0,nph-1 do for it=0,nth-1 do lab2(it,il) = (Nth*Np)*irad+(Np)*ilat(it)+ilon(il)+1

; Imprimir de lab con ith=0 y luego con ith=1 (primer y segunda latitudes)

print,reform( lab(0,*))
print,'.........................................................'
print,reform( lab(1,*))

print,''
print,''
print,''
print,''
print,''


; Imprimir de lab2 con ith=0 y luego con ith=1 (primer y segunda latitudes)
print,reform( lab2(0,*))
print,'.........................................................'
print,reform( lab2(1,*))

; visualizar en colores mapas de LAB y LAB2
loadct,39
window,0,xs=Nth,ys=Nph
tvscl,lab

window,1,xs=Nth,ys=Nph
tvscl,lab2

stop
end
