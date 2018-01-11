

thebox = [50,-50,200,0]
latmax    = thebox(0)
latmin    = thebox(1)
lonmax    = thebox(2)
lonmin    = thebox(3)

;Histograma piernas pertenecientes a loop cerrados, /datas1 indica que
;es con 1914 es decir el primer filesT que cargó
j = where (data1.footlat le latmax and data1.footlat gt latmin and data1.footlon le lonmax and data1.footlon gt lonmin) ;Dentro de caja 
i = where (gradT(j) ne -555 and  opclstat(j) gt 0 and r2N(j) ge 0.95 )
filter2,i,/datas1
histoplot,  Ne0_c/1.e8, min=0,max=5.0,nbins=100,xtit='N!De0!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Densidad con buen ajuste',filename='crapNe022'
histoplot,  lambda_N_c, min=0,max=0.4,nbins=100,xtit='N!De0!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Lambda_N con buen ajuste',filename='craplambda_N22'

;i = where (gradT ne -555 and  opclstat eq 1 and r2P ge 0.95 )
;filter2,i,/datas1
;histoplot,  P0_c, min=0,max=0.4,nbins=100,xtit='N!De0!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Presion con buen ajuste',filename='crapP0'

;i = where (gradT ne -555 and  opclstat eq 1 and r2T ge 0.95 ) 
;filter,i,/datas1
;histoplot,  Tm0_c/1.e8, min=0,max=0.3,nbins=100,xtit='N!De0!N [10!U8!Ncm!U-3!N]',ytit='Frequency Histogram',tit='Temp con buen ajuste',filename='crapTe0'


; comments:

; la subindicacion i,j no es correcta pues asi i subindica al
; SUB-ARRAY que fue seleccionado con j, pero luego i es aplicado a los
; ARRAYS.

; siempre usar el footpointmap para VER si los histogramas miden la
; estadistica de lo que uno se supone eligió.

; no poner cosas como "con buen ajuste" en titulos.Eso se discute en
; texto. La figura debe ser mucho mas "seca".
