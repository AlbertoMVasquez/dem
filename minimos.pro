;------------------------------------------------------------------
; This is a wrapper to do a error box analysis for DEMT results of the
; two last solar minima.
;------------------------------------------------------------------

pro ldem_errorbox_minimos,levels=levels,regstr=regstr

; ESTA RUTINA ESTA SIENDO DESARROLLADA POR ALBERTO. 
; NO ESTARA FUNCIONAL HASTA QUE SE BORRE ESTE MENSAJE.

  nr   = 26                     ; Number of tomographic radial bins
  nth  = 90                     ; Number of latitudinal bins
  npx  = 1024                   ; Number of Image pixels, for DEM.
  CRstring='.CR2081'            ; CR's suffix

     Tmin = 0.5                        ; MK
     Tmax = 3.5                        ; MK
     L    = 171   &   Lstring='_171'   ; Number of Temp bins
     bandsindexes = [0,1,2]   
;    Expstring='_AIA3'

  dir    = '/data1/tomography/bindata/' ;Tomography directory

  ; the FBE files    
  datafiles=[ 'x_EUVIB'+CRstring+'.171.nr26.irm1.26_',$
              'x_EUVIB'+CRstring+'.195.nr26.irm1.26_',$
              'x_EUVIB'+CRstring+'.284.nr26.irm1.26_']

  if not keyword_set(regstr) then regstr='base'
  datafiles=datafiles+regstr

; este sentencia reemplaza el bloque "OBSOLETO-1" que le sigue si el contenido
; de la variable "levels" son mayusculas. Ejemplo:      levels = ['H','H','L']
  if keyword_set(levels) then datafiles = datafiles + '.' + levels
;-----------------------------------------------------------------------------------------------
; OBSOLETO-1
  goto,skip1
  if keyword_set(levels) then begin
     if levels(0) eq 'h' then datafiles(0)=datafiles(0)+'.H'
     if levels(0) eq 'l' then datafiles(0)=datafiles(0)+'.L'
     if levels(1) eq 'h' then datafiles(1)=datafiles(1)+'.H'
     if levels(1) eq 'l' then datafiles(1)=datafiles(1)+'.L'
     if levels(2) eq 'h' then datafiles(2)=datafiles(2)+'.H'
     if levels(2) eq 'l' then datafiles(2)=datafiles(2)+'.L'
     finalname='ldem'+CRstring+Expstring+'_'+levels(0)+levels(1)+levels(2)+'.'+regstr+'.dat'
  endif
  skip1:
;-----------------------------------------------------------------------------------------------

  if not keyword_set(levels) and regstr eq 'base' then begin
     datafiles=datafiles+'.G'
     ; este es el nombre final del archivo ldem
     finalname='ldem'+CRstring+Expstring+'_'+'base'+'.dat'
  endif

;-- voy por aqui

  ; The TRF
  file_ioneq='chianti.ioneq'
  file_abund='sun_coronal_1992_feldman_ext.abund'
  qklfiles=['Qkl_171_','Qkl_195_','Qkl_284_',']+$
             file_ioneq+'_'+file_abund+$
           '.AIA-lastoptimizedWL-photons-Abund-1e-3-ALL-withCONTINUUM_Ne1E08_C71.extended.interpolated.out'
  suffix = 'v2'+CRstring+'_'+file_ioneq+'_'+file_abund+Expstring+Lstring
  binfactor=2
  fact     =1

  if keyword_set(gauss1) then $
     ldem_V2,Tmin,Tmax,datafiles,qklfiles,bandsindexes,suffix,dir,L,nr,nth,npx,binfactor,fact,$
             /aia,/norm_median,/ldem,/gauss1,/single,/linear,finalname=finalname 
   
  if keyword_set(dgfw)   then $
     ldem_V2,Tmin,Tmax,datafiles,qklfiles,bandsindexes,suffix,dir,L,nr,nth,npx,binfactor,fact,$
             /aia,/norm_median,/ldem,/dgfw,/single,/linear,sustrato=sustrato,finalname=finalname 
  return
end

;------------------------------------------------------------------
; This is a wrapper to do a error box analysis for DEMT. Also, works
; to do the DEMT base state.
;------------------------------------------------------------------

pro ldem_errorbox,gauss1=gauss1,dgfw=dgfw,levels=levels,regstr=regstr
  
  nr   = 26                     ; Number of tomographic radial bins
  nth  = 90                     ; Number of latitudinal bins
  npx  = 1024                   ; Number of Image pixels, for DEM.
  CRstring='_cr2099'            ; CR's suffix

  if keyword_set(gauss1) then begin
     Tmin = 0.5                        ; MK
     Tmax = 3.5                        ; MK
     L    = 171   &   Lstring='_171'   ; Number of Temp bins
     bandsindexes = [0,1,2]   
     Expstring='_AIA3'
  endif

  if keyword_set(dgfw) then begin
     Tmin = 0.5                        ; MK
     Tmax = 5.0                        ; MK
     L    = 256   &   Lstring='_256'   ; Number of Temp bins
     bandsindexes = [0,1,2,3]   
     Expstring='_AIA4'
    endif

  dir    = '/data1/tomography/bindata/' ;Tomography directory

  ; the FBE files    
  datafiles=[ 'x_AIA.CR2099.171.nr26.irm1.26_',$
              'x_AIA.CR2099.193.nr26.irm1.26_',$
              'x_AIA.CR2099.211.nr26.irm1.26_',$
              'x_AIA.CR2099.335.nr26.irm1.26_' ]
  if not keyword_set(regstr) then regstr='base'
  datafiles=datafiles+regstr
  
  if keyword_set(levels) then begin
     if levels(0) eq 'h' then datafiles(0)=datafiles(0)+'.H'
     if levels(0) eq 'l' then datafiles(0)=datafiles(0)+'.L'
     if levels(1) eq 'h' then datafiles(1)=datafiles(1)+'.H'
     if levels(1) eq 'l' then datafiles(1)=datafiles(1)+'.L'
     if levels(2) eq 'h' then datafiles(2)=datafiles(2)+'.H'
     if levels(2) eq 'l' then datafiles(2)=datafiles(2)+'.L'
     if keyword_set(dgfw) then begin
        if levels(3) eq 'h' then datafiles(3)=datafiles(3)+'.H'
        if levels(3) eq 'l' then datafiles(3)=datafiles(3)+'.L'
     endif
       ; este archivo se utiliza como sustrato en la corrida de AIA-4/2-normal 
       sustrato='ldem'+CRstring+'_AIA3'+'_'+levels(0)+levels(1)+levels(2)+'.'+regstr+'.dat'
       ;este es el nombre final del archivo ldem
       if keyword_set(gauss1) then $
          finalname='ldem'+CRstring+Expstring+'_'+levels(0)+levels(1)+levels(2)+'.'+regstr+'.dat'
       if keyword_set(dgfw)   then $
          finalname='ldem'+CRstring+Expstring+'_'+levels(0)+levels(1)+levels(2)+levels(3)+'.'+regstr+'.dat'
  endif

  if not keyword_set(levels) and regstr eq 'base' then begin
     datafiles=datafiles+'.G'
     ; este archivo se utiliza como sustrato en la corrida de AIA-4/2-normal 
     sustrato='ldem'+CRstring+'_AIA3_'+'base'+'.dat'
     ; este es el nombre final del archivo ldem
     if keyword_set(gauss1) then $
        finalname='ldem'+CRstring+Expstring+'_'+'base'+'.dat'
     if keyword_set(dgfw)   then $
        finalname='ldem'+CRstring+Expstring+'_'+'base'+'.dat'
  endif

  ; The TRF
  file_ioneq='chianti.ioneq'
  file_abund='sun_coronal_1992_feldman_ext.abund'
  qklfiles=['Qkl_171_','Qkl_193_','Qkl_211_','Qkl_335_']+$
             file_ioneq+'_'+file_abund+$
           '.AIA-lastoptimizedWL-photons-Abund-1e-3-ALL-withCONTINUUM_Ne1E08_C71.extended.interpolated.out'
  suffix = 'v2'+CRstring+'_'+file_ioneq+'_'+file_abund+Expstring+Lstring
  binfactor=2
  fact     =1

  if keyword_set(gauss1) then $
     ldem_V2,Tmin,Tmax,datafiles,qklfiles,bandsindexes,suffix,dir,L,nr,nth,npx,binfactor,fact,$
             /aia,/norm_median,/ldem,/gauss1,/single,/linear,finalname=finalname 
   
  if keyword_set(dgfw)   then $
     ldem_V2,Tmin,Tmax,datafiles,qklfiles,bandsindexes,suffix,dir,L,nr,nth,npx,binfactor,fact,$
             /aia,/norm_median,/ldem,/dgfw,/single,/linear,sustrato=sustrato,finalname=finalname 
  return
end
