; filter must be 'Clear', 'All' or 'Al2' for EIT
; or             'S1a' 'S1b'  Opena'  'Openb' 'S2a' 'S2b' 'Dbla' 'Dblb'  for EUVI


pro cycle_Qkl,EUVIA=EUVIA,EUVIB=EUVIB,EIT=EIT,EITCLEAR=EITCLEAR,ip_1=ip_1,ip_2=ip_2
;Qkl_euvi_eit,'S1b',chianti_ver='C71',N_e=1.e10,/all,/continuum
;Qkl_euvi_eit,'S1b',chianti_ver='C71',N_e=1.e08,/all,/continuum
;Qkl_euvi_eit,'S1b',chianti_ver='C71',N_e=1.e06,/all,/continuum
;Qkl_euvi_eit,'S1b',chianti_ver='C71',N_e=1.e09,/all,/continuum
;Qkl_euvi_eit,'S1b',chianti_ver='C71',N_e=1.e07,/all,/continuum
if keyword_set(EUVIB)      then Qkl_euvi_eit,'S1b'    ,chianti_ver='C71',N_e=1.e08,/continuum,/all
if keyword_set(EUVIA)      then Qkl_euvi_eit,'S1a'    ,chianti_ver='C71',N_e=1.e08,/all,/continuum
if keyword_set(EIT)        then Qkl_euvi_eit,'Al1'    ,chianti_ver='C71',N_e=1.e08,/all,/continuum
if keyword_set(EITCLEAR)   then Qkl_euvi_eit,'Clear'  ,chianti_ver='C71',N_e=1.e08,/all,/continuum
return
end

pro Qkl_euvi_eit,filter,ip_1=ip_1,ip_2=ip_2,precision=precision,continuum=continuum,all=all,chianti_ver=chianti_ver,N_e=N_e

if not keyword_set(ip_1) then ip_1=0
if not keyword_set(ip_2) then ip_2=0
if not keyword_set(precision) then precision=1.e-3

; To get my graphs right:
;device, retain     = 2
;device, true_color = 24
;device, decomposed = 0

; Chianti Ritual steps:
basedir='/usr/local/ssw/packages/chianti/'
!path = '+'+basedir+'idl:'+!path
!path=expand_path(!path)
use_chianti,basedir+'dbase/'

; Temperatures:
Ntemp  = 200
Te_min = 0.25d6 ; K
Te_max = 4.00d6 ; K
T_string='0.25-4.00MK'
logTe=alog10(Te_min)+(alog10(Te_max)-alog10(Te_min))*findgen(Ntemp)/float(Ntemp-1) ; logT linear scale

; Passbands:
Npassband=3
    leer_passband,filter,range171,range195,range284,waves,$
                  bandpass_171,bandpass_195,bandpass_284,$
                  i171,i195,i284

 wlmin=[range171(0),range195(0),range284(0)]
 wlmax=[range171(1),range195(1),range284(1)]

;Density and EM:
if NOT keyword_set(N_e) then N_e=1.d8 ; cm^-3
N_e   = N_e*1.d ; make sure it is double
logem = alog10(N_e^2)
if N_e eq 1.d06 then Ne_suffix='Ne1E06'
if N_e eq 1.d07 then Ne_suffix='Ne1E07'
if N_e eq 1.d08 then Ne_suffix='Ne1E08'
if N_e eq 1.d09 then Ne_suffix='Ne1E09'
if N_e eq 1.d10 then Ne_suffix='Ne1E10'
if NOT   keyword_set(Ne_suffix) then STOP

; Abundance Set:
;file_abund='sun_coronal.abund'
;file_abund='sun_coronal_ext.abund'
 file_abund='sun_coronal_1992_feldman_ext.abund'
 file_abund='sun_coronal_1992_feldman_ext_Cancel-O-Mg-Si.abund'
;file_abund='sun_photospheric_2011_caffau.abund'
;file_abund='sun_photospheric.abund'
;file_abund='sun_coronal_Cancel-O-Mg-Si.abund' ; !!!
;file_abund='sun_coronal_Cancel-Si.abund' ; !!!

; Ion-eq set:
;file_ioneq='arnaud_raymond.ioneq'
;file_ioneq='arnaud_rothenflug.ioneq'
;file_ioneq='mazzotta_etal_9.ioneq'
;file_ioneq='shull_steenberg.ioneq'
;file_ioneq='bryans_etal_09.ioneq'
 file_ioneq='chianti.ioneq'

output_file=['Qkl_171_','Qkl_195_','Qkl_284_']+$
             file_ioneq+'_'+file_abund+$
             '.'+filter+'-Filter-photons-Abund'

if precision eq 1.e-2 then output_file=output_file+'-1e-2'
if precision eq 1.e-3 then output_file=output_file+'-1e-3'
if precision eq 1.e-4 then output_file=output_file+'-1e-4'
if precision eq 2.e-4 then output_file=output_file+'-2e-4'
if precision eq 5.e-4 then output_file=output_file+'-5e-4'

if keyword_set(all) then output_file=output_file+'-ALL'

if keyword_set(continuum) then output_file=output_file+'-withCONTINUUM'

output_file=output_file+'_'+Ne_suffix+'_'+chianti_ver+T_string+'.out'

print,'Doing bands:'
print,transpose([output_file(ip_1:ip_2)])

for ip=ip_1,ip_2 do begin

print,'Computing passband '+output_file(ip)
w1=wlmin(ip)
w2=max(waves)
wbs=precision ;A
instrum_fwhm=wbs
nl=((w2-w1)/wbs)*1L+1L
lambda=w1+(w2-w1)*findgen(nl)/float(nl-1)

 if ip eq 0 then PHI=interpol(bandpass_171,waves,lambda,/spline)
 if ip eq 1 then PHI=interpol(bandpass_195,waves,lambda,/spline)
 if ip eq 2 then PHI=interpol(bandpass_284,waves,lambda,/spline)

PHI=PHI*1.d

openw,11,output_file(ip)
printf,11,'Files used for ioneq and abundance:'
printf,11,file_ioneq
printf,11,file_abund
printf,11,'Ne, logEM:'
printf,11,N_e,logEM
printf,11, 'Instr-fwhm, NTemp, w1, w2, w-bin, Nlambda'
printf,11,instrum_fwhm, Ntemp, w1, w2, wbs,   nl
printf,11,'Renorm. Const for PHI:'
printf,11,max(PHI)

phi_peak1=PHI/max(PHI)  ; detector's normalized response

printf,11,'       logTe[K]  ,  Integral(d_lambda*Emissivity(lambda)*phi_peak1(lambda))/N_e^2    Sum/N_e^2'
for it=0,Ntemp    -1 do begin
print,'Temp',it+1,'/',Ntemp

; generates a list of synthetic line intensities with NO abundance factor,
; and their wavelenghts. [output]= photons cm-2 s-1 sr-1
ch_synthetic,w1,w2,logt_isothermal=logTe(it),logem_isothermal=logem,density=N_e,$
             output=results,$
             ioneq_name=basedir+'dbase/ioneq/'+file_ioneq,/photons

; from ch_synthetic output generates emissivity spectrum
; [output.spctrum]= photons cm-2 sr-1 s-1 A-1

if     keyword_set(continuum) and NOT keyword_set(all) then $
make_chianti_spec,results,lambda,output,binsize=wbs,instr_fwhm=instrum_fwhm,$
                  abund_name=basedir+'dbase/abundance/'+file_abund,/photons,/continuum

if     keyword_set(continuum) and keyword_set(all) then $
make_chianti_spec,results,lambda,output,binsize=wbs,instr_fwhm=instrum_fwhm,$
                  abund_name=basedir+'dbase/abundance/'+file_abund,/photons,/continuum,/all

if NOT keyword_set(continuum) and NOT keyword_set(all) then $
make_chianti_spec,results,lambda,output,binsize=wbs,instr_fwhm=instrum_fwhm,$
                  abund_name=basedir+'dbase/abundance/'+file_abund,/photons

if NOT keyword_set(continuum) and keyword_set(all) then $
make_chianti_spec,results,lambda,output,binsize=wbs,instr_fwhm=instrum_fwhm,$
                  abund_name=basedir+'dbase/abundance/'+file_abund,/photons,/all

; Get Fe abundance
Abundance_Fe = float(output.abund(25))

; Set Emissivty(Ne^pow) proper scaling
if ip eq 0 then pow=1.955
if ip eq 1 then pow=1.950
if ip eq 2 then pow=1.993
pow=2.

  Sum=0.d
  i=0L
  for i=0L,n_elements(output.lines)-1 do begin
    difw   = abs(LAMBDA - output.lines[i].wvl)
    indice = ( where(difw eq min(difw)) )(0)
    phi_peak1_line_i=(phi_peak1(indice))
    Sum=Sum+phi_peak1_line_i*output.lines[i].INT
 endfor

printf,11,logTe(it),$
          int_tabulated(output.lambda,output.spectrum*phi_peak1) / (N_e^pow) ,$
          Sum / (N_e^pow)

endfor;temp loop
close,11
endfor;passband loop

return
end

;--------------------------------------------------------
pro leer_passband,filter,range171,range195,range284,waves,$
                  bandpass_171,bandpass_195,bandpass_284,$
                  i171,i195,i284

;EIT filter options:
if filter eq 'Clear' then openr,1,'bandpasses_clear.txt'
if filter eq 'Al1'   then openr,1,'bandpasses_al1.txt'
if filter eq 'Al2'   then openr,1,'bandpasses_al2.txt'

;EUVI filter options:
if filter eq 'S1a'   then openr,1,'bandpasses_ahead_s1.txt'
if filter eq 'S1b'   then openr,1,'bandpasses_behin_s1.txt'
if filter eq 'S2a'   then openr,1,'bandpasses_ahead_s2.txt'
if filter eq 'S2b'   then openr,1,'bandpasses_behin_s2.txt'
if filter eq 'Opena' then openr,1,'bandpasses_ahead_open.txt'
if filter eq 'Openb' then openr,1,'bandpasses_behin_open.txt'
if filter eq 'Dbla'  then openr,1,'bandpasses_ahead_dbl.txt'
if filter eq 'Dblb'  then openr,1,'bandpasses_behin_dbl.txt'

readf,1,nw
waves       =fltarr(nw)
bandpass_171=fltarr(nw)
bandpass_195=fltarr(nw)
bandpass_284=fltarr(nw)
readf,1,waves,bandpass_171,bandpass_195,bandpass_284
close,1

;goto,skipthis
fact=5000.
i171=where(bandpass_171 gt max(bandpass_171)/fact)
i171=[min(i171)-1,i171]
i195=where(bandpass_195 gt max(bandpass_195)/fact)
i284=where(bandpass_284 gt max(bandpass_284)/fact)
;range171=[170.            ,waves(max(i171))]
 range171=[waves(min(i171)),waves(max(i171))]
 range195=[waves(min(i195)),waves(max(i195))]
 range284=[waves(min(i284)),waves(max(i284))]
o171=where(bandpass_171 le max(bandpass_171)/fact)
o195=where(bandpass_195 le max(bandpass_195)/fact)
o284=where(bandpass_284 le max(bandpass_284)/fact)
;bandpass_171(o171)=0.
;bandpass_195(o195)=0.
;bandpass_284(o284)=0.

skipthis:

return
end

pro compare_passbands
; To get my graphs right:
device, retain     = 2
device, true_color = 24
device, decomposed = 0

; Chianti Ritual steps:
basedir='/usr/local/ssw/packages/chianti/'
!path = '+'+basedir+'idl:'+!path
!path=expand_path(!path)
use_chianti,basedir+'dbase/'

; Temperatures:
Ntemp =200
Te_min=0.25d6 ; K
Te_max=4.00d6 ; K
logTe=alog10(Te_min)+(alog10(Te_max)-alog10(Te_min))*findgen(Ntemp)/float(Ntemp-1) ; logT linear scale

filter='Clear'
Npassband=3
    leer_passband,filter,range171,range195,range284,waves_clear,$
                  bandpass_171_clear,bandpass_195_clear,bandpass_284_clear,$
                  i171,i195,i284
wlmin=[range171(0),range195(0),range284(0)]
wlmax=[range171(1),range195(1),range284(1)]

filter='Al1'
    leer_passband,filter,range171,range195,range284,waves_al1,$
                  bandpass_171_al1,bandpass_195_al1,bandpass_284_al1,$
                  i171,i195,i284

filter='Al2'
    leer_passband,filter,range171,range195,range284,waves_al2,$
                  bandpass_171_al2,bandpass_195_al2,bandpass_284_al2,$
                  i171,i195,i284

    filter='Opena'
Npassband=3
    leer_passband,filter,range171,range195,range284,waves_opena,$
                  bandpass_171_opena,bandpass_195_opena,bandpass_284_opena,$
                  i171,i195,i284
wlmin=[range171(0),range195(0),range284(0)]
wlmax=[range171(1),range195(1),range284(1)]

filter='S1a'
    leer_passband,filter,range171,range195,range284,waves_s1a,$
                  bandpass_171_s1a,bandpass_195_s1a,bandpass_284_s1a,$
                  i171,i195,i284

filter='S2a'
    leer_passband,filter,range171,range195,range284,waves_s2a,$
                  bandpass_171_s2a,bandpass_195_s2a,bandpass_284_s2a,$
                  i171,i195,i284

filter='Dbla'
    leer_passband,filter,range171,range195,range284,waves_dbla,$
                  bandpass_171_dbla,bandpass_195_dbla,bandpass_284_dbla,$
                  i171,i195,i284

    filter='Openb'
Npassband=3
    leer_passband,filter,range171,range195,range284,waves_openb,$
                  bandpass_171_openb,bandpass_195_openb,bandpass_284_openb,$
                  i171,i195,i284
wlmin=[range171(0),range195(0),range284(0)]
wlmax=[range171(1),range195(1),range284(1)]

filter='S1b'
    leer_passband,filter,range171,range195,range284,waves_s1b,$
                  bandpass_171_s1b,bandpass_195_s1b,bandpass_284_s1b,$
                  i171,i195,i284

filter='S2b'
    leer_passband,filter,range171,range195,range284,waves_s2b,$
                  bandpass_171_s2b,bandpass_195_s2b,bandpass_284_s2b,$
                  i171,i195,i284

filter='Dblb'
    leer_passband,filter,range171,range195,range284,waves_dblb,$
                  bandpass_171_dblb,bandpass_195_dblb,bandpass_284_dblb,$
                  i171,i195,i284

stop
window,0,xs=400,ys=1000
!p.multi=[0,1,3]
!p.charsize=2
ps1,'eit-euvi-passbands.eps',0

 c=max([bandpass_171_clear,bandpass_171_s1a,bandpass_171_s1b])

 plot,waves_clear,bandpass_171_clear/c,xr=[165,185],yr=[0,1],$
      title='171 A Passbands [normalized units]'
 c=max(bandpass_171_s1a)
oplot,waves_s1a,bandpass_171_s1a/c,linestyle=2
 c=max(bandpass_171_s1b)
oplot,waves_s1b,bandpass_171_s1b/c,linestyle=3

oplot,[1,1.2]*185
oplot,[1,1.2]*185
oplot,[1,1.2]*185

;xyouts=[],[],[],[,'']

 c=1.;max(bandpass_195_clear)
 plot,waves_clear,bandpass_195_clear/c,xr=[180,220],yr=[0,.07],$
      title='195 A Passbands [normalized units]'
 c=1.;max(bandpass_195_s1a)
oplot,waves_s1a,bandpass_195_s1a/c,linestyle=2
 c=1.;max(bandpass_195_s1b)
oplot,waves_s1b,bandpass_195_s1b/c,linestyle=3

 c=1.;max(bandpass_284_clear)
 plot,waves_clear,bandpass_284_clear/c,xr=[260,300],yr=[0.,.01],$
      title='284 A Passbands [normalized units]',xtitle='!4k!3 [A]'
 c=1.;max(bandpass_284_s1a)
oplot,waves_s1a,bandpass_284_s1a/c,linestyle=2
 c=1.;max(bandpass_284_s1b)
oplot,waves_s1b,bandpass_284_s1b/c,linestyle=3

!p.multi=0
ps2

stop

!p.charsize=1.5
ps1,'eit284-passbands-3filtpos.eps',0                                     
plot,waves_clear,bandpass_284_clear/max(bandpass_284_clear),xr=[260,290],$
title='EIT 284 Passbands [normalized units]',$                            
xtitle='!4k!3 [A]'                                                        
oplot,waves_Al1,bandpass_284_Al1/max(bandpass_284_Al1),linestyle=2        
oplot,waves_Al2,bandpass_284_Al2/max(bandpass_284_Al2),linestyle=3        
ps2                                                                       

return
end
