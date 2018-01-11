pro test
common test_common,test_flag
test_flag=1
cycle_Qkl,/b94 ,Ntemp=400,Nosum=1,Te_min=0.1e6,Te_max=15.e6
cycle_Qkl,/b131,Ntemp=400,Nosum=1,Te_min=0.1e6,Te_max=15.e6
cycle_Qkl,/b171,Ntemp=400,Nosum=1,Te_min=0.1e6,Te_max=15.e6
cycle_Qkl,/b193,Ntemp=400,Nosum=1,Te_min=0.1e6,Te_max=15.e6
cycle_Qkl,/b211,Ntemp=400,Nosum=1,Te_min=0.1e6,Te_max=15.e6
cycle_Qkl,/b335,Ntemp=400,Nosum=1,Te_min=0.1e6,Te_max=15.e6
return
end

pro tarea_A
common test_common,test_flag
test_flag=0
cycle_Qkl,/b94 ,Ntemp=135,Nosum=1,Te_min=15.e6,Te_max=20.e6
cycle_Qkl,/b131,Ntemp=135,Nosum=1,Te_min=15.e6,Te_max=20.e6
return
end

pro tarea_B
common test_common,test_flag
test_flag=0
cycle_Qkl,/b171,Ntemp=135,Nosum=1,Te_min=15.e6,Te_max=20.e6
cycle_Qkl,/b193,Ntemp=135,Nosum=1,Te_min=15.e6,Te_max=20.e6
return
end

pro tarea_C
common test_common,test_flag
test_flag=0
cycle_Qkl,/b211,Ntemp=135,Nosum=1,Te_min=15.e6,Te_max=20.e6
cycle_Qkl,/b335,Ntemp=135,Nosum=1,Te_min=15.e6,Te_max=20.e6
return
end

pro tarea_335_A
common test_common,test_flag
test_flag=0
cycle_Qkl,/b335,Ntemp=25,Nosum=1,Te_min=1.38d6,Te_max=2.27d6,suffix='.peakA'
return
end

pro tarea_335_B
common test_common,test_flag
test_flag=0
cycle_Qkl,/b335,Ntemp=25,Nosum=1,Te_min=2.27d6,Te_max=3.16d6,suffix='.peakB'
return
end

pro tarea_335_C
common test_common,test_flag
test_flag=0
cycle_Qkl,/b335,Ntemp=25,Nosum=1,Te_min=3.16d6,Te_max=4.05d6,suffix='.peakC'
return
end

pro tarea_335_D
common test_common,test_flag
test_flag=0
cycle_Qkl,/b335,Ntemp=25,Nosum=1,Te_min=4.05d6,Te_max=4.94d6,suffix='.peakD'
return
end

pro cycle_Qkl,b94=b94,b131=b131,b171=b171,b193=b193,b211=b211,b335=b335,Ntemp=Ntemp,Nosum=Nosum,Te_min=Te_min,Te_max=Te_max,suffix=suffix
common test_common,test_flag
;Qkl_aia,chianti_ver='C71',N_e=1.e10,/all,/continuum
;Qkl_aia,chianti_ver='C71',N_e=1.e08,/all,/continuum
;Qkl_aia,chianti_ver='C71',N_e=1.e06,/all,/continuum
;Qkl_aia,chianti_ver='C71',N_e=1.e09,/all,/continuum
;Qkl_aia,chianti_ver='C71',N_e=1.e07,/all,/continuum

if NOT keyword_set(Nosum) then Nosum=0

if keyword_set(b94)  then Qkl_aia,ip_1=0,ip_2=0,chianti_ver='C71',N_e=1.e08,/all,/continuum,Ntemp=Ntemp,Nosum=Nosum,Te_min=Te_min,Te_max=Te_max,suffix=suffix
if keyword_set(b131) then Qkl_aia,ip_1=1,ip_2=1,chianti_ver='C71',N_e=1.e08,/all,/continuum,Ntemp=Ntemp,Nosum=Nosum,Te_min=Te_min,Te_max=Te_max,suffix=suffix
if keyword_set(b171) then Qkl_aia,ip_1=2,ip_2=2,chianti_ver='C71',N_e=1.e08,/all,/continuum,Ntemp=Ntemp,Nosum=Nosum,Te_min=Te_min,Te_max=Te_max,suffix=suffix
if keyword_set(b193) then Qkl_aia,ip_1=3,ip_2=3,chianti_ver='C71',N_e=1.e08,/all,/continuum,Ntemp=Ntemp,Nosum=Nosum,Te_min=Te_min,Te_max=Te_max,suffix=suffix
if keyword_set(b211) then Qkl_aia,ip_1=4,ip_2=4,chianti_ver='C71',N_e=1.e08,/all,/continuum,Ntemp=Ntemp,Nosum=Nosum,Te_min=Te_min,Te_max=Te_max,suffix=suffix
if keyword_set(b335) then Qkl_aia,ip_1=5,ip_2=5,chianti_ver='C71',N_e=1.e08,/all,/continuum,Ntemp=Ntemp,Nosum=Nosum,Te_min=Te_min,Te_max=Te_max,suffix=suffix

return
end

pro Qkl_aia,ip_1=ip_1,ip_2=ip_2,precision=precision,continuum=continuum,all=all,chianti_ver=chianti_ver,N_e=N_e,Ntemp=Ntemp,Nosum=Nosum,Te_min=Te_min,Te_max=Te_max,suffix=suffix
common test_common,test_flag

if not keyword_set(ip_1) then ip_1=0
if not keyword_set(ip_2) then ip_2=0
if not keyword_set(precision) then precision=1.e-3

if not keyword_set(chianti_ver) then stop

;To get my graphs right:
;device, retain     = 2
;device, true_color = 24
;device, decomposed = 0

; Chianti Ritual steps:
basedir='/usr/local/ssw/packages/chianti/'
!path = '+'+basedir+'idl:'+!path
!path=expand_path(!path)
use_chianti,basedir+'dbase/'

; Setup Temperature Grid:
 if not keyword_set(Ntemp)  then Ntemp =  400
 if not keyword_set(Te_min) then Te_min=  0.10d6 ; K
 if not keyword_set(Te_max) then Te_max= 20.00d6 ; K
 Te_min=Te_min*1.d
 Te_max=Te_max*1.d
 logTe=alog10(Te_min)+(alog10(Te_max)-alog10(Te_min))*findgen(Ntemp)/float(Ntemp-1) ; logT linear scale

; If Testing select only a few temperature points from the grid
 if test_flag eq 1 then begin
 logTe = logTe( indgen(5)*80  ) 
 Ntemp=n_elements(logTe)
 endif

; Passbands:
Npassband=6

    leer_passband,range094,range131,range171,range193,range211,range335,waves,$
                  Phi_094,Phi_131,Phi_171,Phi_193,Phi_211,Phi_335,$
                  i094,i131,i171,i193,i211,i335

;Specified ranges including secondary peaks and tails:
 newsuffix='lastoptimizedWL'
 wlmin=[ 85, 110, 160, 170, 170, 110]
 wlmax=[100, 150, 190, 250, 300, 400]

;Expanded ranges including secondary peaks and tails:
 newsuffix='fullWL'
 wlmin = min(waves) + fltarr(6)
 wlmax = max(waves) + fltarr(6)

 print,'Wmin',wlmin
 print,'Wmax',wlmax

 print,'Ntemp:',Ntemp

 print,'Tmin:',Te_min
 print,'Tmax:',Te_max

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

;Abundance Set:
 file_abund='sun_coronal.abund'
 file_abund='sun_coronal_ext.abund'
 file_abund='sun_coronal_1992_feldman_ext.abund'
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

output_file=['Qkl_094_','Qkl_131_','Qkl_171_','Qkl_193_','Qkl_211_','Qkl_335_']+$
             file_ioneq+'_'+file_abund+$
             '.'+'AIA-'+newsuffix+'-photons-Abund'

if precision eq 1.e-2 then output_file=output_file+'-1e-2'
if precision eq 1.e-3 then output_file=output_file+'-1e-3'
if precision eq 1.e-4 then output_file=output_file+'-1e-4'
if precision eq 2.e-4 then output_file=output_file+'-2e-4'
if precision eq 5.e-4 then output_file=output_file+'-5e-4'

if keyword_set(all) then output_file=output_file+'-ALL'

if     keyword_set(continuum) then output_file=output_file+'-withCONTINUUM'
if NOT keyword_set(continuum) then output_file=output_file

output_file=output_file+'_'+Ne_suffix+'_'+chianti_ver

if Nosum eq 1 then output_file=output_file+'.Nosum'

output_file=output_file+suffix+'.out'

print,'Doing bands:'
print,transpose([output_file(ip_1:ip_2)])

for ip=ip_1,ip_2 do begin ; Do selected bands

print,'Computing passband '+output_file(ip)
w1=wlmin(ip)
w2=wlmax(ip)
wbs=precision ;A
instrum_fwhm=wbs
nl=((w2-w1)/wbs)*1L+1L
lambda=w1+(w2-w1)*findgen(nl)/float(nl-1)

if ip eq 0 then PHI=interpol(Phi_094,waves,lambda,/spline)
if ip eq 1 then PHI=interpol(Phi_131,waves,lambda,/spline)
if ip eq 2 then PHI=interpol(Phi_171,waves,lambda,/spline)
if ip eq 3 then PHI=interpol(Phi_193,waves,lambda,/spline)
if ip eq 4 then PHI=interpol(Phi_211,waves,lambda,/spline)
if ip eq 5 then PHI=interpol(Phi_335,waves,lambda,/spline)

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

phi_peak1=PHI/max(PHI)  ; detector normalized response

printf,11,'       logTe[K]     Integral(d_lambda*Emissivity(lambda)*phi_peak1(lambda))/N_e^2    Sum/N_e^2'

for it=0,Ntemp    -1 do begin
print,'Temp',it+1,'/',Ntemp

; "ch_synthetic "generates a list of synthetic line intensities with NO abundance factor,
; and their wavelenghts. [output]= photons cm-2 s-1 sr-1
; From "ch_synthetic" output, "make_chianti_spec" generates emissivity spectrum
; [output.spctrum]= photons cm-2 sr-1 s-1 A-1
if not keyword_set(all) then begin
ch_synthetic,w1,w2,logt_isothermal=logTe(it),logem_isothermal=logem,density=N_e,$
             output=results,$
             ioneq_name=basedir+'dbase/ioneq/'+file_ioneq,/photons

if     keyword_set(continuum) then $
make_chianti_spec,results,lambda,output,binsize=wbs,instr_fwhm=instrum_fwhm,$
                  abund_name=basedir+'dbase/abundance/'+file_abund,/photons,/continuum

if NOT keyword_set(continuum) then $
make_chianti_spec,results,lambda,output,binsize=wbs,instr_fwhm=instrum_fwhm,$
                  abund_name=basedir+'dbase/abundance/'+file_abund,/photons
endif

if keyword_set(all) then begin
ch_synthetic,w1,w2,logt_isothermal=logTe(it),logem_isothermal=logem,density=N_e,$
             output=results,$
             ioneq_name=basedir+'dbase/ioneq/'+file_ioneq,/photons,/all

if     keyword_set(continuum) then $
make_chianti_spec,results,lambda,output,binsize=wbs,instr_fwhm=instrum_fwhm,$
                  abund_name=basedir+'dbase/abundance/'+file_abund,/photons,/continuum,/all

if NOT keyword_set(continuum) then $
make_chianti_spec,results,lambda,output,binsize=wbs,instr_fwhm=instrum_fwhm,$
                  abund_name=basedir+'dbase/abundance/'+file_abund,/photons,/all
endif

; Get Fe abundance
Abundance_Fe = float(output.abund(25))

; Set Emissivty(Ne^pow) proper scaling
pow=2.

Sum=0.d

if Nosum eq 1 then goto,skip_sum
  i=0L
  for i=0L,n_elements(output.lines)-1 do begin
    difw   = abs(LAMBDA - output.lines[i].wvl)
    indice = ( where(difw eq min(difw)) )(0)
    phi_peak1_line_i=(phi_peak1(indice))
    Sum=Sum+phi_peak1_line_i*output.lines[i].INT
 endfor
skip_sum:

printf,11, logTe(it),$
           int_tabulated(output.lambda,output.spectrum*phi_peak1,/double) / (N_e^pow) ,$
           Sum / (N_e^pow)
close,11
openw,11,output_file(ip),/append

endfor;temp loop
close,11
endfor;passband loop

return
end

;--------------------------------------------------------
pro leer_passband,range094,range131,range171,range193,range211,range335,waves,$
                  Phi_094,Phi_131,Phi_171,Phi_193,Phi_211,Phi_335,$
                  i094,i131,i171,i193,i211,i335

;openr,1,'bandpasses_aia.txt'
;openr,1,'bandpasses_aia_new.txt' ; the one sent by Rich, used up to May-27-2013
 openr,1,'bandpasses_aia_ssw-latest.txt'  ;  the latest
readf,1,nw
waves  =fltarr(nw)
Phi_094=fltarr(nw)
Phi_131=fltarr(nw)
Phi_171=fltarr(nw)
Phi_193=fltarr(nw)
Phi_211=fltarr(nw)
Phi_335=fltarr(nw)
readf,1,waves,Phi_094,Phi_131,Phi_171,Phi_193,Phi_211,Phi_335
close,1

;goto,skipthis
fact=10000.
i094=where(Phi_094 gt max(Phi_094)/fact)
i131=where(Phi_131 gt max(Phi_131)/fact)
i171=where(Phi_171 gt max(Phi_171)/fact)
i193=where(Phi_193 gt max(Phi_193)/fact)
i211=where(Phi_211 gt max(Phi_211)/fact)
i335=where(Phi_335 gt max(Phi_335)/fact)
 range094=[waves(min(i094)),waves(max(i094))]
 range131=[waves(min(i131)),waves(max(i131))]
 range171=[waves(min(i171)),waves(max(i171))]
 range193=[waves(min(i193)),waves(max(i193))]
 range211=[waves(min(i211)),waves(max(i211))]
 range335=[waves(min(i335)),waves(max(i335))]
o094=where(Phi_094 le max(Phi_094)/fact)
o131=where(Phi_131 le max(Phi_131)/fact)
o171=where(Phi_171 le max(Phi_171)/fact)
o193=where(Phi_193 le max(Phi_193)/fact)
o211=where(Phi_211 le max(Phi_211)/fact)
o335=where(Phi_335 le max(Phi_335)/fact)

;Phi_094(o094)=0.
;Phi_131(o131)=0.
;Phi_171(o171)=0.
;Phi_193(o193)=0.
;Phi_211(o211)=0.
;Phi_335(o335)=0.

skipthis:

;stop
return
end

