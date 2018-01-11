; cadence: integer, cadence in [hr] 

; Example:
; Use of grab to get STEREO-A data to get a full tomographic period:
; grab_data,'2008-03-20','2008-04-17',1,'/data1/tomography/DATA/euvi/CR2068/',/A
; If /GRAF is used only data availability info is generated
;
; Direct use of the SSW routines to get high-cadence data for a short 6 hr period:
; B171 = vso_search(date='2009-03-18T15:30 - 2009-03-18T16:30',inst='EUVI',sample=100L,source='STEREO_B',wave='171 Angstrom')
; A195 = vso_get(A195,/force,out_dir='/media/disk/data1/tomography/DATA/euvi/2009.03/hicad195_1day/')

pro grab_data,date1,date2,cadence,basedir,graf=graf,A=A,B=B

common metainfo,A171,A195,A284,A304,B171,B195,B284,B304

if NOT keyword_set(graf) and NOT keyword_set(A) and NOT keyword_set(B) then begin
    print,'You are requesting to actually DOWNLOAD the data, please specify: /A, /B, or both.'
    return
endif

cadence = cadence * 3600L

cadenceA171 = cadence; 10L
cadenceB171 = cadence; 10L

cadenceA195 = cadence; 10L
cadenceB195 = cadence; 10L

cadenceA284 = cadence; 10L
cadenceB284 = cadence; 10L

cadenceA304 = cadence; 10L
cadenceB304 = cadence; 10L

print,'Cadences:'
print,'             171        195          284        304'
print,'  B:',[cadenceB171,cadenceB195,cadenceB284,cadenceB304]
print,'  A:',[cadenceA171,cadenceA195,cadenceA284,cadenceA304]

dirA171=basedir+'A171/'
dirA195=basedir+'A195/'
dirA284=basedir+'A284/'
dirA304=basedir+'A304/'
dirB171=basedir+'B171/'
dirB195=basedir+'B195/'
dirB284=basedir+'B284/'
dirB304=basedir+'B304/'

if keyword_set(A) then begin
 print,'Records for A171:'
 A171 = vso_search(date1,date2, inst='EUVI',sample=cadenceA171,source='STEREO_A',wave='171 Angstrom')
 print,'Records for A195:'
 A195 = vso_search(date1,date2, inst='EUVI',sample=cadenceA195,source='STEREO_A',wave='195 Angstrom')
 print,'Records for A284:'
 A284 = vso_search(date1,date2, inst='EUVI',sample=cadenceA284,source='STEREO_A',wave='284 Angstrom')
 print,'Records for A304:'
 A304 = vso_search(date1,date2, inst='EUVI',sample=cadenceA304,source='STEREO_A',wave='304 Angstrom')
 if keyword_set(graf) then begin
     graph_data,'plot-A.eps',basedir,/A
     print,'plot of the data generated in '+basedir+'plot-A.eps'
 endif
endif

if keyword_set(B) then begin
 print,'Records for B171:'
 B171 = vso_search(date1,date2, inst='EUVI',sample=cadenceB171,source='STEREO_B',wave='171 Angstrom')
 print,'Records for B195:'
 B195 = vso_search(date1,date2, inst='EUVI',sample=cadenceB195,source='STEREO_B',wave='195 Angstrom')
 print,'Records for B284:'
 B284 = vso_search(date1,date2, inst='EUVI',sample=cadenceB284,source='STEREO_B',wave='284 Angstrom')
 print,'Records for B304:'
 B304 = vso_search(date1,date2, inst='EUVI',sample=cadenceB304,source='STEREO_B',wave='304 Angstrom')
 if keyword_set(graf) then begin
     graph_data,'plot-B.eps',basedir,/B
     print,'plot of the data generated in '+basedir+'plot-B.eps'
 endif
endif

if keyword_set(graf) then return

; DOWNLOAD THE DATA:
if keyword_set(B) then begin
 B171 = vso_get(B171,/force,out_dir=dirB171)
 B195 = vso_get(B195,/force,out_dir=dirB195)
 B284 = vso_get(B284,/force,out_dir=dirB284)
 B304 = vso_get(B304,/force,out_dir=dirB304)
endif
if keyword_set(A) then begin
 A171 = vso_get(A171,/force,out_dir=dirA171)
 A195 = vso_get(A195,/force,out_dir=dirA195)
 A284 = vso_get(A284,/force,out_dir=dirA284)
 A304 = vso_get(A304,/force,out_dir=dirA304)
endif

return
end

pro graph_data,filename,basedir,A=A,B=B
common metainfo,A171,A195,A284,A304,B171,B195,B284,B304
;rename metainfo structures
if keyword_set(A) then begin
 M171=A171
 M195=A195
 M284=A284
 M304=A304
endif
if keyword_set(B) then begin
 M171=B171
 M195=B195
 M284=B284
 M304=B304
endif 
;read dates of data points
 dates171=M171.time.start
 dates195=M195.time.start
 dates284=M284.time.start
 dates304=M304.time.start
;compute number of data points
 n171=n_elements(dates171)
 n195=n_elements(dates195)
 n284=n_elements(dates284)
 n304=n_elements(dates304)
;Define julian-date arrays
 julians171=dblarr(n171)
 julians195=dblarr(n195)
 julians284=dblarr(n284)
 julians304=dblarr(n304)
;Compute julian-date arrays
 fmt='(C(CYI4, 1X, CMOI2, 1X, CDI2, 1X, CHI2, 1X, CMI2, 1X, CSF7.4))'
 READS,dates171,julians171, FORMAT=fmt
 READS,dates195,julians195, FORMAT=fmt
 READS,dates284,julians284, FORMAT=fmt
 READS,dates304,julians304, FORMAT=fmt
; Order julian-date arrays in ascending date
 i171=sort(julians171) & julians171=julians171(i171)
 i195=sort(julians195) & julians195=julians195(i195)
 i284=sort(julians284) & julians284=julians284(i284)
 i304=sort(julians304) & julians304=julians304(i304)

 i0171=where(julians171 eq min(julians171))
 i0195=where(julians195 eq min(julians195))
 i0284=where(julians284 eq min(julians284))
 i0304=where(julians304 eq min(julians304))
 if171=where(julians171 eq max(julians171))
 if195=where(julians195 eq max(julians195))
 if284=where(julians284 eq max(julians284))
 if304=where(julians304 eq max(julians304))

 ; create psym=8
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=2.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL
 !p.multi=[0,2,2]
 !p.charsize=0.5
 ps1,basedir+filename,0
 sym=8
 thi=2
 plot,julians171-min(julians171)+1,findgen(n171),title='171 from '+dates171(i0171)+' to '+dates171(if171),psym=sym,xstyle=1,ystyle=1,th=thi
 plot,julians195-min(julians195)+1,findgen(n195),title='195 from '+dates195(i0195)+' to '+dates195(if195),psym=sym,xstyle=1,ystyle=1,th=thi
 plot,julians284-min(julians284)+1,findgen(n284),title='284 from '+dates284(i0284)+' to '+dates284(if284),psym=sym,xstyle=1,ystyle=1,th=thi
 plot,julians304-min(julians304)+1,findgen(n304),title='304 from '+dates304(i0304)+' to '+dates304(if304),psym=sym,xstyle=1,ystyle=1,th=thi
 ps2

return
end
