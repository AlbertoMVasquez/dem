pro explore
  
;grab_data_full_diego,'2009-03-09','2009-04-05','EUVI_A','/data1/work/Minimum_2009/','data_cr2081_diego',/graf;tesis
;grab_data_full_diego,'1996-10-15','1996-11-11','EIT'   ,'/data1/work/Minimum_1996/','data_cr1915_diego',/graf;tesis

grab_data_full_diego,'2017-06-23T00:00:00','2017-07-20T23:59:59','EUVI_A' ,'/data1/tomography/DATA/euvi/CR2192/','data_euvi_cr2192',/graf
stop
grab_data_full_diego,'2017-06-23T00:00:00','2017-07-20T23:59:59','AIA'   ,'/data1/tomography/DATA/aia/CR2192/','data_cr2192',/graf
stop
grab_data_full_diego,'2017-03-06T00:00:00','2017-04-02T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2188_diego',/graf;tesis
grab_data_full_diego,'2017-04-02T00:00:00','2017-04-29T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2189_diego',/graf
grab_data_full_diego,'2017-02-06T00:00:00','2017-03-06T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2187_diego',/graf
grab_data_full_diego,'2017-01-10T00:00:00','2017-02-06T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2186_diego',/graf
grab_data_full_diego,'2016-12-14T00:00:00','2017-01-10T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2185_diego',/graf
grab_data_full_diego,'2016-11-16T00:00:00','2016-12-14T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2184_diego',/graf
grab_data_full_diego,'2016-10-20T00:00:00','2016-11-16T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2183_diego',/graf
grab_data_full_diego,'2016-09-23T00:00:00','2016-10-20T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2182_diego',/graf
grab_data_full_diego,'2016-08-26T00:00:00','2016-09-23T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2181_diego',/graf
grab_data_full_diego,'2016-07-30T00:00:00','2016-08-26T23:59:59','AIA'   ,'/data1/work/Minimum_2020/','data_cr2180_diego',/graf

;para los magnetogramas ver el mreadfits.pro de solar soft y el array
;que tira puede graficarse con tvscl
return
end

pro grab_data_full_diego,date1,date2,inst,basedir,filename,graf=graf
common infoeit ,eit171,eit195,eit284,eit304
common infoeuvi,euvi171,euvi195,euvi284,euvi304
common infoaia ,aia094,aia131,aia171,aia193,aia211,aia304,aia335
; Examples:
;
; Use of grab data to get a full tomographic period:
; grab_data_eit,'1996-04-08','1996-05-06','/data1/tomography/DATA/eit/CR1908/','',/graf
if inst eq 'EIT' then begin
 dir171=basedir+'171/'
 dir195=basedir+'195/'
 dir284=basedir+'284/'
 dir304=basedir+'304/'
 wave=['171 Angstrom','195 Angstrom','284 Angstrom','304 Angstrom']
endif

if inst eq 'EUVI_A' then begin
 dira171=basedir+'A171/'
 dira195=basedir+'A195/'
 dira284=basedir+'A284/'
 dira304=basedir+'A304/'
 wave=['171 Angstrom','195 Angstrom','284 Angstrom','304 Angstrom']
 source = 'STEREO_A'
 instru = 'EUVI'
endif

if inst eq 'EUVI_B' then begin
 dirb171=basedir+'B171/'
 dirb195=basedir+'B195/'
 dirb284=basedir+'B284/'
 dirb304=basedir+'B304/'
 wave=['171 Angstrom','195 Angstrom','284 Angstrom','304 Angstrom']
 source = 'STEREO_B'
 instru = 'EUVI'
endif

if inst eq 'AIA' then begin
 dir094=basedir+'094/'
 dir131=basedir+'131/'
 dir171=basedir+'171/'
 dir193=basedir+'193/'
 dir211=basedir+'211/'
 dir304=basedir+'304/'
 dir335=basedir+'335/'
 wave=['094 Angstrom','131 Angstrom','171 Angstrom','193 Angstrom','211 Angstrom','304 Angstrom','335 Angstrom']
 pix = 4096
 maxval = 5000L
; cadences3 =  [10,20,30] * 60L
 cadences3 =  [60] * 60L ;cadencia de 1 imagen por hora
; cadences3 =  [30,40,50,60] * 60L
endif

 cadences  =  [1,10,20,30,40,50,60] * 60L
 cadences2 =  [30,40,50,60] * 60L
 cadences2 =  [60,60,60,60] * 60L


;modificar nombre de esta entrada y extender a aia que necesita 3 wave mas
;OBS: euvi lleva source='STEREO_A' o B, y AIA lleva
;,pixels=pix,level=lev1 ver que son a ver si puedo unificar todo. leer
;mejor vso_search. Ademas el vso_get de aia lleva 2 parametros mas que
;son /rice,site=server ver que carajo hacen y si se pueden unificar
if inst eq 'EIT' then begin
 print,'Records for'+wave(0)+':'
 icad=0
 research171_1:
 cadence=cadences[icad]
eit171 = vso_search(date1,date2, inst=inst,wave=wave(0),sample=cadence)
 if n_elements(eit171.time) gt 999L then begin
    icad=icad+1
    goto,research171_1
 endif

 print,'Records for'+wave(1)+':'
 icad=0
 research195_1:
 cadence=cadences[icad]
 eit195 = vso_search(date1,date2, inst=inst,wave=wave(1),sample=cadence) 
 if n_elements(eit195.time) gt 999L then begin
    icad=icad+1
    goto,research195_1
 endif

 print,'Records for'+wave(2)+':'
 icad=0
 research284_1:
 cadence=cadences[icad]
 eit284 = vso_search(date1,date2, inst=inst,wave=wave(2),sample=cadence)
 if n_elements(eit284.time) gt 999L then begin
    icad=icad+1
    goto,research284_1
 endif

 print,'Records for'+wave(3)+':'
 icad=0
 research304_1:
 cadence=cadences[icad]
 eit304 = vso_search(date1,date2, inst=inst,wave=wave(3),sample=cadence)
 if n_elements(eit304.time) gt 999L then begin
    icad=icad+1
    goto,research304_1
 endif
endif

if inst eq 'EUVI_A' or inst eq 'EUVI_B' then begin
print,'Records for'+wave(0)+':'
 icad=0
 research171_2:
 cadence=cadences2[icad] 
euvi171 = vso_search(date1,date2, inst=instru,wave=wave(0),sample=cadence,source=source)
 if n_elements(euvi171.time) gt 999L then begin
    icad=icad+1
    goto,research171_2
 endif

 print,'Records for'+wave(1)+':'
 icad=0
 research195_2:
 cadence=cadences2[icad]
 euvi195 = vso_search(date1,date2, inst=instru,wave=wave(1),sample=cadence,source=source)
 if n_elements(euvi195.time) gt 999L then begin
    icad=icad+1
    goto,research195_2
 endif

 print,'Records for'+wave(2)+':'
 icad=0
 research284_2:
 cadence=cadences2[icad]
 euvi284 = vso_search(date1,date2, inst=instru,wave=wave(2),sample=cadence,source=source)
 if n_elements(euvi284.time) gt 999L then begin
    icad=icad+1
    goto,research284_2
 endif
 print,'Records for'+wave(3)+':'

 icad=0
 research304_2:
 cadence=cadences2[icad]
 euvi304 = vso_search(date1,date2, inst=instru,wave=wave(3),sample=cadence,source=source)
 if n_elements(euvi304.time) gt 999L then begin
    icad=icad+1
    goto,research304_2
 endif
endif


if inst eq 'AIA' then begin
print,'Records for'+wave(0)+':'
 icad=0
 research094_3:
 cadence=cadences3[icad]
 aia094 = vso_search(date1,date2, inst=inst,wave=wave(0),sample=cadence,pixels=pix,level=lev1)
 if n_elements(aia094.time) gt maxval then begin
    icad=icad+1
    goto,research094_3
 endif

print,'Records for'+wave(1)+':'
 icad=0
 research131_3:
 cadence=cadences3[icad]
 aia131 = vso_search(date1,date2, inst=inst,wave=wave(1),sample=cadence,pixels=pix,level=lev1)
 if n_elements(aia131.time) gt maxval then begin
    icad=icad+1
    goto,research131_3
 endif

print,'Records for'+wave(2)+':'
 icad=0
 research171_3:
 cadence=cadences3[icad]
aia171 = vso_search(date1,date2, inst=inst,wave=wave(2),sample=cadence,pixels=pix,level=lev1)
 if n_elements(aia171.time) gt maxval then begin
    icad=icad+1
    goto,research171_3
 endif

 print,'Records for'+wave(3)+':'
 icad=0
 research193_3:
 cadence=cadences3[icad]
 aia193 = vso_search(date1,date2, inst=inst,wave=wave(3),sample=cadence,pixels=pix,level=lev1)
 if n_elements(aia193.time) gt maxval then begin
    icad=icad+1
    goto,research193_3
 endif

 print,'Records for'+wave(4)+':'
 icad=0
 research211_3:
 cadence=cadences3[icad]
 aia211 = vso_search(date1,date2, inst=inst,wave=wave(4),sample=cadence,pixels=pix,level=lev1)
 if n_elements(aia211.time) gt maxval then begin
    icad=icad+1
    goto,research211_3
 endif

 print,'Records for'+wave(5)+':'
 icad=0
 research304_3:
 cadence=cadences3[icad]
 aia304 = vso_search(date1,date2, inst=inst,wave=wave(5),sample=cadence,pixels=pix,level=lev1)
 if n_elements(aia304.time) gt maxval then begin
    icad=icad+1
    goto,research304_3
 endif

 print,'Records for'+wave(6)+':'
 icad=0
 research335_3:
 cadence=cadences3[icad]
 aia335 = vso_search(date1,date2, inst=inst,wave=wave(6),sample=cadence,pixels=pix,level=lev1)
 if n_elements(aia335.time) gt maxval then begin
    icad=icad+1
    goto,research335_3
 endif

endif


;aca debo agregar 3 wave de aia

 if keyword_set(graf) then begin
     graph_data,basedir,filename,inst 
     print,'plot of the data generated in '+basedir+filename+'.gif'
 endif

; DOWNLOAD THE DATA:
if not keyword_set(graf) then begin

if inst eq 'EIT' then begin
 eit171 = vso_get(eit171,/force,out_dir=dir171)
 eit195 = vso_get(eit195,/force,out_dir=dir195)
 eit284 = vso_get(eit284,/force,out_dir=dir284)
 eit304 = vso_get(eit304,/force,out_dir=dir304)
endif

if inst eq 'EUVI_B' then begin
 euvi171 = vso_get(euvi171,/force,out_dir=dirb171)
 euvi195 = vso_get(euvi195,/force,out_dir=dirb195)
 euvi284 = vso_get(euvi284,/force,out_dir=dirb284)
 euvi304 = vso_get(euvi304,/force,out_dir=dirb304)
endif

if inst eq 'EUVI_A' then begin
 euvi171 = vso_get(euvi171,/force,out_dir=dira171)
 euvi195 = vso_get(euvi195,/force,out_dir=dira195)
 euvi284 = vso_get(euvi284,/force,out_dir=dira284)
 euvi304 = vso_get(euvi304,/force,out_dir=dira304)
endif

if inst eq 'AIA' then begin
 aia171 = vso_get(aia171,/force,out_dir=dir171)
 aia193 = vso_get(aia193,/force,out_dir=dir193)
 aia211 = vso_get(aia211,/force,out_dir=dir211)
 aia335 = vso_get(aia335,/force,out_dir=dir335)
;aia304 = vso_get(aia304,/force,out_dir=dir304)
;aia094 = vso_get(aia094,/force,out_dir=dir094)
;aia131 = vso_get(aia131,/force,out_dir=dir131)
endif

endif

return
end

pro graph_data,basedir,filename,instru
common infoeit ,eit171,eit195,eit284,eit304
common infoeuvi,euvi171,euvi195,euvi284,euvi304
common infoaia ,aia094,aia131,aia171,aia193,aia211,aia304,aia335
;rename metainfo structures

if instru eq 'EIT' then begin
 M171=eit171
 M195=eit195
 M284=eit284
 M304=eit304
endif

if instru eq 'EUVI_A' or instru eq 'EUVI_B' then begin
 M171=euvi171
 M195=euvi195
 M284=euvi284
 M304=euvi304
endif

if instru eq 'AIA' then begin
 M094=aia094
 M131=aia131
 M171=aia171
 M193=aia193
 M211=aia211
 M304=aia304
 M335=aia335
endif

if instru eq 'EIT' or instru eq 'EUVI_A' or instru eq 'EUVI_B' then begin
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
 i171=sort(julians171) & julians171=julians171(i171) & dates171=dates171(i171)
 i195=sort(julians195) & julians195=julians195(i195) & dates195=dates195(i195)
 i284=sort(julians284) & julians284=julians284(i284) & dates284=dates284(i284)
 i304=sort(julians304) & julians304=julians304(i304) & dates304=dates304(i304)

 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=2.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL
 !p.charsize=1.25
 !p.background=255
 !p.color      =0
 !p.multi=[0,2,2]
 plot,julians171-min(julians171)+1,findgen(n171)+1,title='171 from '+dates171(0)+' to '+dates171(n171-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians195-min(julians195)+1,findgen(n195)+1,title='195 from '+dates195(0)+' to '+dates195(n195-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians284-min(julians284)+1,findgen(n284)+1,title='284 from '+dates284(0)+' to '+dates284(n284-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians304-min(julians304)+1,findgen(n304)+1,title='304 from '+dates304(0)+' to '+dates304(n304-1),psym=8,xstyle=1,ystyle=1,th=1
 xyouts,[0.05,0.55,0.05,0.55],[0.8,0.8,0.2,0.2],[string(n171),string(n195),string(n284),string(n304)],/normal
 record_gif,basedir,filename+'.gif','X'
endif

if instru eq 'AIA' then begin
 M094=aia094
 M131=aia131
 M171=aia171
 M193=aia193
 M211=aia211
 M304=aia304
 M335=aia335
;read dates of data points 
 dates094=M094.time.start
 dates131=M131.time.start
 dates171=M171.time.start
 dates193=M193.time.start
 dates211=M211.time.start
 dates304=M304.time.start
 dates335=M335.time.start
;compute number of data points  
 n094=n_elements(dates094)
 n131=n_elements(dates131)
 n171=n_elements(dates171)
 n193=n_elements(dates193)
 n211=n_elements(dates211)
 n304=n_elements(dates304)
 n335=n_elements(dates335)
;Define julian-date arrays  
 julians094=dblarr(n094)
 julians131=dblarr(n131)
 julians171=dblarr(n171)
 julians193=dblarr(n193)
 julians211=dblarr(n211)
 julians304=dblarr(n304)
 julians335=dblarr(n335)
;Compute julian-date arrays     
 fmt='(C(CYI4, 1X, CMOI2, 1X, CDI2, 1X, CHI2, 1X, CMI2, 1X, CSF7.4))'
 READS,dates094,julians094, FORMAT=fmt
 READS,dates131,julians131, FORMAT=fmt
 READS,dates171,julians171, FORMAT=fmt
 READS,dates193,julians193, FORMAT=fmt
 READS,dates211,julians211, FORMAT=fmt
 READS,dates304,julians304, FORMAT=fmt
 READS,dates335,julians335, FORMAT=fmt
 i094=sort(julians094) & julians094=julians094(i094) & dates094=dates094(i094)
 i131=sort(julians131) & julians131=julians131(i131) & dates131=dates131(i131)
 i171=sort(julians171) & julians171=julians171(i171) & dates171=dates171(i171)
 i193=sort(julians193) & julians193=julians193(i193) & dates193=dates193(i193)
 i211=sort(julians211) & julians211=julians211(i211) & dates211=dates211(i211)
 i304=sort(julians304) & julians304=julians304(i304) & dates304=dates304(i304)
 i335=sort(julians335) & julians335=julians335(i335) & dates335=dates335(i335)
;stop
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=2.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL

 WINDOW, 1, XSIZE=1600, YSIZE=600
 !p.charsize=2.0
 !p.background=255
 !p.color      =0
 !p.multi=[0,4,2]
 plot,julians094-min(julians094)+1,findgen(n094)+1,title='094 from '+dates094(0)+' to '+dates094(n094-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians131-min(julians131)+1,findgen(n131)+1,title='131 from '+dates131(0)+' to '+dates131(n131-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians171-min(julians171)+1,findgen(n171)+1,title='171 from '+dates171(0)+' to '+dates171(n171-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians193-min(julians193)+1,findgen(n193)+1,title='193 from '+dates193(0)+' to '+dates193(n193-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians211-min(julians211)+1,findgen(n211)+1,title='211 from '+dates211(0)+' to '+dates211(n211-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians304-min(julians304)+1,findgen(n304)+1,title='304 from '+dates304(0)+' to '+dates304(n304-1),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians335-min(julians335)+1,findgen(n335)+1,title='335 from '+dates335(0)+' to '+dates335(n335-1),psym=8,xstyle=1,ystyle=1,th=1
 xyouts,[0.05,0.30,0.55,0.80,0.05,0.30,0.55],[0.8,0.8,0.8,0.8,0.25,0.25,0.25],[string(n094),string(n131),string(n171),string(n193),string(n211),string(n304),string(n335)],/normal
 record_gif,basedir,filename+'.gif','X'
endif


return
end
