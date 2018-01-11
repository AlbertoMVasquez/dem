pro explore

grab_data_eit,'1996-01-17','1996-02-13','/data1/work/Minimum_1996/','data_cr1905',/graf
grab_data_eit,'1996-02-13','1996-03-11','/data1/work/Minimum_1996/','data_cr1906',/graf
grab_data_eit,'1996-03-11','1996-04-08','/data1/work/Minimum_1996/','data_cr1907',/graf
grab_data_eit,'1996-04-08','1996-05-06','/data1/work/Minimum_1996/','data_cr1908',/graf
grab_data_eit,'1996-05-05','1996-06-01','/data1/work/Minimum_1996/','data_cr1909',/graf
grab_data_eit,'1996-06-01','1996-06-28','/data1/work/Minimum_1996/','data_cr1910',/graf
grab_data_eit,'1996-06-28','1996-07-25','/data1/work/Minimum_1996/','data_cr1911',/graf
grab_data_eit,'1996-07-25','1996-08-22','/data1/work/Minimum_1996/','data_cr1912',/graf
grab_data_eit,'1996-08-22','1996-09-18','/data1/work/Minimum_1996/','data_cr1913',/graf
grab_data_eit,'1996-09-18','1996-10-15','/data1/work/Minimum_1996/','data_cr1914',/graf
grab_data_eit_diego,'1996-10-15','1996-11-11','/data1/work/Minimum_1996_diego/','data_cr1915_diego',/graf;tesis
grab_data_eit,'1996-11-11','1996-12-09','/data1/work/Minimum_1996/','data_cr1916',/graf
grab_data_eit,'1996-12-09','1997-01-05','/data1/work/Minimum_1996/','data_cr1917',/graf
grab_data_eit,'1997-01-05','1997-02-01','/data1/work/Minimum_1996/','data_cr1918',/graf
grab_data_eit,'1997-02-01','1997-03-01','/data1/work/Minimum_1996/','data_cr1919',/graf
grab_data_eit,'1997-03-01','1997-03-28','/data1/work/Minimum_1996/','data_cr1920',/graf
grab_data_eit,'1997-03-28','1997-04-24','/data1/work/Minimum_1996/','data_cr1921',/graf
grab_data_eit,'1997-04-24','1997-05-22','/data1/work/Minimum_1996/','data_cr1922',/graf
grab_data_eit,'1997-05-22','1997-06-18','/data1/work/Minimum_1996/','data_cr1923',/graf
grab_data_eit,'1997-06-18','1997-07-15','/data1/work/Minimum_1996/','data_cr1924',/graf
grab_data_eit,'1997-07-15','1997-08-11','/data1/work/Minimum_1996/','data_cr1925',/graf
grab_data_eit,'1997-08-11','1997-09-07','/data1/work/Minimum_1996/','data_cr1926',/graf
grab_data_eit,'1997-09-07','1997-10-05','/data1/work/Minimum_1996/','data_cr1927',/graf
grab_data_eit,'1997-10-05','1997-11-01','/data1/work/Minimum_1996/','data_cr1928',/graf
grab_data_eit,'1997-11-01','1997-11-28','/data1/work/Minimum_1996/','data_cr1929',/graf
grab_data_eit,'1997-11-28','1997-12-26','/data1/work/Minimum_1996/','data_cr1930',/graf
grab_data_eit,'1997-12-26','1998-01-22','/data1/work/Minimum_1996/','data_cr1931',/graf
grab_data_eit,'1998-01-22','1998-02-18','/data1/work/Minimum_1996/','data_cr1932',/graf
grab_data_eit,'1998-02-18','1998-03-18','/data1/work/Minimum_1996/','data_cr1933',/graf
grab_data_eit,'1998-03-18','1998-04-14','/data1/work/Minimum_1996/','data_cr1934',/graf
grab_data_eit,'1998-04-14','1998-05-11','/data1/work/Minimum_1996/','data_cr1935',/graf
grab_data_eit,'1998-05-11','1998-06-07','/data1/work/Minimum_1996/','data_cr1936',/graf
grab_data_eit,'1998-06-07','1998-07-05','/data1/work/Minimum_1996/','data_cr1937',/graf

grab_data_eit_diego,'2000-09-02','2000-09-30','/data1/work/Minimum_1996_diego/','data_cr1967_diego',/graf;tesis

return
end

pro grab_data_eit_diego,date1,date2,basedir,filename,graf=graf
common metainfo,eit171,eit195,eit284,eit304

; Examples:
;
; Use of grab data to get a full tomographic period:
; grab_data_eit,'1996-04-08','1996-05-06','/data1/tomography/DATA/eit/CR1908/','',/graf

 dir171=basedir+'171/'
 dir195=basedir+'195/'
 dir284=basedir+'284/'
 dir304=basedir+'304/'

 cadences =  [1,10,20,30,40,50,60] * 60L
 
 print,'Records for 171:'
 icad=0
 research171:
 cadence=cadences[icad]
 eit171 = vso_search(date1,date2, inst='EIT',wave='171 Angstrom',sample=cadence)
 if n_elements(eit171.time) gt 999L then begin
    icad=icad+1
    goto,research171
 endif

 print,'Records for A195:'
 icad=0
 research195:
 cadence=cadences[icad]
 eit195 = vso_search(date1,date2, inst='EIT',wave='195 Angstrom',sample=cadence) 
 if n_elements(eit195.time) gt 999L then begin
    icad=icad+1
    goto,research195
 endif

 print,'Records for A284:'
 icad=0
 research284:
 cadence=cadences[icad]
 eit284 = vso_search(date1,date2, inst='EIT',wave='284 Angstrom',sample=cadence)
 if n_elements(eit284.time) gt 999L then begin
    icad=icad+1
    goto,research284
 endif

 print,'Records for A304:'
 icad=0
 research304:
 cadence=cadences[icad]
 eit304 = vso_search(date1,date2, inst='EIT',wave='304 Angstrom',sample=cadence)
 if n_elements(eit304.time) gt 999L then begin
    icad=icad+1
    goto,research304
 endif

 if keyword_set(graf) then begin
     graph_data,basedir,filename
     print,'plot of the data generated in '+basedir+filename+'.gif'
     return
 endif

; DOWNLOAD THE DATA:
 eit171 = vso_get(eit171,/force,out_dir=dir171)
 eit195 = vso_get(eit195,/force,out_dir=dir195)
 eit284 = vso_get(eit284,/force,out_dir=dir284)
 eit304 = vso_get(eit304,/force,out_dir=dir304)

return
end

pro graph_data,basedir,filename
common metainfo,eit171,eit195,eit284,eit304
;rename metainfo structures
 M171=eit171
 M195=eit195
 M284=eit284
 M304=eit304
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
 !p.charsize=1.25
;ps1,basedir+filename+'.eps',0
;window,xs=1000,ys=600
 !p.background=255
 !p.color      =0
 plot,julians171-min(julians171)+1,findgen(n171)+1,title='171 from '+dates171(i0171)+' to '+dates171(if171),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians195-min(julians195)+1,findgen(n195)+1,title='195 from '+dates195(i0195)+' to '+dates195(if195),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians284-min(julians284)+1,findgen(n284)+1,title='284 from '+dates284(i0284)+' to '+dates284(if284),psym=8,xstyle=1,ystyle=1,th=1
 plot,julians304-min(julians304)+1,findgen(n304)+1,title='304 from '+dates304(i0304)+' to '+dates304(if304),psym=8,xstyle=1,ystyle=1,th=1
 xyouts,[0.05,0.55,0.05,0.55],[0.8,0.8,0.2,0.2],[string(n171),string(n195),string(n284),string(n304)],/normal
 record_gif,basedir,filename+'.gif','X'
;ps2

return
end
