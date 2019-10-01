
;imagecheckwrapper,'/Storage1TB/tomography/DATA/euvi/CR2055/b4/','list.B.304.b4','euvi',0.,1,'x',304,/select
;makemovie,images,'/Storage1TB/tomography/DATA/euvi/CR2055/b4/Movie/B304/'

pro concatenar
;concatena varias listas de filenames seleccionados en uno unico, para binear.
;dir='/data2/tomography/DATA/aia/CR2208/211/'
  dir = '/media/Data1/tomography/DATA/aia/CR2219/171/'
  outfile ='list.171.selected'
  
  files=['list1.171',$
         'list2.171',$
         'list3.171',$
         'list4.171',$
         'list5.171',$
         'list6.171',$
         'list7.171',$
         'list8.171',$
         'list9.171',$
         'list10.171']
  
  nfiles=n_elements(files)
  n=fltarr(nfiles)
  nn=0
  for j=0,nfiles-1 do begin
     openr,1,dir+files(j)
     readf,1,nn
     n(j)=nn
     close,1
  endfor
  a=0                           
  x=''
  Ntot=total(n)
  filenames=strarr(Ntot)
  s=0
  vec=[0,n]
  for j=0,nfiles-1 do begin
     openr,1,dir+files(j)
     readf,1,s
     for i=0,s-1 do begin
        readf,1,x
        w=total(vec(0:j))
        filenames(w+i)=x
     endfor
     close,1
  endfor
  Ntot=total(n)
  openw,2,dir+outfile
  printf,2,Ntot
  for i=0,Ntot-1 do begin
     printf,2,filenames(i)
  endfor
  close,2
  print, 'file escrito en: '+dir+outfile
return
end

pro destripar ;TERMINAR
;desarma una lista de datos strings en varias listas.
;para datos de aia 
band = '193'
  dir='/media/Data1/tomography/DATA/aia/CR2219/'+band+'/'
infile='list.'+band
outfiles=['list1.'+band,$
          'list2.'+band,$
          'list3.'+band,$
          'list4.'+band,$
          'list5.'+band,$
          'list6.'+band,$
          'list7.'+band,$
          'list8.'+band,$
          'list9.'+band,$
          'list10.'+band]

n=0
imag=''
     openr,1,dir+infile
     readf,1,n ;escalar
    
   cant_imag =  floor(n*1./n_elements(outfiles))

   for j=0,n_elements(outfiles)-1 do begin
      openw,2,dir+outfiles(j)
      for i =0, cant_imag-1  do begin
         readf,1,imag     
         printf,2, imag
      endfor
      if j eq n_elements(outfiles)-1 and  cant_imag ge n*1./n_elements(outfiles) then begin
         for i =0, (n-cant_imag*n_elements(outfiles))-1  do begin
            readf,1,imag
            printf,2, imag
         endfor
      endif
      
      close,2
   endfor
   
   stop
close,1
   return
end

pro inspeccion_2219_aia
common contar,conteo
goto,banda211
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list1.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list2.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list3.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list4.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list5.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list6.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list7.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list8.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list9.171','aia',0.,1,'x',171,/select
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/171/','list10.171','aia',0.,1,'x',171,/select
banda211:
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list1.193','aia',0.,1,'x',193,/select,/para_movie
 stop
imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list2.193','aia',0.,1,'x',193,/select,/para_movie
stop
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list3.193','aia',0.,1,'x',193,/select,/para_movie
stop
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list4.193','aia',0.,1,'x',193,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list5.193','aia',0.,1,'x',193,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list6.193','aia',0.,1,'x',193,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list7.193','aia',0.,1,'x',193,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list8.193','aia',0.,1,'x',193,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list9.193','aia',0.,1,'x',193,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/193/','list10.193','aia',0.,1,'x',193,/select,/para_movie

 stop
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list1.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list2.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list3.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list4.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list5.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list6.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list7.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list8.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list9.211','aia',0.,1,'x',211,/select,/para_movie
 imagecheckwrapper,'/media/Data1/tomography/DATA/aia/CR2219/211/','list10.211','aia',0.,1,'x',211,/select,/para_movie
 
  return
end


pro asd2082B
imagecheckwrapper,'/media/Data1/tomography/DATA/euvi/CR2082/B171/','list.B171.decon.proc','euvi',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/media/Data1/tomography/DATA/euvi/CR2082/B195/','list.B195.decon.proc','euvi',0.,1,'x',195,/select,/zbuff
imagecheckwrapper,'/media/Data1/tomography/DATA/euvi/CR2082/B284/','list.B284.decon.proc','euvi',0.,1,'x',284,/select,/zbuff

return
end

pro testeo2208
  imagecheckwrapper,'/data2/tomography/DATA/aia/CR2208/171/','list1.171.processed','aia',0.,1,'x',171,/select,/zbuff  
;  imagecheckwrapper,'/data2/tomography/DATA/aia/CR2208/193/','list.193.processed','aia',0.,1,'x',193,/select,/zbuff 
;  imagecheckwrapper,'/data2/tomography/DATA/aia/CR2208/211/','list.211.processed','aia',0.,1,'x',211,/select,/zbuff 
  return
end

pro inspeccion_2198_euvi
;  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2198/A171/','list.A171.b4.FULLDISK','euvi',0.,1,'x',171
;stop
  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2198/A195/','list.A195.b4.FULLDISK','euvi',0.,1,'x',195
stop
  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2198/A284/','list.A284.b4.FULLDISK','euvi',0.,1,'x',284
stop
  return
end


pro inspeccion_2198_aia

  imagecheckwrapper,'/data1/tomography/DATA/aia/CR2198/171/','list.171.b4.FULLDISK','aia',0.,1,'x',171
  imagecheckwrapper,'/data1/tomography/DATA/aia/CR2198/193/','list.193.b4.FULLDISK','aia',0.,1,'x',193
  imagecheckwrapper,'/data1/tomography/DATA/aia/CR2198/211/','list.211.b4.FULLDISK','aia',0.,1,'x',211
  imagecheckwrapper,'/data1/tomography/DATA/aia/CR2198/335/','list.335.b4.FULLDISK','aia',0.,1,'x',335

  
  return
end

pro inspeccion_2082_euvi                        
  
;  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/A171/','list.171.b4','euvi',0.,1,'x',171
;  stop
;  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/A195/','list.195.b4','euvi',0.,1,'x',195
 ; stop
  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/A284/','list.284.b4','euvi',0.,1,'x',284
  stop
;list.A171.DECON_2.proc.selected
end

pro inspeccion_2198_aia
  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2198/A171/','list.171.b4','euvi',0.,1,'x',171
  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2198/A195/','list.195.b4','euvi',0.,1,'x',171
  imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2198/A284/','list.284.b4','euvi',0.,1,'x',171
  
end


pro inspection_cycle_euvi_2082

; imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A171/','list.A171.b4','euvi',0.,1,'x',171 ;,/select
;imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/B171/','listB171.b4.nodecon','euvi',0.,1,'x',171;,/select
;imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/B195/','listB195.b4.nodecon','euvi',0.,1,'x',195;,/select
;imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/B284/','listB284.b4.nodecon','euvi',0.,1,'x',284;,/select

;imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/B171/','listB171.b4','euvi',0.,1,'x',171;,/select
;imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/B195/','listB195.b4','euvi',0.,1,'x',195;,/select
;imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2082/B284/','listB284.b4','euvi',0.,1,'x',284;,/select

return
end


pro inspection_cycle_eit_minimum1996

imagecheckwrapper,'/data1/tomography/DATA/eit/CR1919/171/','list.171.b','eit',0.,1,'x',171,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1919/195/','list.195.b','eit',0.,1,'x',195,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1919/284/','list.284.b','eit',0.,1,'x',284,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1919/304/','list.304.b','eit',0.,1,'x',304,/zbuff

imagecheckwrapper,'/data1/tomography/DATA/eit/CR1915/171/','list.171.b','eit',0.,1,'x',171,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1915/195/','list.195.b','eit',0.,1,'x',195,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1915/284/','list.284.b','eit',0.,1,'x',284,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1915/304/','list.304.b','eit',0.,1,'x',304,/zbuff

imagecheckwrapper,'/data1/tomography/DATA/eit/CR1914/171/','list.171.b','eit',0.,1,'x',171,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1914/195/','list.195.b','eit',0.,1,'x',195,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1914/284/','list.284.b','eit',0.,1,'x',284,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1914/304/','list.304.b','eit',0.,1,'x',304,/zbuff

return

imagecheckwrapper,'/data1/tomography/DATA/eit/CR1919/171/','list.171.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1919/195/','list.195.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1919/284/','list.284.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1919/304/','list.304.proc','eit',0.,1,'x',171,/select,/zbuff

imagecheckwrapper,'/data1/tomography/DATA/eit/CR1915/171/','list.171.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1915/195/','list.195.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1915/284/','list.284.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1915/304/','list.304.proc','eit',0.,1,'x',171,/select,/zbuff

imagecheckwrapper,'/data1/tomography/DATA/eit/CR1914/171/','list.171.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1914/195/','list.195.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1914/284/','list.284.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1914/304/','list.304.proc','eit',0.,1,'x',171,/select,/zbuff

imagecheckwrapper,'/data1/tomography/DATA/eit/CR1911/171/','list.171.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1911/195/','list.195.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1911/284/','list.284.proc','eit',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/eit/CR1911/304/','list.304.proc','eit',0.,1,'x',171,/select,/zbuff

return
end


pro inspection_cycle_euvi_2081

imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A171/','list.A171.b4','euvi',0.,1,'x',171,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A195/','list.A195.b4','euvi',0.,1,'x',195,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A284/','list.A284.b4','euvi',0.,1,'x',284,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A304/','list.A304.b4','euvi',0.,1,'x',304,/zbuff

imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A171/','list.A171.b4.nodecon','euvi',0.,1,'x',171,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A195/','list.A195.b4.nodecon','euvi',0.,1,'x',195,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A284/','list.A284.b4.nodecon','euvi',0.,1,'x',284,/zbuff

return

imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A171n/','list.A171.proc','euvi',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A195n/','list.A195.proc','euvi',0.,1,'x',195,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A284n/','list.A284.proc','euvi',0.,1,'x',284,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A304n/','list.A304.proc','euvi',0.,1,'x',304,/select,/zbuff

imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A171n/','list.A171.proc.nodecon','euvi',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A195n/','list.A195.proc.nodecon','euvi',0.,1,'x',195,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A284n/','list.A284.proc.nodecon','euvi',0.,1,'x',284,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A304n/','list.A304.proc.nodecon','euvi',0.,1,'x',304,/select,/zbuff

return
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A171/','list.A171.proc','euvi',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A195/','list.A195.proc','euvi',0.,1,'x',195,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A284/','list.A284.proc','euvi',0.,1,'x',284,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A304/','list.A304.proc','euvi',0.,1,'x',304,/select,/zbuff

imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A171/','list.A171.proc.nodecon','euvi',0.,1,'x',171,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A195/','list.A195.proc.nodecon','euvi',0.,1,'x',195,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A284/','list.A284.proc.nodecon','euvi',0.,1,'x',284,/select,/zbuff
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/A304/','list.A304.proc.nodecon','euvi',0.,1,'x',304,/select,/zbuff

return
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B171/','list.B171.nodecon.b4','euvi',0.,1,'x',171;,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.nodecon.b4','euvi',0.,1,'x',195;,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B284/','list.B284.nodecon.b4','euvi',0.,1,'x',284;,/select
return
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B171/','list.B171.proc.nodecon','euvi',0.,1,'x',171,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.proc.nodecon','euvi',0.,1,'x',195,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B284/','list.B284.proc.nodecon','euvi',0.,1,'x',284,/select
return
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B171/','list.B171.b4','euvi',0.,1,'x',171;,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.b4','euvi',0.,1,'x',195;,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B284/','list.B284.b4','euvi',0.,1,'x',284;,/select
return
imagecheckwrapper,'data1/tomography/DATA/euvi/CR2081/B171/','list.B171.proc','euvi',0.,1,'x',171,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.proc','euvi',0.,1,'x',195,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B284/','list.B284.proc','euvi',0.,1,'x',284,/select
imagecheckwrapper,'/data1/tomography/DATA/euvi/CR2081/B304/','list.B304.proc','euvi',0.,1,'x',304,/select

 dir='/data1/tomography/DATA/euvi/CR2081/B284/'
list='list.B284.proc.selected'
apply_mask,dir,list
return
end

pro imagecheckwrapper,datadir,listfile,instrument,Rmin,wn,teamname,band,select=select,pointing=pointing,zbuff=zbuff,para_movie=para_movie

common data,images,dir_and_filenames
common head,hdr
common calibration,team
common typical,typval,minval,maxval
common devi,dev
common contar,conteo

if NOT keyword_set(zbuff) then SET_PLOT,'X'
if NOT keyword_set(zbuff) then dev=     'X'

if     keyword_set(zbuff) then SET_PLOT,'Z'
if     keyword_set(zbuff) then dev=     'Z'

team=teamname
bining=2
if instrument eq 'aia' then bining = 4;xq las imageens vienen en 4096x4096

if NOT keyword_set(select) and NOT keyword_set(pointing) then $
imagecheck,listfile,datadir,instrument,bining,band
if keyword_set(select)  and keyword_set(para_movie) then imagecheck,listfile,datadir,instrument,bining,band,/select,/para_movie
if keyword_set(select)  and not keyword_set(para_movie) then imagecheck,listfile,datadir,instrument,bining,band,/select
if keyword_set(pointing) then imagecheck,listfile,datadir,instrument,bining,band,/pointing

show,instrument,'',wn,1050,Rmin,band,bining,datadir,listfile,dev

if keyword_set(zbuff) then SET_PLOT,'X'

return
end

pro imagecheck,listfile,datadir,instrument,bining,band,select=select,pointing=pointing,iminimum=iminimum,para_movie=para_movie
common data,images,dir_and_filenames
common res,y2,z2
common efem,cl,tilt
common indexes, index_lat,index_lon,index_lon2
common demresults,fbe171,fbe195,fbe284,demc,tc,l1,l2,l3,tmin,fdem
common head,hdr
common calibration,team
common typical,typval,minval,maxval
common devi,dev
common contar,conteo

;"listfile": The file-name at the end of "<instrumentrument>_CONFSTRING"  
;            in headers.h, containing the list of images,
;            and the number of them at the top.
;"datadir" : The RELATIVE to DATABASEDIR path to the images listed in 
;readf,1;            "listfile", with "/" at the end.
;"instrument" : A string, one of these: 'c2', 'mk4', 'eit'.
;"imgtitle": A string, the filename to give to the final GIF file.
;"wn"      : The IDL window number to use.
;"ws"      : The allowed output window maximum linear size (in pixels)

; set graph stuff
if dev eq 'Z' then begin
Device, Decomposed=0, Set_Pixel_Depth=24, Set_Resolution=[1024,1024]
;Device, Decomposed=0, Set_Pixel_Depth=24, Set_Resolution=[512,512]
endif

if not keyword_set(conteo) then conteo =0
;databasedir='/media/disk/data1/tomography/DATA/'
;file=databasedir+datadir+listfile

file =datadir+listfile
file2=datadir+listfile+'.selected'
file5=datadir+listfile+'.skipped'
file3=datadir+listfile+'.pointing'

if NOT keyword_set(select) then $
file4=datadir+listfile+'.disk-medians'

if     keyword_set(select) then $
file4=file2+'.disk-medians'  ; Note that in this case this will keep the medians of the ones SELECTED in this pass, so a new SELECTED suffix is needed.

openw,4,file4

if instrument ne 'aia' then eit_colors,band
if instrument eq 'aia' then aia_lct,wave=band,/load

n=0
x=''
openr,1,file
if NOT keyword_set(iminimum) then iminimum=0
if keyword_set(select) then begin
  openw,2,file2
  fractol =0.10
  iminimum=0
  ;read,'Enter fractional tolerance:',fractol
  ;read,'Enter minimum index:',iminimum
endif
if keyword_set(pointing) then begin
  openw,3,file3
endif

readf,1,n
filenames=strarr(n)
for i=0,n-1 do begin
readf,1,x & filenames(i)=x
endfor
close,1
;se definen vectores a utilizar en el chequeo de las imag.
;serán medianas de distintos valores que sugeriran si una imag esta ok o no.
med1 = fltarr(n) 
med2 = fltarr(n) 
med3 = fltarr(n) 
med4 = fltarr(n) 
minfull = fltarr(n) 
maxfull = fltarr(n) 
flag_empty = 0                  ;siempre debe valer cero

mreadfits,datadir+filenames(0),hdr,ima
np=(size(ima))(1)
;bining=2
np=np/bining

;if keyword_set(select) then openw,2,file2
openw,5,file5

  i0=0
iend=n-1;total de files dentro de la lista.

flag_typval=0
n2=0
n3=0
flag_e=0
;se definen los contadores de filtros
filt1cont = 0
filt2cont = 0
filt3cont = 0
filt4cont = 0
filt5cont = 0
filt6cont = 0
filt7cont = 0
flag_fractol = 0 
;=====

;aqui comienza el loop
for i=i0,iend do begin 
   print,i
   
   mreadfits,datadir+filenames(i),hdr,ima

   if i eq i0 then computegrid,hdr,ra,instrument
;ra es salida va desde 0 a 2.4rsun

   ;chequea valores en el disco
   Med_i = 1.e5
   pgooddisk=where(ra le 1. and ima gt 0.)
   if pgooddisk(0) eq -1 then goto,next
   if pgooddisk(0) ne -1 then begin
      Med_i = median(ima(pgooddisk)) 
      minfull(i) = min (ima(pgooddisk))
      maxfull(i) = max (ima(pgooddisk))
   endif
     
   med1(i) = median(ima( where(ra le 1. and ima gt 0.) )) ;datos de disco
   med2(i) = median(ima( where(ra gt 1.02 and ra le 1.25 and ima gt 0.)  )) ;datos de hollow
   med3(i) = median(ima( where( ra le 1.25 and ima gt 0.)  )) ;todo
   med4(i) = median(ima( where( ra le 1.1 and ra ge 0.85 and ima gt 0.)  )) ;borde del disco
  ;seteo de parametro para primera imagen "buena"
   if flag_typval eq 0 AND pgooddisk(0) ne -1 then begin
      typval=Med_i
      maxval=50.*typval
      minval=.001*typval
      flag_typval = 1
      if keyword_set(select) then printf,2,filenames(i)
      n2=1
      n3=1
      mediansA=fltarr(n3)
      images=fltarr(n2,np,np)
      mediansA(0)=med_i
      images(0,*,*)=rebin(ima,np,np)

      goto,skipfirst
   endif
   
   ppositive=where( ima gt 0. and ra le 1.25) ;pixels positivos en el rango tomografico.
   if keyword_set(select) then begin
      if n_elements(ppositive) eq 1 then begin
         printf,5, filenames(i)
         filt1cont=filt1cont +1
         goto,next
      endif
                                ;OBS para band 211 y 284 puede que no funciona usar 
      if n_elements(ppositive)*1./n_elements(where(ra le 1.25))*1. le 0.75 then begin
         printf,5, filenames(i)
         filt2cont=filt2cont +1
         goto,next
      ;saltea la imagen que tenga menos del 75% de píxeles neg.
      endif

      if instrument eq 'aia' then begin
         if hdr.quality ne 0 then begin
            printf,5, filenames(i)
            filt3cont=filt3cont +1
            goto,next
                                ;saltea la imagen que no tenga buena
                                ;calidad segun el hdr.
         endif
      endif
   
;      if ISA(hdr.crpix1, /STRING) eq 1 or ISA(hdr.crpix2, /STRING) eq 1 then begin
;ISA only works in idl 8.6 or above y chequea si la variable es un string.
      tam1 = size(hdr.crpix1)
      tam2 = size(hdr.crpix2)
      if  tam1(1)eq 7 or tam2(1) eq 7 then begin
         printf,5, filenames(i)
         filt4cont=filt4cont +1
         goto,next
      endif
                                ;saltea aquella imagen donde crpix1/2
                                ;sea un string. 'NaN' (error que se ha
                                ;colado en algunos headers de euvi)
      if finite(hdr.crpix1,/NAN) eq 1 or finite(hdr.crpix2,/NAN) eq 1  then begin
         printf,5, filenames(i)
         filt4cont=filt4cont +1
         goto,next
         ;saltea si crpix1/2 es verdaderamente NAN (no string)
      endif

      ;OBS que la inmagene este rotada no significa que deba tirarse!!!!
      if instrument eq 'euvi' then begin
;funcionan para 1 rotacion en particular, hacer mas robusto el valor
;de crota2, los demas deben andar bien xq el centro suele estar en el
;mismo lugar en todas las rotaciones creo.
      difcrpix1=abs(hdr.crpix1-517.5)
      difcrpix2=abs(hdr.crpix2-525.)
      if difcrpix1 gt 0.2 or difcrpix2 gt 0.2 then begin
         filt6cont=filt6cont +1
         printf,5, filenames(i)
         goto,next
      ;estos valores son grandes, luego se podria refinar el filtro
      endif
      difcrota =abs(hdr.crota2  + 2.) 
      if  difcrota gt 4.  then begin
         filt7cont=filt7cont +1
         printf,5, filenames(i)
         goto,next
      endif
   endif
   endif

    
   if keyword_set(select) and i lt iminimum then printf,2,filenames(i)
   ;OBS: si la primera imag es mala entonces entra aca, y escribe en lista select. CORREGIR
   if keyword_set(select) and i ge iminimum then begin
      difrel=abs(Med_i-mediansA(n3-1))/mediansA(n3-1)
                                ;if hdr.nmissing ne 0 then goto,next
      print,mediansA(n3-1),Med_i,difrel
      if difrel gt fractol or  n_elements(where(ima le 0. and ra le 1.)) gt 100 then begin 
      ;imagenes donde la mediana de la intensidad difiere bastante de la anterior o bien el disco tiene varios pixeles negativos
         ;detecta sectores con pixeles sin data.
         if flag_e eq 1 then  con_errores = [con_errores , i];vector con las posiciones de las imagenes a chequear. 
         if flag_e eq 0 then begin ;primero cae aca.
            con_errores = [i]
            flag_e =1
         endif
         filt5cont=filt5cont +1
         flag_fractol = 1
         printf,5, filenames(i)
         goto,next
      endif
   endif

   if keyword_set(select) then printf,2,filenames(i)
   ;llegado a este punto entonces es buena imagen aunque puede estar rotada.
   ;entonces la info de aca abajo solo corresponde a las imagenes seleccionadas
   n2                = n2+1 ;n2 crece siempre
   n3                = n3+1 ;n3 crece solo cuando la imagen es buena
   mediansA_prev     = mediansA
   images_prev     = images
   mediansA          = fltarr(n3)
   images            = fltarr(n2,np,np)
   images(0:n2-2,*,*)= images_prev
   mediansA(0:n3-2)  = mediansA_prev
   images(n2-1,*,*)  = rebin(ima,np,np)
   mediansA(n3-1)    = Med_i

   
   skipfirst:

   if Med_i ne 1.e5 then printf,4,Med_i,'    '+filenames(i)
   if Med_i eq 1.e5 then printf,4,-666.,'    '+filenames(i)
   
   if keyword_set(pointing) then $
      printf,3,[i,hdr.crpix1,hdr.crpix2,hdr.cdelt1,hdr.cdelt2]
   if flag_empty eq 1 then begin ;flag vacio solo se enta aca con el goto
      next: ;saltea imagenes al final del loop, ya que quedan descartadas.
      if flag_fractol ne 1 then begin ;asi no repite el vector con_errores si viene del loop de fractol (de ese goto)
         if flag_e eq 1 then con_errores = [con_errores , i]
         if flag_e eq 0  then begin ;si la primera imagen erronea viene filtrada
            con_errores = [i]
            flag_e =1
         endif
      endif
      flag_fractol = 0
      n2                = n2+1
      images_prev     = images
      images            = fltarr(n2,np,np)
      images(0:n2-2,*,*)= images_prev
      images(n2-1,*,*)  = rebin(ima,np,np) ;estas imag estan rebineadas a 512 x 512
   endif
 
endfor                    ;termina el loop for




if keyword_set(select) then begin
   print, 'Imagenes filtradas:'
   print, string(filt1cont) + '   Imagenes filtradas xq no tienen pixels positivos'
   print, string(filt2cont) + '   Imagenes filtradas xq menos del 75% son positivos'
   print, string(filt3cont) + '   Imagenes filtradas xq hdr-quality es malo'
   print, string(filt4cont) + '   Imagenes filtradas xq hdr.crpix NaN'
   print, string(filt5cont) + '   Imagenes filtradas xq la mediana de intensidad varia mucho'
   print, string(filt6cont) + '   Imagenes filtradas xq no estan centradas '
   print, string(filt7cont) + '   Imagenes filtradas xq  estan rotadas '
endif

;chequeo estadistico de imagenes.
mediansB = fltarr(iend+1)

if flag_e eq 1 then begin ;si hubo imagenes con intensidades dudosas
   medians_select = fltarr(iend+1)
endif

maxfull_select =maxfull
minfull_select =minfull
      med1_select = med1
      med2_select = med2
      med3_select = med3
      med4_select = med4

   if flag_e eq 1 then begin      
      ;if where(i eq con_errores ) ne -1 then begin ;valores negativos a los elementos que no quiero usar xq las imagenes han sido o bien filtradas o bien difieren mucho la intensidad de la media.
         minfull_select(con_errores) = -555.
         maxfull_select(con_errores) = -555.
         med1_select(con_errores) = -555.
         med2_select(con_errores) = -555.
         med3_select(con_errores) = -555.
         med4_select(con_errores) = -555.
      ;endif
   endif


maxval = median(maxfull)
;minval = median(minfull)

if flag_e eq 1 then begin
   maxval = median(maxfull_select(where(maxfull_select ne -555.)))
   minval = median(minfull_select(where(minfull_select ne -555.))) ;deberia chequearse arriba de 1.02rsun.
endif


if flag_e eq 1 then begin
   datadir2=datadir+'select.'
   meds1 = med1_select(where(med1_select ne -555.))
   meds2 = med2_select(where(med2_select ne -555.))
   meds3 = med3_select(where(med3_select ne -555.))
   meds4 = med4_select(where(med4_select ne -555.))
endif

if flag_e eq 0 then begin
   datadir2=datadir
   meds1 = med1
   meds2 = med2
   meds3 = med3
   meds4 = med4
endif


;SET_PLOT,'X'
;dev=     'X'
filename1=datadir2+listfile+'medianas_imagecheck_disk_'+strmid(string(band),5,3)
tit1= 'rad le 1.'
min = median(meds1) - 3*stdev(meds1)
max = median(meds1) + 3*stdev(meds1)
histoplot,meds1,min=min,max=max,nbins=50,xtit='mediana de imagenes',ytit='histograma de frequencia',tit=tit1,filename=filename1

filename2=datadir2+listfile+'medianas_imagecheck_hollow_'+strmid(string(band),5,3)
tit2= 'rad gt 1.02 rad le 1.25'
min = median(meds2) - 3*stdev(meds2)
max = median(meds2) + 3*stdev(meds2)
histoplot,meds2,min=min,max=max,nbins=50,xtit='mediana de imagenes',ytit='histograma de frequencia',tit=tit2,filename=filename2

filename3=datadir2+listfile+'medianas_imagecheck_full_'+strmid(string(band),5,3)
tit3= 'rad le 1.25'
min = median(meds3) - 3*stdev(meds3)
max = median(meds3) + 3*stdev(meds3)
histoplot,meds3,min=min,max=max,nbins=50,xtit='mediana de imagenes',ytit='histograma de frequencia',tit=tit3,filename=filename3

filename4=datadir2+listfile+'medianas_imagecheck_ring_'+strmid(string(band),5,3)
tit4= 'rad le 1.1 and ra ge 0.85'
min = median(meds4) - 3*stdev(meds4)
max = median(meds4) + 3*stdev(meds4)
histoplot,meds4,min=min,max=max,nbins=50,xtit='mediana de imagenes',ytit='histograma de frequencia',tit=tit4,filename=filename4



;goto,estoahorano
if dev eq 'X' then begin
   device, retain     = 2
   device, true_color = 24
   device, decomposed = 0
   if instrument eq 'aia' then window,xs=1024,ys=1024
   if instrument ne 'aia' then window,xs=512,ys=512
endif

if dev eq 'Z' then begin
   SET_PLOT,'Z'
   dev=     'Z'
   Device, Decomposed=0, Set_Pixel_Depth=24, Set_Resolution=[1024,1024]
endif

minval2=0.05
if band eq 171 then begin
   minval2=0.005
   maxval=mean(maxfull)+1*stdev(maxfull)
endif

if instrument eq 'euvi' then SECCHI_COLORS, 'EUVI', band, R, G, B,/load
if instrument eq 'aia'  then AIA_LCT,wave=band,/load 

if keyword_set(para_movie) then begin
   inames = strarr(conteo+n)
   for i=0,8 do begin
      inames(i)=string('frame00',strcompress(i+1,/remove_all),'.jpg')
   endfor
   for i=9,98 do begin
      inames(i)=string('frame0',strcompress(i+1,/remove_all),'.jpg')
   endfor
   for i=99,n-1+conteo do begin
      inames(i)=string('frame',strcompress(i+1,/remove_all),'.jpg')
   endfor

endif

for i= i0,iend do begin
   ima=reform(images(i,*,*))    ;congrid(reform(images(i,*,*)),512,512)
   ima(0,0)=maxval
   ima(0,1)=minval2
   ima=ima>minval2<maxval  
   tvscl,alog10(ima)
   xyouts,[0.1],[0.05],[filenames(i)],color=255,/normal,charsize=1.5,charthick=2
   record_gif,datadir,filenames(i)+'.gif',dev
   if keyword_set(para_movie) then   WRITE_JPEG, datadir+'video/'+inames(i+conteo), TVRD(/TRUE), /TRUE
endfor

estoahorano:
;SET_PLOT,'X'

;==== termina loop1
close,/all

dir_and_filenames=datadir+filenames

;if dev eq 'X' then window,10
;SET_PLOT,'Z'
;dev=     'Z'
;plot,mediansB/mean(mediansB),psym=4,th=2,ystyle=1
; oplot,mediansB/mean(mediansB) 
; oplot,[0,200],mean(mediansB)/mean(mediansB)*[1,1],linestyle=2
; record_gif,datadir,listfile+'.variability_disk.gif',dev

; window,9
; plot,cacas1/mean(cacas1),psym=4,th=2,ystyle=1
; oplot,cacas1/mean(cacas1)
; oplot,[0,200],mean(cacas1)/mean(cacas1)*[1,1],linestyle=2
;  record_gif,datadir,listfile+'.variability_disk.gif',dev
 medianas, meds1,filename=datadir+listfile+'.variability_disk.gif'
; window,8
;  plot,cacas2/mean(cacas2),psym=4,th=2,ystyle=1 
;  oplot,cacas2/mean(cacas2)
;  oplot,[0,200],mean(cacas2)/mean(cacas2)*[1,1],linestyle=2
; record_gif,datadir,listfile+'.variability_hollow.gif',dev
  medianas,meds2,filename=datadir+listfile+'.variability_hollow.gif'
;  window,7
;  plot,cacas3/mean(cacas3),psym=4,th=2,ystyle=1                                      
;  oplot,cacas3/mean(cacas3)                                                                    
;  oplot,[0,200],mean(cacas3)/mean(cacas3)*[1,1],linestyle=2 
;   record_gif,datadir,listfile+'.variability_full.gif',dev
  medianas, meds3,filename=datadir+listfile+'.variability_full.gif'
;   window,6
;   plot,cacas4/mean(cacas4),psym=4,th=2,ystyle=1
;   oplot,cacas4/mean(cacas4)
;   oplot,[0,200],mean(cacas4)/mean(cacas4)*[1,1],linestyle=2
;   record_gif,datadir,listfile+'.variability_ring.gif',dev
   medianas,meds4,filename=datadir+listfile+'.variability_ring.gif'
   
   if (where(meds1/mean(meds1) ge 1.3))[0] ne -1 then begin
      tempo = where(meds1/mean(meds1) ge 1.3)
      for i=0,n_elements(tempo)-1 do  print, 'figuras a chequear '+filenames(tempo(i))
   endif
   if (where(meds1/mean(meds1) le 0.7))[0] ne -1 then begin
      tempo = where(meds1/mean(meds1) le 0.7)
      for i=0,n_elements(tempo)-1 do  print, 'figuras a chequear '+filenames(tempo(i))
   endif
   
   if (where(meds2/mean(meds2) ge 1.3))[0] ne -1 then begin
      tempo = where(meds2/mean(meds2) ge 1.3)
      for i=0,n_elements(tempo)-1 do  print, 'figuras a chequear '+filenames(tempo(i))
   endif
   if (where(meds2/mean(meds2) le 0.7))[0] ne -1 then begin
      tempo = where(meds2/mean(meds2) le 0.7)
      for i=0,n_elements(tempo)-1 do  print, 'figuras a chequear '+filenames(tempo(i))
   endif

   if (where(meds3/mean(meds3) ge 1.3))[0] ne -1 then begin
      tempo = where(meds3/mean(meds3) ge 1.3)
      for i=0,n_elements(tempo)-1 do  print, 'figuras a chequear '+filenames(tempo(i))
   endif
   if (where(meds3/mean(meds3) le 0.7))[0] ne -1 then begin
      tempo = where(meds3/mean(meds3) le 0.7)
      for i=0,n_elements(tempo)-1 do  print, 'figuras a chequear '+filenames(tempo(i))
   endif

   if (where(meds4/mean(meds4) ge 1.3))[0] ne -1 then begin
      tempo = where(meds4/mean(meds4) ge 1.3)
      for i=0,n_elements(tempo)-1 do  print, 'figuras a chequear '+filenames(tempo(i))
   endif
   if (where(meds4/mean(meds4) le 0.7))[0] ne -1 then begin
      tempo = where(meds4/mean(meds4) le 0.7)
      for i=0,n_elements(tempo)-1 do  print, 'figuras a chequear '+filenames(tempo(i))
   endif
   
;   stop
   conteo=conteo+n
   return
end

;make_movie,'list.A171.DECON_2.proc.selected',171,/euvi
;make_movie,'list.171.processed',171,/aia

pro make_movie,lista,band,sufijo,euvi=euvi,euviB=euviB,aia=aia,dir=dir,outdir=outdir
;dir='/data1/tomography/DATA/euvi/CR2082/A171/'
;for que recorra el pointer y genere un cubo con imagen x las seleccionadas.
  if not keyword_set(dir) then dir='/data1/tomography/DATA/euvi/CR2082/'
;  dir='/data2/tomography/DATA/aia/CR2209/'
  if not keyword_set(outdir) then outdir=''
  if keyword_set(euvi) then begin
     if  band eq 171 then   dir=dir+'A171/'
     if  band eq 195 then   dir=dir+'A195/'
     if  band eq 284 then   dir=dir+'A284/'
  endif

  if keyword_set(euviB) then begin
     if  band eq 171 then   dir=dir+'B171/'
     if  band eq 195 then   dir=dir+'B195/'
     if  band eq 284 then   dir=dir+'B284/'

  endif

  if keyword_set(aia) then begin
     if band eq 171 then dir =dir+'171/'
     if band eq 193 then dir =dir+'193/'
     if band eq 211 then dir =dir+'211/'
     ;if band eq 335 then dir =dir+'335/'
  endif
  outdir=dir+outdir  
  if not keyword_set(sufijo) then sufijo = ''

  openr,1,dir+lista
  readf,1,n
  filenames=strarr(n)
  x=''
  for i=0,n-1 do begin
     readf,1,x & filenames(i)=x
  endfor
  close,1

 im_selected = fltarr(n,512,512)
;  im_selected = fltarr(n,1024,1024)
stop
 for i=0,n-1 do begin
     print,i
     mreadfits,dir+filenames(i),hdr,ima
     im_selected(i,*,*) = rebin(ima,512,512)
  endfor
  maximos = fltarr(n)
  minimos = fltarr(n)

  for i=0,n-1 do begin
     maximos(i) = max (im_selected(i,*,*) >0)
     minimos(i) = 0.01;min ( im_selected(where(im_selected(i,*,*) gt 0.)))
  endfor
  minval3=median(minimos)
  maxval3=median(maximos)+2*stdev(maximos)
  im_selected(*,0,0) = minval3
  im_selected(*,0,1) = maxval3
  data2 = im_selected > minval3 < maxval3

;  dim = size(*ptarr.imagen)
goto,nope
  window,xs=1024,ys=1024
  inames = strarr(n)
  for i=0,8 do begin
     inames(i)=string('frame00',strcompress(i+1,/remove_all),'.jpg')
  endfor
  for i=9,98 do begin
     inames(i)=string('frame0',strcompress(i+1,/remove_all),'.jpg')
  endfor
  for i=99,n-1 do begin
     inames(i)=string('frame',strcompress(i+1,/remove_all),'.jpg')
  endfor


  for i=0,n-1 do begin

     tvlct,red,green,blue,/get
     image=reform(alog10(data2(i,*,*)) )
     bimage=bytscl(image)
     s=size(bimage)
     image3d=bytarr(3,s(1),s(2))
     image3d(0,*,*)=red(bimage)
     image3d(1,*,*)=green(bimage)
     image3d(2,*,*)=blue(bimage)
     ;tvscl,alog10(data2(i,*,*))
     ;xyouts,0.02,0.02,filenames(i),/norm
     ;write_jpeg,dir+inames(i),tvrd()
     xyouts,0.5,0.5,filenames(i),/norm
     write_jpeg,dir+inames(i),image3d,quality=100,true=1 ;WRITE_JPEG, outdir+'testeo3.jpg', TVRD(/TRUE), /TRUE
;     if i eq 0 then stop
  endfor
nope:
  stop
  inames = strarr(n)
if keyword_set(euvi) or keyword_set(euviB) then  SECCHI_COLORS, 'EUVI', band, R, G, B,/load
if keyword_set(aia ) then  AIA_LCT,wave=band,/load
window,xs=512,ys=512

     for i=0,8 do begin
        inames(i)=string('frame00',strcompress(i+1,/remove_all),+sufijo+'.gif')
     endfor
     for i=9,98 do begin
        inames(i)=string('frame0' ,strcompress(i+1,/remove_all),+sufijo+'.gif')
     endfor
     for i=99,n-1 do begin
        inames(i)=string('frame'  ,strcompress(i+1,/remove_all),+sufijo+'.gif')
     endfor
     
     for i=0,n-1 do begin
        tvscl,alog10(data2(i,*,*))
        xyouts,[0.1],[0.05],[filenames(i)],color=255,/normal,charsize=1.5,charthick=2
        record_gif,outdir,inames(i),'X'
     endfor
  
;stop
 return 
end


pro histoplot,data,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename

  ps1,filename+'.eps',0
  f = histogram(data,min=min,max=max,nbins=nbins,locations=vbin) & f = f / total(f)
  plot,vbin,f,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,xstyle=1,thick=3,charthick=2,Font=0

  avg        =   mean(data) & print, avg
  med        = median(data)
  stdev_frac =  stdev(data)/abs(avg)
  cant       = long(n_elements(data))
  xyouts,0.7*[1,1,1,1],1-[0.18,0.25,0.32,0.38],['m='+strmid(string(med),4,6),'!9m!3='+strmid(string(avg),4,6),'!9s!3/!9m!3='+strmid(string(stdev_frac),4,6),'N='+strmid(string(cant),7,7)],/normal,charthick=1,Font=0
  ps2
  return
end

pro medianas,data,filename=filename
  ps1,filename+'.eps',0
  plot,data/mean(data),psym=4,th=2,ystyle=1
  oplot,data/mean(data)
  oplot,[0,200],mean(data)/mean(data)*[1,1],linestyle=2
  ps2
  return
end

pro computegrid,hdr,ra,instrument

 if instrument eq 'euvi' then $
 Rs=hdr.rsun              ; Sun radius in arcsec
 if instrument eq 'aia' then $
 Rs=hdr.rsun_obs          ; Sun radius in arcsec
 px=hdr.cdelt1            ; Pixel size in arcsec                     
 if instrument ne 'eit' then $
 Rs=Rs/px                 ; Sun radius in pixels
 if instrument eq 'eit' then $
 Rs=hdr.solar_r           ; Sun radius in pixels
 px=1./Rs                 ; Pixel size in Rsun units
 iy0=hdr.crpix1-1
 iz0=hdr.crpix2-1
 y = px * (findgen(hdr.naxis1)-iy0)
 z = px * (findgen(hdr.naxis1)-iz0)
 u=1.+fltarr(hdr.naxis1)
 ya=y#u
 za=u#z
 ra = sqrt(ya^2+za^2)

ta = fltarr(hdr.naxis1,hdr.naxis1)
p=where(ya gt 0.)
ta(p) = Acos( za(p) / ra(p) )
p=where(ya lt 0.)
ta(p) = 2.*!pi-Acos( za(p) / ra(p) )
p=where(ya eq 0. AND za gt 0.)
if p(0) ne -1 then ta(p)=0.
p=where(ya eq 0. AND za lt 0.)
if p(0) ne -1 then ta(p)=!pi
ta=2.*!pi-ta

return
end


pro show,instrument,imgtitle,wn,ws,Rmin,band,bining,datadir,listfile,dev

common data,images,dir_and_filenames
common head,hdr
common calibration,team
common typical,typval,minval,maxval

if team eq 'mars' then images=images*1.e-10
images_orig=images

n=fix((size(images))(1))

if instrument eq 'eit' then begin
;get_CL_LAT,logfile,n,cl0_A,T_A
;cl=cl0_a
;tilt=t_A
eit_colors,band
endif

;if instrument eq 'mk4' then begin
;centralval=images(0,hdr.crpix1/bining,hdr.crpix2/bining)
;pp=where(images gt 0.)
;maxdata=   max(images(pp))
;meddata=median(images(pp))
;pc=where(images eq centralval)
;minlim=meddata/(2.*maxdata/meddata)
;maxlim=maxdata*2.
;images(pc)=minlim
;images=images>minlim
;images=images<maxlim
;endif

if instrument eq 'c2'  and team eq 'nrl'  then Rmin=Rmin*(get_solar_radius(hdr)/hdr.cdelt1/bining)
if instrument eq 'c2'  and team eq 'mars' then Rmin=Rmin*hdr.rsun_pix/bining
if instrument eq 'mk4' then Rmin=Rmin*(hdr.rsun/hdr.cdelt1/bining)

npx=hdr.naxis1/bining
diskval=0.
ix=intarr(npx,npx)
iy=intarr(npx,npx)
for i=0,npx-1 do ix(i,*)=indgen(npx)
for i=0,npx-1 do iy(*,i)=indgen(npx)
if team ne 'mars' then $
p=where((ix-hdr.crpix1/bining)^2+(iy-hdr.crpix2/bining)^2 le Rmin^2)
if team eq 'mars' then $
p=where((ix-hdr.xsun/bining)^2+(iy-hdr.ysun/bining)^2 le Rmin^2)

for i=0,n-1 do begin
m=reform(images(i,*,*))
if p(0) ne -1 then m(p)=diskval
images(i,*,*)=m
endfor

if instrument eq 'euvi' then SECCHI_COLORS, 'EUVI', band, R, G, B,/load
if instrument eq 'aia'  then AIA_LCT,wave=band,/load

; compute required window size and required rebin size
nx=fix(sqrt(n))
ny=fix(sqrt(n))
if n gt nx*ny then nx=nx+1
if n gt nx*ny then ny=ny+1
mnrb=fix((ws*1.)/(nx*1.))
nrb=0
i  =0
while nrb eq 0 do begin 
i=i+1
if mnrb le 2^i then nrb=2^(i-1)
endwhile

; set window size
;xss=nrb*nx
;yss=nrb*ny
;stop
xss=1600
yss=1024
if dev eq 'X' then window,wn,xs=xss,ys=yss
if dev eq 'Z' then begin
   SET_PLOT,'Z'
   Device, Decomposed=0, Set_Pixel_Depth=24, Set_Resolution=[xss,yss]
endif
;stop
;loadct,3

; images=images>minval<maxval

;========================
; aca deberia poner img([0,1])=[MINI,MAXI]
; img=img<MIN<MAXI
imagenes_test=fltarr(n,nrb,nrb)
minimos = fltarr(n)
maximos = fltarr(n)
;=======================
;normalize,images

;maxim=max(images)
;images(*,0,0)=maxim

if instrument eq 'c2' or instrument eq 'c3' or instrument eq 'mk4' then begin
for i=0,n-1 do tvscl,alog10(congrid(reform(images(i,*,*)),nrb,nrb)),i
endif
;if instrument eq 'mk4' then begin
;for i=0,n-1 do tvscl,alog10(congrid(reform(images(i,*,*)>1.e-11,nrb,nrb)),i
;endif
if instrument eq 'eit' then begin
for i=0,n-1 do tvscl,alog(congrid(reform(images(i,*,*)),nrb,nrb)),i
endif
if instrument eq 'euvi' or instrument eq 'aia' then begin
   for i=0,n-1 do begin

      imagenes_test(i,*,*) = congrid(reform(images(i,*,*)),nrb,nrb)
   endfor

   for i=0,n-1 do begin
      imagenes_test2 = reform( imagenes_test(i,*,*) )
      p              = where(imagenes_test2 gt 0.)
      minimos[i]     = min(imagenes_test2(p))
      maximos[i]     = max(imagenes_test2(p))
   endfor

   minval2 = median(minimos)
   maxval2 = mean(maximos)+2*stdev(maximos)
   if band eq 171 and instrument eq 'euvi' then maxval2= max(maximos)*1.3
   imagenes_test(*,0,0) = minval2
   imagenes_test(*,0,1) = maxval2
   imagenes_test3 = imagenes_test > minval2 < maxval2

   flag_caca = 1
   gil:
;   stop
   for i=0,n-1 do begin
      ;tvscl,alog(congrid(reform(images(i,*,*)),nrb,nrb)),i
         tvscl,alog(imagenes_test3(i,*,*)),i
      endfor
   flag_caca=0
   if flag_caca eq 1 then goto,gil

endif

;image24 = TVRD(True=1)
;image2d = Color_Quan(image24, 1, r, g, b)
;write_GIF,imgtitle   , image2d, r, g, b

goto,fin

if instrument eq 'c2' then begin
RMAX = 6.1
RMIN = 2.3
npx=512
diskval=1.e-20
window,1,xs=npx,ys=npx
endif

if instrument eq 'c3' then begin
RMAX = 6.1
RMIN = 2.3
npx=512
diskval=1.e-20
window,1,xs=npx,ys=npx
endif

if instrument eq 'mk4' then begin
RMAX = 2.85
RMIN = 1.10
npx=960
diskval=images(0,480,480)
window,1,xs=npx/2,ys=npx/2
endif

radii_to_px=float(npx)/2./Rmax
Rmin=radii_to_px*Rmin
ix0=npx/2-.5
iy0=npx/2-.5
ix=intarr(npx,npx)
iy=intarr(npx,npx)
for i=0,npx-1 do ix(i,*)=indgen(npx)
for i=0,npx-1 do iy(*,i)=indgen(npx)
p=where((ix-ix0)^2+(iy-iy0)^2 le Rmin^2)
for i=0,n-1 do begin
m=reform(images(i,*,*))
m(p)=diskval
images(i,*,*)=m
endfor

for i=0,n-1 do begin
   if instrument eq 'mk4' then $
      tvscl,(rebin(reform(images(i,*,*)),480,480))^.3
   if instrument eq 'c2' or instrument eq 'c3' then $
      tvscl,reform(images(i,*,*))^.3
;image24 = TVRD(True=1)
;image2d = Color_Quan(image24, 1, r, g, b)
   if i le 9 then strnum='0'+strmid(string(i),7,1)
   if i gt 9 then strnum=strmid(string(i),6,2)
   write_GIF,'/home/albert/Desktop/'+instrument+'.image.'+strnum+'.gif', image2d, r, g, b
endfor

fin:
;stop
record_gif,datadir,listfile+'.allframes.gif',dev

images=images_orig
;stop
return
end

;findvoxel,90,112,74,'log_euvi.B.195.cr2071.26x90_bf4_ri.98_ro1.025',2,/euvi

;findvoxel,90,110,61,'log_euvi.BcompA.195.2008.Mar20-Apr12.26x90_bf4_ri.98_ro1.025',2,/euvi

;findvoxel,90,138,64,'log_euvi.AB.195.cr2095.17days.26x90_bf4_ri.98_ro1.025',2,/euvi

;findvoxel,90,170,18,'log_euvi.AB.171.2008.Nov20-Dec11.26x90_bf4_ri.98_ro1.025',2,/euvi

;-------------------------------------------------------------------------
pro findvoxel,Nlat,nim,imnum,logfile,binning,eit=eit,euvi=euvi

common data,images,dir_and_filenames
common res,y2,z2
common efem,cl,tilt
common indexes, index_lat,index_lon,index_lon2
common demresults,fbe171,fbe195,fbe284,demc,tc,l1,l2,l3,tmin,fdem

; set graph stuff
device, retain     = 2
device, true_color = 24
device, decomposed = 0

get_CL_LAT,logfile,nim,cl,tilt
mreadfits,dir_and_filenames(imnum),header,image

print,dir_and_filenames(imnum)

SECCHI_COLORS, 'EUVI', header.WAVELNTH, R, G, B,/load

npix=fix((size(image))(1))

; Rebinning
if binning gt 1 then begin
npix=npix/binning
image=rebin(image,npix,npix)
; C' = 1 + (C-1) * (N/B-1)/(N-1).
header.CRPIX1 = 1 + (header.CRPIX1 -1) * (header.NAXIS1/binning - 1) / (header.NAXIS1-1)
header.CRPIX2 = 1 + (header.CRPIX2 -1) * (header.NAXIS2/binning - 1) / (header.NAXIS2-1)
header.NAXIS1 = header.NAXIS1 /binning
header.NAXIS2 = header.NAXIS2 /binning
header.CDELT1 = header.CDELT1 *binning
header.CDELT2 = header.CDELT2 *binning
endif

CENTER_Y = header.crpix1-1
CENTER_Z = header.crpix2-1

image(CENTER_Y,*)=max(image)
image(*,CENTER_Z)=max(image)

window,xs=npix,ys=npix
tvscl,alog10(image>.005)
cursor,y,z,/wait

print,y,z
stop

if keyword_set(eit) then radperfov=512./header.solar_R

if keyword_set(euvi) then begin
 rsun_in_arcsec=header.Rsun
pixel_in_arcsec=header.cdelt1
radperfov=npix*pixel_in_arcsec/rsun_in_arcsec
endif
stop
y2=(y-CENTER_Y/float(npix))*radperfov
z2=(z-CENTER_Z/float(npix))*radperfov
skip:
;read,'y2: ',y2
;read,'z2: ',z2

print,'Selected point (y2,z2,r2) [Rsun]:',y2,z2,sqrt(y2^2+z2^2)

cl0=   cl[imnum] ; CL [-180,+180]
  t= tilt[imnum] ; in deg, t>0 is NP towards observer

; For test purposes:
;   t = -45.
; cl0 = 100.

; convert CL to grid [0,360]
if cl0 lt 0. then cl0=cl0+360.
print,' TILT and Meridian CL [0-360]:',t,cl0
s2s3,cl0,t,Nlat 

;getdem,index_rad,index_lat,index_lon
stop
return
end

;---------------------------------------------------------------------
;cl0=central meridian C.R., must be in degs, in range [-180,+180]
;t=tilt (it is the disk center pixel latitude)
;Nlat,Nlon: Numer of lat CELLS, so that grid points are Nlat+1
;(y2,z2) are the image pixel coordinates in Rsun units

pro s2s3,cl0,t,Nlat
common res,y2,z2
common indexes, index_lat,index_lon,index_lon2

Nlon=2*Nlat

; change CL and TILT to radians.
cl0 = cl0 * !pi/180.
  t = t   * !pi/180.

 r2=sqrt(y2^2+z2^2)
 eps=1.e-10
 if r2 gt 1. then y2=y2/(r2+eps)
 if r2 gt 1. then z2=z2/(r2+eps)
 print,'Corrected point (y2,z2,r2) [Rsun]:',y2,z2,sqrt(y2^2+z2^2)

      tiny=1.e-10

      X2 = Sqrt(1-((Y2^2+Z2^2)))

Latitude = ( Asin( X2 * Sin(T) + Z2 * Cos(T) ) )>(-!pi/2.)<(!pi/2.)
     Lat = Latitude + !pi/2. ; South pole:Lat=0 
                             ; North pole:Lat=pi

      cosl=Cos(Latitude)

      CL = CL0 + Asin( (Y2 / (cosl>tiny))>(-1.)<(1.) )
if CL gt 2.*!pi then CL=CL-2*!pi
if CL lt 0.     then CL=CL+2*!pi

      CL2= CL0 + Acos( ((X2 * Cos(T) - Z2 * Sin(T)) / (cosl>tiny))>(-1.)<(1.) )
if CL2 gt 2.*!pi then CL2=CL2-2*!pi
if CL2 lt 0.     then CL2=CL2+2*!pi

      if (X2 * Cos(T) - Z2 * Sin(T)) eq 0. then $
      CL3=CL0 + Atan(Y2/tiny)
      if (X2 * Cos(T) - Z2 * Sin(T)) ne 0. then $
      CL3=CL0 + Atan(Y2/(X2 * Cos(T) - Z2 * Sin(T)))
if CL3 gt 2.*!pi then CL3=CL3-2*!pi
if CL3 lt 0.     then CL3=CL3+2*!pi

 Dlat =    !pi/Nlat
 Dlon = 2.*!pi/Nlon


 print,'Latitudinal and Longitudinal coordinates at selected (or corrected) point'
 print,'Lat [-90,+90] and CLs [0-360]:',[Latitude,CL,CL3,CL2]*180./!pi

 if sqrt(y2^2+z2^2) gt 1. then begin
 print,'Pixel is out of the disk, can not compute indexes.'
 return
 endif

 index_lat = fix(Lat/Dlat)
 index_lon = fix(CL /Dlon)
 index_lon2= fix(CL2/Dlon)
 index_lon3= fix(CL3/Dlon)
 print,'      Cell indexes Ilat,Ilons:',[index_lat,index_lon,index_lon3,index_lon2]

 return
end

;---------------------------------------------------------------------
pro get_CL_LAT,logfile,n,cl0_A,T_A
common data,images,dir_and_filenames
x=strarr(1)
openr,1,'/data1/tomography/bindata/'+logfile
openw,2,'/data1/tomography/bindata/'+logfile+'.CL0'
openw,3,'/data1/tomography/bindata/'+logfile+'.TILT'
for i=0,n-1 do begin
readf,1,x
;print,
;if i eq 30 then print,x
i0=0
for j=0,strlen(x[0])-3 do begin
if strmid(x[0],j,4) eq 'cl= ' then i0=j+4
if i0 ne 0 then goto,out1
endfor
out1:
printf,2,strmid(x[0],i0,6)
i0=0
for j=0,strlen(x[0])-3 do begin
if strmid(x[0],j,4) eq 'ng= ' then i0=j+4
if i0 ne 0 then goto,out2
endfor
out2:
printf,3,strmid(x[0],i0,6)
endfor
close,1
close,2
close,3

CL0_A=fltarr(n)
  T_A=fltarr(n)
openr,2,'/data1/tomography/bindata/'+logfile+'.CL0'
readf,2,CL0_A
openr,3,'/data1/tomography/bindata/'+logfile+'.TILT'
readf,3,T_A
close,/all

return
end

pro read_results,demfile
common demresults,fbe171,fbe195,fbe284,demc,tc,l1,l2,l3,tmin,fdem

fdem=demfile

sca=1
tmin=0.
tmax=0.
nr=0
nt=0
np=0
openr,1,'/data1/dem/'+demfile
readu,1,sca,tmin,tmax,nr,nt,np
results=dblarr(nr,nt,np,8)                                            
readu,1,results            
close,1

;nr=13
;nt=60
;np=120

demc=dblarr(nr,nt,np)
  tc=dblarr(nr,nt,np)
  l1=dblarr(nr,nt,np)
  l2=dblarr(nr,nt,np)
  l3=dblarr(nr,nt,np)
fbe171=dblarr(nr,nt,np)
fbe195=dblarr(nr,nt,np)
fbe284=dblarr(nr,nt,np)

for ir=0,nr-1 do begin
for it=0,nt-1 do begin
for ip=0,np-1 do begin
fbe171(ir,it,ip)=results(ir,it,ip,0)
fbe195(ir,it,ip)=results(ir,it,ip,1)
fbe284(ir,it,ip)=results(ir,it,ip,2)
  demc(ir,it,ip)=results(ir,it,ip,3)
    tc(ir,it,ip)=results(ir,it,ip,4)
    l1(ir,it,ip)=results(ir,it,ip,5)
    l2(ir,it,ip)=results(ir,it,ip,6)
    l3(ir,it,ip)=results(ir,it,ip,7)
endfor
endfor
endfor

return
end

pro getdem,ir,it,ip
common demresults,fbe171,fbe195,fbe284,demc,tc,l1,l2,l3,tmin,fdem

print,''
print,'------------------------------------------'
print,'DEM Analysis at selected voxel: (Temps in [MK])'
print,'         FBE:',[fbe171(ir,it,ip),fbe195(ir,it,ip),fbe284(ir,it,ip)]
print,'Single T,DEM:',[tc(ir,it,ip)/1.e6,demc(ir,it,ip)]
print,'   Amp,T0,dT:',[l1(ir,it,ip),l2(ir,it,ip)*Tmin/1.e6,l3(ir,it,ip)*Tmin/1.e6]
print,'------------------------------------------'
print,''

return
end

;-------------------------------------------------------------------------
pro diskaverage,imnum,eit=eit,euvi=euvi
common data,images,dir_and_filenames
common results,mn,md

mreadfits,dir_and_filenames(imnum),header,image

CENTER_Y = header.crpix1
CENTER_Z = header.crpix2

npix=header.naxis1

if keyword_set(eit) then radperfov=npix/header.solar_R

if keyword_set(euvi) then begin
 rsun_in_arcsec=header.Rsun
pixel_in_arcsec=header.cdelt1
radperfov=npix*pixel_in_arcsec/rsun_in_arcsec
endif

disk_selector=fltarr(npix,npix)
for y=0,npix-1 do begin
for z=0,npix-1 do begin
y2=(y-CENTER_Y)*radperfov/npix
z2=(z-CENTER_Z)*radperfov/npix
r2=y2^2+z2^2
;if r2 le 0.980                 then disk_selector(y,z)=1.
;if r2 ge 1.025 and r2 le 1.250 then disk_selector(y,z)=1.
 if r2 ge 0.980 and r2 le 1.025 then disk_selector(y,z)=1.
endfor
endfor
disk=where(disk_selector eq 1.)
diskvalues=reform(image(disk))
p=where(diskvalues ne -999.)
gooddiskvalues=diskvalues(p)
mn=mean(gooddiskvalues)
md=median(gooddiskvalues)
print,'Average/Median disk intensity:',mn,md

eit_colors,header.wavelnth
;image(CENTER_Y,*)=max(image)
;image(*,CENTER_Z)=max(image)
tvscl,alog10(image>1)
return
end

;-------------------------------------------------------------------------
pro diskaveragedistribution,eit=eit,euvi=euvi
common data,images,dir_and_filenames
common results,mn,md

nim=fix((size(images))(1))
avgs=fltarr(nim)
mdns=fltarr(nim)
for i=0,nim-1 do begin
if keyword_set(euvi) then diskaverage,i,/euvi
if keyword_set(eit)  then diskaverage,i,/eit
avgs(i)=mn
mdns(i)=md
endfor
print,mean(mdns),(max(mdns)-min(mdns))/mean(mdns)
print,mean(mdns),mean(avgs)
;stop
return
end

;-------------------------------------------------------------------------
pro diskhistogram,dir,file,lsty,eit=eit,euvi=euvi,over=over

; set graph stuff
device, retain     = 2
device, true_color = 24
device, decomposed = 0

mreadfits,dir+file,header,image  

CENTER_Y = header.crpix1
CENTER_Z = header.crpix2

npix=header.naxis1
;image(CENTER_Y,*)=max(image)
;image(*,CENTER_Z)=max(image)

if keyword_set(eit) then radperfov=npix/header.solar_R

if keyword_set(euvi) then begin
 rsun_in_arcsec=header.Rsun
pixel_in_arcsec=header.cdelt1
radperfov=npix*pixel_in_arcsec/rsun_in_arcsec
endif

disk_selector=fltarr(npix,npix)
for y=0,npix-1 do begin
for z=0,npix-1 do begin
y2=(y-CENTER_Y)*radperfov/npix
z2=(z-CENTER_Z)*radperfov/npix
r2=y2^2+z2^2
if r2 le 1.0 then disk_selector(y,z)=1.
endfor
endfor

disk=where(disk_selector eq 1.)
diskvalues=reform(image(disk))
p=where(diskvalues ne -999.)
gooddiskvalues=diskvalues(p)

mn=mean(gooddiskvalues)
md=median(gooddiskvalues)
print,'Average/Median disk intensity:',mn,md
print,min(gooddiskvalues),max(gooddiskvalues)

;eit_colors,header.wavelnth
;tvscl,alog10(image>1)

if header.wavelnth eq 171 then begin
print,'171'
minval=0.5
maxval=200.
endif
if header.wavelnth eq 195 then begin
print,'195'
minval=0.5
maxval=400.
endif
if header.wavelnth eq 284 then begin
print,'284'
minval=0.1
maxval=10.
endif

nbins=100
;minval=min(gooddiskvalues)
;maxval=max(gooddiskvalues)
dv=(maxval-minval)/nbins
 v=minval+dv/2.+dv*findgen(nbins)
nvals=n_elements(gooddiskvalues)
histo=fltarr(nbins)
i=0L
next:
v0=gooddiskvalues(i)
p=fix( ( where(abs(v-v0) eq min(abs(v-v0))) )(0) )
histo(p)=histo(p)+1.
i=i+1L
if i le nvals-1 then goto,next

if not keyword_set(over) then  plot,v,histo,linestyle=lsty,/xlog;,yr=[0,6000]
if     keyword_set(over) then oplot,v,histo,linestyle=lsty

return
end

;-------------------------------------------------------------------------
; order files as: B,EIT,A
pro compare3,dir,files

; set graph stuff
device, retain     = 2
device, true_color = 24
device, decomposed = 0

mreadfits,dir+files(0),headerB,imageB
mreadfits,dir+files(1),headerE,imageE
mreadfits,dir+files(2),headerA,imageA

mna=fltarr(3)
mda=fltarr(3)
nbins=300
histoA=fltarr(nbins,3)
vA=fltarr(3)

for i=0,2 do begin

if i eq 0 then begin
 header=headerB
 image =imageB
 inst='euviB'
endif
if i eq 1 then begin
 header=headerE
 image =imageE
 inst='eit'
endif
if i eq 2 then begin
 header=headerA
 image =imageA
 inst='euviA'
endif

CENTER_Y = header.crpix1
CENTER_Z = header.crpix2

npix=header.naxis1

if inst eq 'eit' then radperfov=512./header.solar_R

if strmid(inst,0,4) eq 'euvi' then begin
 rsun_in_arcsec=header.Rsun
pixel_in_arcsec=header.cdelt1
radperfov=npix*pixel_in_arcsec/rsun_in_arcsec
endif

disk_selector=fltarr(npix,npix)
for y=0,511 do begin
for z=0,511 do begin
y2=(y-CENTER_Y)*radperfov/npix
z2=(z-CENTER_Z)*radperfov/npix
r2=y2^2+z2^2
if r2 le 1.0 then disk_selector(y,z)=1.
endfor
endfor

disk=where(disk_selector eq 1.)
diskvalues=reform(image(disk))
p=where(diskvalues ne -999.)
gooddiskvalues=diskvalues(p)

mn=mean(gooddiskvalues)
md=median(gooddiskvalues)

mna(i)=mn
mda(i)=md

print,'Average/Median disk intensity:',mn,md
print,'Min and Max disk values:      ',min(gooddiskvalues),max(gooddiskvalues)

if header.wavelnth eq 171 then begin
print,'171'
minval=1.
maxval=50.
endif
if header.wavelnth eq 195 then begin
print,'195'
minval=1.
maxval=50.
endif
if header.wavelnth eq 284 then begin
print,'284'
minval=.1
maxval=50.
endif

dv=(maxval-minval)/nbins
 v=minval+dv/2.+dv*findgen(nbins)
nvals=n_elements(gooddiskvalues)
histo=fltarr(nbins)
j=0L
next:
v0=gooddiskvalues(j)
p=fix( ( where(abs(v-v0) eq min(abs(v-v0))) )(0) )
histo(p)=histo(p)+1.
j=j+1L
if j le nvals-1 then goto,next

histoA(*,i)=histo
endfor


window,4,xs=768,ys=768
!p.multi=0

 plot,v,histoA(*,0),/xlog,yr=[0,max(histoA(10:nbins-1,*))]*1.5,xstyle=1,psym=4
oplot,v,histoA(*,1)
oplot,v,histoA(*,2),psym=5

top=max([imageB,imageE,imageA])

eit_colors,headerB.WAVELNTH
tvscl,alog10(rebin(imageB,256,256)>1.e-1<top),0
tvscl,alog10(rebin(imageE,256,256)>1.e-1<top),1
tvscl,alog10(rebin(imageA,256,256)>1.e-1<top),2

stop
return
end

pro profile,datadir,listfile,im,hor=hor,ver=ver

window,xs=1024,ys=512
!p.multi=0
;!p.multi=[0,2,1]

; set graph stuff
device, retain     = 2
device, true_color = 24
device, decomposed = 0

databasedir='/data1/tomography/DATA/'
file=databasedir+datadir+listfile
n=0
x=''
openr,1,file
readf,1,n
filenames=strarr(n)
for i=0,n-1 do begin
readf,1,x & filenames(i)=x
endfor
close,1
mreadfits,databasedir+datadir+filenames(im),hdr,ima

icx=hdr.crpix1
icy=hdr.crpix2+25

if keyword_set(hor) then begin
xmin=float(  0-icx)/(hdr.RSUN/hdr.CDELT1)
xmax=float(511-icx)/(hdr.RSUN/hdr.CDELT1)
x=xmin+(xmax-xmin)*findgen(512)/float(511)
 plot,x,ima(*,icy)>0.,psym=1
oplot,[-1,-1],[0,1]*500
oplot,[ 1, 1],[0,1]*500
i=where(x lt -1.)
ia=max(i)
i=where(x gt +1.)
ib=min(i)
loadct,12
oplot,[x(ia  )],[ima(ia  ,icy)],color=200,psym=1;,th=2
oplot,[x(ib  )],[ima(ib  ,icy)],color=200,psym=1;,th=2
oplot,[x(ia+1)],[ima(ia+1,icy)],color=110,psym=1;,th=2
oplot,[x(ib-1)],[ima(ib-1,icy)],color=110,psym=1;,th=2
loadct,0
ima(*,icy)=max(ima)
endif

if keyword_set(ver) then begin
ymin=float(  0-icy)/(hdr.RSUN/hdr.CDELT2)
ymax=float(511-icy)/(hdr.RSUN/hdr.CDELT2)
y=ymin+(ymax-ymin)*findgen(512)/float(511)
 plot,y,ima(icx,*),psym=1
oplot,[-1,-1],[0,1]*100
oplot,[ 1, 1],[0,1]*100
i=where(y lt -1.)
ia=max(i)
i=where(y gt +1.)
ib=min(i)
loadct,12
oplot,[y(ia  )],[ima(icx,ia  )],color=200,psym=1;,th=2
oplot,[y(ib  )],[ima(icx,ib  )],color=200,psym=1;,th=2
oplot,[y(ia+1)],[ima(icx,ia+1)],color=110,psym=1;,th=2
oplot,[y(ib-1)],[ima(icx,ib-1)],color=110,psym=1;,th=2
loadct,0
ima(icx,*)=max(ima)
endif

window,1,xs=512,ys=512
eit_colors,hdr.wavelnth
tvscl,alog10(ima>.1)

stop

return
end

pro playimages,images,band,wn,waittimeinsecs

window,wn,xs=512,ys=512
SECCHI_COLORS, 'EUVI', band, R, G, B,/load

nim=fix((size(images))(1))

for i=0,nim-1 do begin
    if i le   9              then name='00'+strmid(string(i),7,1)
    if i ge  10 and i le  99 then name= '0'+strmid(string(i),6,2)
    if i ge 100 and i le 999 then name=strmid(string(i),5,3)
    tvscl,alog10(rebin(reform(images(i,*,*)),512,512)>.1)
;    record_gif,'~/Desktop/Movie/','frame-'+name+'.gif'
    print,i
    wait,waittimeinsecs
endfor

return
end

FUNCTION GET_SOLAR_RADIUS, hdr, PIXEL=PIXEL, DISTANCE=distance
;
;+
; NAME:
;       GET_SOLAR_RADIUS
;
; PURPOSE:
;       This function returns the radius of the sun in arc seconds.
;
; CATEGORY:
;       LASCO_ANALYSIS
;
; CALLING SEQUENCE:
;       Result = GET_SOLAR_RADIUS (Hdr)
;
; INPUTS:
;       Hdr:    A LASCO header structure
;
; KEYWORDS:
;	PIXEL	Output result in pixels
;	DISTANCE	Set equal to variable which will contain distance 
;			to sun in km
;
; OUTPUTS:
;       solar radius in arc seconds
;
;
; PROCEDURE:
;	Determines the solar radius from the position of SOHO as
;	defined in the *.cdf format files.
;
; MODIFICATION HISTORY:
;       Written by:     D.A. Biesecker, 12 September 1996
;	General routine taken from POINTING3.PRO written by S.P. Plunkett
;       Modified 30 Sept. 1996 by DAB: return default values of the solar
;        radius if calls to read the SOHO orbit files fail.
;	RA Howard 5/20/97	Added keyword PIXEL to return solar radius in pixels
;	NB Rich 8/25/98		Use detector instead of telescop to allow MVIHDR
;	NB Rich 12/16/98	Use get_orbit_cdf2 instead of get_orbit_cdf
;	NB Rich 11/13/01	Use solar_ephem instead of sohoephem if linux is OS
;	NB Rich 07/15/03	Ditto for darwin (MaxOSX)
;	NB Rich 04.03.29	Fix /pixel for linux case
;   NB Rich, 05.08.19 - Try to make general for any (FITS) header
;
;	08/19/05 @(#)get_solar_radius.pro	1.13 - LASCO IDL LIBRARY
;-
;

   IF datatype(hdr) NE 'STC' THEN hdr=lasco_fitshdr2struct(hdr)
   tags=tag_names(hdr)
   tel = STRTRIM(STRUPCASE(hdr.detector),2)
   ; !!! NOTE: MVI hdr does not have TELESCOP or INSTRUME !!! -nbr, 05.08.19
   telnum = where(tel EQ ['C1','C2','C3','EIT'], soho)

;  Check to see whether LASCO or HAO-MLO
;
   IF keyword_set(PIXEL) and ((tel EQ 'MK3') or (tel EQ 'MK4')) THEN $
        return, hdr.CRRADIUS
   
;
; Obtain the midpoint time of the observation, 'YYYY/MM/DD HH:MM:SS.SSS'
;
	date_obs = anytim2utc(hdr.date_d$obs+' '+hdr.time_d$obs)
	tai_obs = utc2tai(date_obs) + hdr.exptime / 2.
	tdb_obs = tai_obs + 32.184
	tdb_utc = tai2utc(tdb_obs, /ecs)
;
; Return a default value for the solar radius, if no true number can
; be found.
;
        default = 960.d0
        IF (KEYWORD_SET(pixel))  THEN default = default/GET_SEC_PIXEL(hdr)

;
; Use solar_ephem.pro for linux systems, because of problems with executing external
; binary in sohoephem.
;
IF !version.os EQ 'linux' OR !version.os EQ 'darwin' OR NOT(soho) THEN BEGIN
    yymmdd = utc2yymmdd(date_obs)
    solar_ephem,yymmdd,radius=radius,SOHO=soho
    radius = radius*3600
    
ENDIF ELSE BEGIN
; BEGIN SOHO-specific computation from external
    ;
    ; Obtain the Julian Date (needed to get the solar ephemeris)
    ;

	    jd = cds2jd(tdb_utc)
	    tjd = jd.int + jd.frac

    ;
    ; Determine which type of ORBIT files exist.  
    ;

    type = orbit_file_type(anytim2utc(tdb_utc))

    ; If the files are of type 'CDF' then determine whether to
    ; use GET_ORBIT_CDF or GET_ORBIT_CDF2.  If the date is on or before
    ; March 10, 1996, use GET_ORBIT_CDF.  Otherwise, use GET_ORBIT_CDF2.
    ;

    icoord = 1                      ; if SOHO coordinates not found, use L1 coords.
    if type eq 'CDF' then begin
      cdf = tdb_obs lt utc2tai('11-Mar-1996 00:00:00.000')

    ;
    ; Read the appropriate CDF format file.
    ;

      if cdf then soho_orbit = get_orbit_cdf2(tdb_utc) else begin
        check = execute('soho_orbit = get_orbit_cdf2(tdb_utc)')
        if not check then return, default ; Return if execute failed.
      endelse

      hec_pos = soho_orbit.hec_pos / 1.495979d8
      hec_vel = soho_orbit.hec_vel * (8.64d5 / 1.495979d8)
      icoord = 2
    ;
    ; Or else read the FITS file if it exists.  If not, return a default
    ; value of solar radius
    ;

    endif else if type eq 'FITS' then begin
        check = execute('soho_orbit = get_orbit_fits(tdb_utc)')

       if not check then return, default ; Return if execute failed.

        hec_pos = [soho_orbit.hec_x, soho_orbit.hec_y, soho_orbit.hec_z]
        distance = SQRT(TOTAL(double(hec_pos)^2))
        hec_pos = hec_pos / 1.495979d8
        hec_vel = [soho_orbit.hec_vx, soho_orbit.hec_vy, soho_orbit.hec_vz]
        hec_vel = hec_vel * (8.64d5 / 1.495979d8)
        icoord = 2
    endif else if type eq 'NULL' then return, default

    ;
    ; Obtain the SOHO orbit parameters.  Distance (AU) and velocity
    ; (AU/day).
    ;

	    hec_orbit = [hec_pos, hec_vel]

    ;
    ; Obtain the ephemerides for the sun and planets
    ;

	    sohoephem, tjd, hec_orbit, icoord, sunephem, planephem


    ;
    ; Return the radius of the sun (arcseconds).
    ;

    RADIUS = (sunephem.diameter.arcmin*60.d + sunephem.diameter.arcsec) / 2.d

ENDELSE

IF (KEYWORD_SET(pixel))  THEN radius = radius/GET_SEC_PIXEL(hdr)

RETURN, radius
END

pro readeuvi,dir,file,ima,ra,hdr

common maps,ya,za,Rs_arcsec
;dir='/data1/tomography/DATA/euvi/2008.04/proc.bin.res/'
mreadfits,dir+file,hdr,ima
;rotate,ima,hdr

; Sun radius in arcsec
Rs_arcsec = hdr.rsun
; Pixel size in arcsec
px = hdr.cdelt1
; Sun radius in pixels
Rs = Rs_arcsec / px
; Pixel size in Rsun units
px = 1./Rs
;Central pixel
iy0 = hdr.crpix1
iz0 = hdr.crpix2
;Y and Z vectors
y = px * (findgen(hdr.naxis1)-iy0)
z = px * (findgen(hdr.naxis1)-iz0)
;Y and Z arrays
 u=1.+fltarr(hdr.naxis1)
ya=y#u
za=u#z
;r arrays
ra = sqrt(ya^2+za^2)
;theta arrays
ta = fltarr(hdr.naxis1,hdr.naxis1)
p=where(ya gt 0.)
ta(p) = Acos( za(p) / ra(p) )
p=where(ya lt 0.)
ta(p) = 2.*!pi-Acos( za(p) / ra(p) )
p=where(ya eq 0. AND za gt 0.)
if p(0) ne -1 then ta(p)=0.
p=where(ya eq 0. AND za lt 0.)
if p(0) ne -1 then ta(p)=!pi

eit_colors,hdr.wavelnth

;stop
return
end

pro normalize,images

mini=.01
maxi=3000.

images=images>mini<maxi

images(*,0,0)=maxi
images(*,0,1)=mini

return
end


pro makemovie,images,dir
n=(size(images))(1)
npx=512
window,5,xs=npx,ys=npx
for frame=0,n-1 do begin
 tvscl,alog10(images(frame,*,*)) 
 if frame ge   0 and frame le   9 then numberstring='00'+strmid(string(frame),7,1)
 if frame ge  10 and frame le  99 then numberstring= '0'+strmid(string(frame),6,2)
 if frame ge 100 and frame le 999 then numberstring=     strmid(string(frame),5,3)
 record_gif,dir,'frame-'+numberstring+'.gif' 
endfor

return
end

; checkcrota,'/Storage1TB/tomography/DATA/euvi/CR2077/B171/','list.171.B','graph.171.eps'
; checkcrota,'/Storage1TB/tomography/DATA/euvi/CR2077/B195/','list.195.B','graph.195.eps'
; checkcrota,'/Storage1TB/tomography/DATA/euvi/CR2077/B284/','list.284.B','graph.284.eps'
pro checkcrota,dir,file,outfile

n=0
openr,1,dir+file
readf,1,n
crota=fltarr(n)
crpix1=fltarr(n)
crpix2=fltarr(n)
filename=''
for i=0,n-1 do begin
readf,1,filename
mreadfits,dir+filename,hdr,img
crota(i)=hdr.crota2
crpix1(i)=hdr.crpix1
crpix2(i)=hdr.crpix2
endfor
close,1

medcrota=median(crota)
medcrpix1=median(crpix1)
medcrpix2=median(crpix2)

dcrota=(crota(1:n-1)-crota(0:n-2))
mcrota=(crota(1:n-1)+crota(0:n-2)) /2.

dcrpix1=crpix1(1:n-1)-crpix1(0:n-2)
dcrpix2=crpix2(1:n-1)-crpix2(0:n-2)

!p.multi=[0,1,3]
ps1,outfile,0
;plot,crota/medcrota  ,yr=[min(crota) ,max(crota) ]/medcrota ,ystyle=1,title='CROTA  / median(CROTA)'
;plot,crpix1/medcrpix1,yr=[min(crpix1),max(crpix1)]/medcrpix1,ystyle=1,title='CRPIX1 / median(CRPIX1)'
;plot,crpix2/medcrpix2,yr=[min(crpix2),max(crpix2)]/medcrpix2,ystyle=1,title='CRPIX2 / median(CRPIX2)'

plot,dcrpix1,yr=[min(dcrpix1),max(dcrpix1)],title='Delta(CRPIX1_i)',ystyle=1,xstyle=1,psym=4,th=2
plot,dcrpix2,yr=[min(dcrpix2),max(dcrpix2)],title='Delta(CRPIX2_i)',ystyle=1,xstyle=1,psym=4,th=2
plot,1024.*dcrota*!pi/180.,yr=[min(dcrota),max(dcrota)]*1024.*!pi/180.,ystyle=1,title='1024 * Delta(CROTA_i [rad])',xstyle=1,xtitle='Image Pair #i',psym=4,th=2

ps2
!p.multi=0

stop
return
end

pro apply_mask,dir,list
mreadfits,'/data1/work/image.processing/mask_factor.fts'  ,hdr,mask_factor
mreadfits,'/data1/work/image.processing/mask_constant.fts',hdr,mask_constant
n=0
x=''
openr,1,dir+list
readf,1,n
for i=0,n-1 do begin
readf,1,x
mreadfits,dir+x,header,image
maskedimage=image*mask_factor+mask_constant
MWRITEFITS, header,maskedimage, outfile=dir+x+'.masked'
endfor
close,1
return
end

; correct_jump,'/data1/tomography/DATA/aia/CR2107/131/','list.131.processed.disk-medians.selected','list.131.selected.adjusted',290,0.964449

; correct_jump,'/data1/tomography/DATA/aia/CR2107/335/','list.335.processed.disk-medians.selected','list.335.selected.adjusted',295,0.699870
pro correct_jump,dir,infile,outfile,index0,factor

n=0
openr,1,dir+infile
openw,2,dir+outfile

 readf,1,n
printf,2,n

b=''

for i=0,n-1 do begin
readf,1,b
b = strmid(b,11,46)

if i lt index0 then begin
  printf,2,b
endif

if i ge index0 then begin
  mreadfits,dir+b,hdr,ima
  ima=ima*factor
  bnew=strmid(b,0,strlen(b)-3)+'adjusted.fts'
  MWRITEFITS, hdr, ima, outfile=dir+bnew
  printf,2,bnew
endif

endfor
close,/all

return
end


pro  check_medians,dir,listfile

n=0
openr,1,dir+listfile
readf,1,n
close,1

medians=fltarr(n)

openr,1,dir+listfile+'.disk-medians'
mediana = 0.d
cadena  = ''
for i=0,n-1 do begin
readf,1,mediana,cadena
medians[i] = mediana
endfor
close,1

window,0
 plot,medians/mean(medians),title='Medians',yr=[min(medians),max(medians)]/mean(medians),psym=4
oplot,medians/mean(medians)
oplot,[0,n-1],[1,1]
record_gif,dir,listfile+'.disk-medians.gif'

return
end
