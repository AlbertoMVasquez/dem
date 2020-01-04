z;load_mapoc,'CR2082_90x180blines_r_',1.065,/mhd,mapoc,file='CR2082_90X180blines_r_1.105_open-close-map_MHD.dat'

pro load_mapoc,filesuffix,rc,mdi=mdi,gng=gng,mhd=mhd,mapoc,dir=dir,file=file
 

  if not keyword_set(dir) then dir ='/data1/work/MHD/'
  if keyword_set(mdi) then postsuffix='_MDI.dat'
  if keyword_set(gng) then postsuffix='_GNG.dat'
  if keyword_set(mhd) then postsuffix='_MHD.dat'
  nr = 26
  nth= 90
  np =180
  mapoc=fltarr(nr,nth,np)
  tmp=fltarr(1,nth,np)



  stringheight=strmid(string(rc),6,5)
  openclosefile=filesuffix+stringheight+'_open-close-map'+postsuffix
  if keyword_set(file) then openclosefile=file
  print,'-----> O/C: '+openclosefile
  openr,1,dir+openclosefile
  readu,1,tmp
  close,1

  dr=0.01
  radv=1.+dr/2.+dr*findgen(nr)
  f = (radv -rc)^2
  ;ir = where(radv eq rc)
  ir = where(f eq min(f))
  mapoc(ir,*,*)=tmp(0,*,*)
  return
end
