pro read_awsom_matrix,dir=dir,suff_file=suff_file,nr=nr,nt=nt,np=np,n_e=n_e,te=te,qrad=qrad,qheat=qheat,qebyq=qebyq,ne_lasco=ne_lasco,te,n_e,qrad,qheat,qebyq,ne_lasco
  if not keyword_set(dir) then dir = '/media/Data/data1/work/MHD/'
  if keyword_set(n_e) then begin
     n_e=fltarr(nr,nt,np)
     openr,1,dir+'Ne_'+suff_file
     readu,1,N_e
     close,1
  endif
  if keyword_set(te) then begin
     te=fltarr(nr,nt,np)
     openr,1,dir+'Te_'+suff_file
     readu,1,Te
     close,1
  endif
  if keyword_set(qrad) then begin
     qrad=fltarr(nr,nt,np)
     openr,1,dir+'qrad_'+suff_file
     readu,1,qrad
     close,1
  endif
  if keyword_set(qheat) then begin
     qheat=fltarr(nr,nt,np)
     openr,1,dir+'qheat_'+suff_file
     readu,1,qheat
     close,1
  endif
  if keyword_set(qebyq) then begin
     qebyq=fltarr(nr,nt,np)
     openr,1,dir+'qebyq_'+suff_file
     readu,1,qebyq
     close,1
  endif
  if keyword_set(ne_lasco) then begin
     ne_lasco=fltarr(nr,nt,np)
     openr,1,dir+'ne_lasco'+suff_file
     readu,1,ne_lasco
     close,1
  endif

  return
end
