pro read_awsom_matrix,dir=dir,suff_file=suff_file,nr=nr,nt=nt,np=np,n_e=n_e,te=te,te_awsom=te_awsom,ne_awsom=ne_awsom
  if not keyword_set(dir) then dir = '/data1/work/MHD/'
  if keyword_set(n_e) then begin
     ne_awsom=fltarr(nr,nt,np)
     openr,1,dir+'Ne_'+suff_file
     readu,1,Ne_awsom
     close,1
  endif
  if keyword_set(te) then begin
     te_awsom=fltarr(nr,nt,np)
     openr,1,dir+'Te_'+suff_file
     readu,1,Te_awsom
     close,1
  endif

  return
end
