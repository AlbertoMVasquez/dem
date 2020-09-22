pro read_awsom_matrix,dir=dir,suff_file=suff_file,nr=nr,nt=nt,np=np,ne_out=ne_out,te_out=te_out,qrad_out=qrad_out,qheat_out=qheat_out,qebyq_out=qebyq_out,nelasco_out=nelasco_out,$
                      Vr_out=Vr_out,Vth_out=Vth_out,Vph_out=Vph_out,output;,te,n_e,qrad,qheat,qebyq,ne_lasco,Vr,Vth,Vph
  if not keyword_set(dir) then dir = '/data1/work/MHD/'
  if keyword_set(ne_out) then begin
     n_e=fltarr(nr,nt,np)
     openr,1,dir+'Ne_'+suff_file
     readu,1,n_e
     close,1
     output=n_e
  endif

  if keyword_set(te_out) then begin
     te=fltarr(nr,nt,np)
     openr,1,dir+'Te_'+suff_file
     readu,1,Te
     close,1
     output=Te
  endif
  if keyword_set(qrad_out) then begin
     qrad=fltarr(nr,nt,np)
     openr,1,dir+'qrad_'+suff_file
     readu,1,qrad
     close,1
     output=qrad
  endif

  if keyword_set(qheat_out) then begin
     qheat=fltarr(nr,nt,np)
     openr,1,dir+'qheat_'+suff_file
     readu,1,qheat
     close,1
     output=qheat
  endif
  if keyword_set(qebyq_out) then begin
     qebyq=fltarr(nr,nt,np)
     openr,1,dir+'qebyq_'+suff_file
     readu,1,qebyq
     close,1
     output=qebyq
  endif
  if keyword_set(nelasco_out) then begin
     ne_lasco=fltarr(nr,nt,np)
     openr,1,dir+'ne_lasco_'+suff_file
     readu,1,ne_lasco
     close,1
     output=ne_lasco
  endif

  if keyword_set(Vr_out) then begin
     Vr=fltarr(nr,nt,np)
     openr,1,dir+'Vr_'+suff_file
     readu,1,Vr
     close,1
     output=Vr
  endif

  if keyword_set(Vth_out) then begin
     Vth=fltarr(nr,nt,np)
     openr,1,dir+'Vth_'+suff_file
     readu,1,Vth
     close,1
     output=Vth
  endif

    if keyword_set(Vph_out) then begin
       Vph=fltarr(nr,nt,np)
       openr,1,dir+'Vph_'+suff_file
       readu,1,Vph
       close,1
       output=Vph
    endif


  return
end
