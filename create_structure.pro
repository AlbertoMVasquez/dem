; create_structure,'fdips_field_150x180x360_synop_Mr_0.polfil.2081.dat',/writefile
; create_structure,'fdips_field_150x180x360_synop_Mr_0.polfil.1974.out',/writefile
; create_structure,'fdips_field_151x180x361_mrmqs110425t2355c2109_000.out',/writefile
; create_structure,'CR2106_PFSSM_GNG_151X180X361_FDIPS.dat'
pro create_structure,inputfile,writefile=writefile
  common pfssm,rB,thB,phB,Br,Bth,Bph,B,nrB,nphB,nthB,Bint
  common structure,sph_data

  print,inputfile
  nname=strlen(inputfile)
  suffix=strmid(inputfile,nname-5,nname-1)

  if suffix eq 'ubdat' then begin
     NrB=0
     NthB=0
     NphB=0
     openr,1,'/data1/DATA/PFSSM/'+inputfile
     readu,1,NrB,NthB,NphB
     r=fltarr(NrB)
     th=fltarr(NthB)
     ph=fltarr(NphB)
     Br_new=fltarr(NphB,NthB,NrB)
     Bth_new=fltarr(NphB,NthB,NrB)
     Bph_new=fltarr(NphB,NthB,NrB)
     readu,1,r,th,ph
     readu,1,Br_new,Bth_new,Bph_new
     close,1
     sph_data = {        BR:  ptr_new( Br_new)                            ,$
                         BTH: ptr_new(Bth_new)                            ,$
                         BPH: ptr_new(Bph_new)                            ,$
                         BDERIVS: ptr_new()                               ,$
                         NR:    NrB*1L                                    ,$
                         NLAT: nthB*1L                                    ,$
                         NLON: nphB*1L                                    ,$
                         RIX: ptr_new( r *1.d)                            ,$
                         THETA: ptr_new( th*1.d)                          ,$
                         PHI: ptr_new( ph*1.d)                            ,$
                         LAT: ptr_new( 90-th/!dtor       *1.d )           ,$
                         LON: ptr_new( ph/!dtor          *1.d )           ,$
                         LONBOUNDS: dblarr(2)-1.                          ,$
                         STR: ptr_new() ,STTH: ptr_new() ,STPH: ptr_new() ,$
                         PTR: ptr_new() ,PTTH: ptr_new() ,PTPH: ptr_new() ,$
                         NSTEP: ptr_new() ,EXTRA_OBJECTS: ptr_new()          }
  endif else begin

     read_PFSSM,inputfile     
;--Take out last LON point-----
     NphB=NphB-1
     phB = phB(*,*,0:nphB-1)
;------------------------------
; Re-order the dimensions of Br, Bth, and Bph, so that for each array: 
; the 3rd dimension (index=2) becomes 1st,
; the 2nd dimension (index=1) stays   2nd,
; the 1st dimension (index=0) becomes 3rd. 
     Br_new  = TRANSPOSE(  Br(*,*,0:nphB-1) , [2, 1, 0] ) 
     Bth_new = TRANSPOSE( Bth(*,*,0:nphB-1) , [2, 1, 0] ) 
     Bph_new = TRANSPOSE( Bph(*,*,0:nphB-1) , [2, 1, 0] ) 
 
     sph_data = {        BR:  ptr_new( Br_new)                              ,$
                         BTH: ptr_new(Bth_new)                              ,$
                         BPH: ptr_new(Bph_new)                              ,$
                         BDERIVS: ptr_new()                                 ,$
                         NR:   NrB*1L                                       ,$
                         NLAT: nthB*1L                                      ,$
                         NLON: nphB*1L                                      ,$
                         RIX:   ptr_new( reform(rB(*,0,0))*1.d )            ,$
                         THETA: ptr_new( reform(90.-thB(0,*,0))*!pi/180.d ) ,$
                         PHI:   ptr_new( reform(phB(0,0,*))*!pi/180.d )     ,$
                         LAT:   ptr_new( reform(thB(0,*,0))*1.d )           ,$
                         LON:   ptr_new( reform(phB(0,0,*))*1.d )           ,$
                         LONBOUNDS: dblarr(2)-1.                            ,$
                         STR: ptr_new() ,STTH: ptr_new() ,STPH: ptr_new()   ,$
                         PTR: ptr_new() ,PTTH: ptr_new() ,PTPH: ptr_new()   ,$
                         NSTEP: ptr_new() ,EXTRA_OBJECTS: ptr_new()          }

stop
;----------------------------------------------------------------------------------------
     if keyword_set(writefile) then begin
        nname=strlen(inputfile)
        outputfile=strmid(inputfile,0,nname-4)+'.ubdat'
        openw,1,'/data1/DATA/PFSSM/'+outputfile
        writeu,1,NrB,NthB,NphB
        writeu,1,reform(rB(*,0,0)),reform(90.-thB(0,*,0))*!pi/180.,reform(phB(0,0,*))*!pi/180.
        writeu,1,Br_new,Bth_new,Bph_new
        close,1
     endif
;---------------------------------------------------------------------------------------
  endelse
  return
end


pro read_PFSSM,inputfile
  common pfssm,rB,thB,phB,Br,Bth,Bph,B,nrB,nphB,nthB,Bint
     
     x=''
     nrB=0
     nthB=0
     nphB=0
     openr,1,'/data1/DATA/PFSSM/'+inputfile
     for i=1,2 do readf,1,x
     readf,1,nrB,nphB,nthB
     for i=1,2 do readf,1,x
     rB  =fltarr(nrB,nthB,nphB)
     thB =fltarr(nrB,nthB,nphB)
     phB =fltarr(nrB,nthB,nphB)
     Br  =fltarr(nrB,nthB,nphB)
     Bth =fltarr(nrB,nthB,nphB)
     Bph =fltarr(nrB,nthB,nphB)
     B   =fltarr(nrB,nthB,nphB)
     rB_0=0.
     thB_0=0.
     phB_0=0.
     Br_0=0.
     Bth_0=0.
     Bph_0=0.
     for ith=0,nthB-1 do begin
        for iph=0,nphB-1 do begin
           for ir =0, nrB-1 do begin
              readf,1, rB_0, phB_0, thB_0, Br_0, Bph_0, Bth_0
              rB(ir,ith,iph) =  rB_0
              thB(ir,ith,iph) = thB_0 *180./!pi
              phB(ir,ith,iph) = phB_0 *180./!pi
              Br(ir,ith,iph) =  Br_0
              Bth(ir,ith,iph) = Bth_0
              Bph(ir,ith,iph) = Bph_0
           endfor
        endfor
     endfor
     close,1
     p    = where(Br ne 0.)
     B(p) = sqrt(Br(p)^2+Bth(p)^2+Bph(p)^2) * Br(p)/abs(Br(p))
     Bunit=1. 
     B=B/Bunit
     print,'-----> Bunit =',Bunit

     return
  end
