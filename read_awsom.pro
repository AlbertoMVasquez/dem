;read_awsom,'3dAWSoM_DEMT_LASCO_1.85.dat',grilla_demt=1.26,te_out='Te_awsom_2208_1.85',ne_out='Ne_awsom_2208_1.85'
;read_awsom,'CR2082_r=1-6_1deg_AWSoM.dat',grilla_demt=1.26,te_out='Te_awsom_2208_1.85',ne_out='Ne_awsom_2208_1.85',/interpol
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat',grilla_demt=1.26,te_out='Te_awsom_2082_1.85',ne_out='Ne_awsom_2082_1.85',/interpol
;read_awsom,'CR2208_grid1X1_ADAPT_GONG_AWSOM.dat','awsom_2208_1.85',grilla_demt=1.26,/te_out,/ne_out,/B_sph_out,/interpol,N1=25
;read_awsom,'CR2208_grid1X1_ADAPT_GONG_AWSOM.dat','awsom_2208_1.85',grilla_demt=1.26,N1=25,/sph_data
pro read_awsom,inputfile,file_out,dir_out=dir_out,grilla_demt=grilla_demt,te_out=te_out,ne_out=ne_out,B_sph_out=B_sph_out,interpol=interpol,N1=N1,sph_data=sph_data
;  common grilla_chip,r_grilla,theta_grilla,phi_grilla,ne_awsom,te_awsom,rho_awsom,er_awsom,ti_awsom,ne_lasco

;file out es un string, nombre de archivo

;grilla_demt es input  para recortar las matrices entre 2 radios
;grilla_demt se espera que sea un escalar si va de 1.025 hasta
;grilla_demt(0) o bien un vector de 2 dimensiones y en tal caso la
;matriz ira de rilla_demt(0) a grilla_demt(1) en la parte radial
;sin modificar th y ph (90 y 180)

;te_out y ne_out keywords que se usan para guardar las matrices.

;N1 es un entero, cantida de lineas del header del file.
if not keyword_set(dir_out) then dir_out='/data1/work/MHD/'
  x = 0.
  y = 0.
  z = 0.
  rho = 0.
  vx = 0.
  vy = 0.
  vz = 0.
  te = 0.
  tp = 0.
  bx = 0.
  by = 0.
  bz = 0.
  i01 = 0.
  i02 = 0.
  qrad = 0.
  qheat = 0.
  qebyq = 0.
  qparbyq = 0.
  n_e = 0.
  ne_lasco = 0.
  
  Nphi=360.
  nph=360.
  Ntheta=180
  nth=180
  Nr=500
  
  xx=''
  if not keyword_set(N1) then  N1=26 ;el header de los datos de chip suele tener 26 lineas
  r_grilla       = fltarr(nr,nth,nph)
  theta_grilla   = fltarr(nr,nth,nph)
  phi_grilla     = fltarr(nr,nth,nph)
  
;  r_grilla2      = fltarr(nr,nth,nph)
;  theta_grilla2  = fltarr(nr,nth,nph)
;  phi_grilla2    = fltarr(nr,nth,nph)
  
  ne_awsom       = fltarr(nr,nth,nph)
  te_awsom       = fltarr(nr,nth,nph)
  rho_awsom      = fltarr(nr,nth,nph)
  er_awsom       = fltarr(nr,nth,nph)
  tp_awsom       = fltarr(nr,nth,nph)
  ne_lasco_awsom = fltarr(nr,nth,nph)

  if keyword_set (B_sph_out) then begin
     Br  = fltarr(nr,nth,nph)
     Bth = fltarr(nr,nth,nph)
     Bph = fltarr(nr,nth,nph)
  endif

  if keyword_set (V_field) then begin
     Vr  = fltarr(nr,nth,nph)
     Vth = fltarr(nr,nth,nph)
     Vph = fltarr(nr,nth,nph)
  endif

; inputfile = '3dAWSoM_DEMT_LASCO_1.85.dat'
  openr,1,'/data1/work/MHD/'+inputfile
  
  for i=1L,N1-1 do begin
     readf,1,xx
  endfor

  for iph=0,Nphi-1 do begin
     for ith=0,Ntheta-1 do begin
        for ir =0, Nr-1 do begin
;           readf,1, x,y,z,vx,vy,vz,tp,te,bx,by,bz,i01,i02,qrad,qheat,qebyq,qparbyq,n_e,ne_lasco
;           readf,1, x,y,z,rho,vx,vy,vz,te,tp,bx,by,bz,i01,i02,qrad,qheat,qebyq,n_e,ne_lasco
           readf,1, x,y,z,rho,vx,vy,vz,te,tp,bx,by,bz,i01,i02,qrad,qheat,qebyq,n_e
           V=[x,y,z]
           cart_to_sphcoord,V,sphcoord
           ;r  = sqrt(x^2+y^2+z^2)
           ;th = acos(z/r)
           ;ph =          atan(y/x)
           ;if x gt 0 and y lt 0. then ph = 2*!dpi + atan(y/x)
           ;if x lt 0             then ph =   !dpi - atan(y/abs(x))
           r_grilla(ir,ith,iph)     = sphcoord[0]         ;r          
           theta_grilla(ir,ith,iph) = sphcoord[1] ;th *180./!pi         
           phi_grilla(ir,ith,iph)   = sphcoord[2] ;ph *180./!pi         
;           r_grilla2(ir,ith,iph)     = r
;           theta_grilla2(ir,ith,iph) = th *180./!pi
;           phi_grilla2(ir,ith,iph)   = ph *180./!pi
           
           ne_awsom(ir,ith,iph)       = n_e
           te_awsom(ir,ith,iph)       = te
           rho_awsom(ir,ith,iph)      = rho
           er_awsom(ir,ith,iph)       = qrad
           tp_awsom(ir,ith,iph)       = tp
;           ne_lasco_awsom(ir,ith,iph) = ne_lasco
           
           if keyword_set (B_sph_out) then begin
              transform_b_cart_to_sph,sphcoord[1],sphcoord[2],[Bx,By,Bz],B_sph
              Br(ir,ith,iph)  = B_sph[0]
              Bth(ir,ith,iph) = B_sph[1]
              Bph(ir,ith,iph) = B_sph[2]
           endif
           
           if keyword_set (Vfield) then begin
              transform_b_cart_to_sph,sphcoord[1],sphcoord[2],[vx,vy,vz],V_sph
              Vr(ir,ith,iph)  = V_sph[0]
              Vth(ir,ith,iph) = V_sph[1]
              Vph(ir,ith,iph) = V_sph[2]
           endif

        endfor
     endfor
  endfor
  close,1

  if keyword_set(grilla_demt) then begin
     if n_elements(grilla_demt) eq 1 then begin
        ok= where(r_grilla[*,0,0] le grilla_demt)
        ne_awsom = ne_awsom[ok,*,*]
        te_awsom = te_awsom[ok,*,*]
     endif
     if n_elements(grilla_demt) eq 2 then begin
       ok= where(r_grilla[*,0,0] ge grilla_demt(0) and r_grilla[*,0,0] le grilla_demt(2))
       ne_lasco_awsom = ne_lasco_awsom[ok,*,*]
     endif
  endif

  if keyword_set(interpol) then begin
     nrads = n_elements(ok)
     nlat2 = 90
     nlon2 = 180
     ne_awsom_interp  = fltarr(nrads,nlat2,nlon2)
     te_awsom_interp  = fltarr(nrads,nlat2,nlon2)

     for ir=0,nrads-1 do begin
        A1 = reform(ne_awsom(ir,*,*))
        B1 = reform(te_awsom(ir,*,*))
        inter,A1=A1,A2=A2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180;,Lat1=Lat1,Lon1=Lon1,Lat2=Lat2,Lon2=Lon2
        inter,A1=B1,A2=B2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180;,Lat1=Lat1,Lon1=Lon1,Lat2=Lat2,Lon2=Lon2
        ne_awsom_interp[ir,*,*] = A2
        te_awsom_interp[ir,*,*] = B2

     endfor
  endif


  if keyword_set(ne_out) then begin
     openw,2,dir_out+'Ne_'+file_out
     writeu,2,ne_awsom_interp
     close,2
  endif
  if keyword_set(te_out) then begin
     openw,3,dir_out+'Te_'+file_out
     writeu,3,te_awsom_interp
     close,3
  endif
 
  if keyword_set(B_sph_out) then begin
     openw,4,dir_out+'Br_'+file_out
     writeu,4,Br
     close,4
     openw,5,dir_out+'Bth_'+file_out
     writeu,5,Bth
     close,5
     openw,6,dir_out+'Bph_'+file_out
     writeu,6,Bph
     close,6
  endif


  if keyword_set(sph_data) then begin

     Br_new  = TRANSPOSE(  Br , [2, 1, 0] )
     Bth_new = TRANSPOSE( Bth , [2, 1, 0] )
     Bph_new = TRANSPOSE( Bph , [2, 1, 0] )
     rg  = reform(    r_grilla(*,0,0))
     thg = reform(theta_grilla(0,*,0))     
     phg = reform(  phi_grilla(0,0,*))
          
     sph_data = {        BR: ptr_new( Br_new)                            ,$
                         BTH: ptr_new(Bth_new)                           ,$
                         BPH: ptr_new(Bph_new)                           ,$
                         BDERIVS: ptr_new()                              ,$
                         NR: long(Nr)                                    ,$
                         NLAT: long(Nth)                                 ,$
                         NLON: long(Nph)                                 ,$
                         RIX: ptr_new(  rg * 1.d )                       ,$
                         THETA: ptr_new( thg * 1.d )                     ,$
                         PHI: ptr_new( phg * 1.d )                       ,$
                         LAT: ptr_new( thg - 90.d)                       ,$
                         LON: ptr_new(  reform(phg(0,0,*) ))             ,$
                         LONBOUNDS: dblarr(2)-1.                         ,$
                         STR: ptr_new() ,STTH: ptr_new() ,STPH: ptr_new(),$
                         PTR: ptr_new() ,PTTH: ptr_new() ,PTPH: ptr_new(),$
                         NSTEP: ptr_new() ,EXTRA_OBJECTS: ptr_new()       }     
     
     stop
     save,sph_data,FILENAME = 'sph_data_'+file_out+'.sav'
  endif



  return
end

