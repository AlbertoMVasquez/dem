;read_awsom,'3dAWSoM_DEMT_LASCO_1.85.dat',grilla_demt=1.26,te_out='Te_awsom_2208_1.85',ne_out='Ne_awsom_2208_1.85'
;read_awsom,'CR2082_r=1-6_1deg_AWSoM.dat',grilla_demt=1.26,te_out='Te_awsom_2208_1.85',ne_out='Ne_awsom_2208_1.85',/interpol
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat',grilla_demt=1.26,te_out='Te_awsom_2082_1.85',ne_out='Ne_awsom_2082_1.85',/interpol
;read_awsom,'CR2208_grid1X1_ADAPT_GONG_AWSOM.dat','awsom_2208_1.85',grilla_demt=1.26,N1=25,/sph_data
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat','awsom_2082_1.85',grilla_demt=1.26,/sph_data
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat','awsom_2082_1.85',/te_out,/ne_out,/qrad_out,/qheat_out,/qebyq_out,/ne_lasco_out,/interpol
;read_awsom,'CR2208_grid1X1_ADAPT_GONG_AWSOM.dat','awsom_2208_1.85',/sph_data,N1=25
;read_awsom,'CR2208_grid1X1_ADAPT_GONG_AWSOM.dat','awsom_2208_1.85',grilla_demt=1.26,/te_out,/ne_out,/interpol,N1=25
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat','awsom_2082_1.85_short',grilla_demt=1.26,/te_out,/ne_out,/interpol,N1=26
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat','awsom_2082_1.85_short',grilla_demt=1.26,/qrad,/interpol,N1=26,/te_out,/ne_out
;read_awsom,'CR2208_grid1X1_ADAPT_GONG_AWSOM.dat','awsom_2208_1.85',grilla_demt=1.26,/qrad,/interpol,N1=25
;-----
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat','awsom_2082_1.85_extend',/interpol,N1=26,/B_out,/V_out
;read_awsom,'CR2208_grid1X1_ADAPT_GONG_AWSOM.dat','awsom_2208_1.85_extend',/interpol,N1=25,/B_out,/V_out
;-----
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat','awsom_2082_1.85_extended',/sph_data
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat','awsom_2082_1.85_extended',/B_out
pro read_awsom,inputfile,file_out,dir_out=dir_out,grilla_demt=grilla_demt,te_out=te_out,ne_out=ne_out,qrad_out=qrad_out,qheat_out=qheat_out,qebyq_out=qebyq_out,ne_lasco_out=ne_lasco_out,B_out=B_out,interpol=interpol,N1=N1,sph_data=sph_data,v_out=v_out
;  common grilla_chip,r_grilla,theta_grilla,phi_grilla,ne_awsom,te_awsom,rho_awsom,er_awsom,ti_awsom,ne_lasco

;file out es un string, nombre de archivo

;grilla_demt es input  para recortar las matrices entre 2 radios
;grilla_demt se espera que sea un escalar si va de 1.025 hasta
;grilla_demt(0) o bien un vector de 2 dimensiones y en tal caso la
;matriz ira de grilla_demt(0) a grilla_demt(1) en la parte radial
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
  
  ne_awsom          = fltarr(nr,nth,nph)
  te_awsom          = fltarr(nr,nth,nph)
  rho_awsom         = fltarr(nr,nth,nph)
  qrad_awsom        = fltarr(nr,nth,nph)
  qheat_awsom       = fltarr(nr,nth,nph)
  qebyq_awsom       = fltarr(nr,nth,nph)
  tp_awsom          = fltarr(nr,nth,nph)
  ne_lasco_awsom    = fltarr(nr,nth,nph)

  Bxx = fltarr(nr,nth,nph)
  Byy = fltarr(nr,nth,nph)
  Bzz = fltarr(nr,nth,nph)
  B_cart_mod = fltarr(nr,nth,nph)

  if keyword_set (B_out) or keyword_set(sph_data) then begin
     Br  = fltarr(nr,nth,nph)
     Bth = fltarr(nr,nth,nph)
     Bph = fltarr(nr,nth,nph)
     B_mod = fltarr(nr,nth,nph)
  endif

  if keyword_set (v_out) then begin
     Vr  = fltarr(nr,nth,nph)
     Vth = fltarr(nr,nth,nph)
     Vph = fltarr(nr,nth,nph)
  endif


  openr,1,'/data1/work/MHD/'+inputfile
  
  for i=1L,N1-1 do begin
     readf,1,xx
  endfor

  for iph=0,Nphi-1 do begin
     for ith=0,Ntheta-1 do begin
        for ir =0, Nr-1 do begin
;           readf,1, x,y,z,vx,vy,vz,tp,te,bx,by,bz,i01,i02,qrad,qheat,qebyq,qparbyq,n_e,ne_lasco
;           readf,1,
;           x,y,z,rho,vx,vy,vz,te,tp,bx,by,bz,i01,i02,qrad,qheat,qebyq,n_e,ne_lasco                 ;PARA 2082
           readf,1, x,y,z,rho,vx,vy,vz,te,tp,bx,by,bz,i01,i02,qrad,qheat,qebyq,n_e                  ;PARA 2208
           V=[x,y,z]
           cart_to_sphcoord,V,sphcoord

           r_grilla(ir,ith,iph)     = sphcoord[0]         ;r          
           theta_grilla(ir,ith,iph) = sphcoord[1] ;th *180./!pi         
           phi_grilla(ir,ith,iph)   = sphcoord[2] ;ph *180./!pi         
           
           ne_awsom(ir,ith,iph)          = n_e
           te_awsom(ir,ith,iph)          = te
           rho_awsom(ir,ith,iph)         = rho
           qrad_awsom(ir,ith,iph)        = qrad *(-10.) ;esto implica un cambio de signo y un cambio de unidades de J/m3 -> 10*Erg/cm3
           qheat_awsom(ir,ith,iph)       = qheat
           qebyq_awsom(ir,ith,iph)       = qebyq
           tp_awsom(ir,ith,iph)          = tp
           ne_lasco_awsom(ir,ith,iph)    = ne_lasco

           Bxx(ir,ith,iph) = bx
           Byy(ir,ith,iph) = by
           Bzz(ir,ith,iph) = bz
           B_cart_mod(ir,ith,iph) = bx^2 + by^2 +bz^2
           if B_cart_mod(ir,ith,iph) le 0 then stop ;this should not happen

           if keyword_set (B_out) or keyword_set(sph_data) then begin
              cord_th = sphcoord[1]*!dtor
              cord_ph = sphcoord[2]*!dtor
              transform_b_cart_to_sph,cord_th,cord_ph,[Bx,By,Bz],B_sph
              Br(ir,ith,iph)  = B_sph[0]
              Bth(ir,ith,iph) = B_sph[1]
              Bph(ir,ith,iph) = B_sph[2]
              B_mod(ir,ith,iph) = B_sph[0]^2 + B_sph[1]^2 + B_sph[2]^2
              if B_mod(ir,ith,iph) le 0 then stop
           endif
           
           if keyword_set (v_out) then begin
              cord_th = sphcoord[1]*!dtor
              cord_ph = sphcoord[2]*!dtor
              transform_b_cart_to_sph,cord_th,cord_ph,[vx,vy,vz],V_sph
              Vr(ir,ith,iph)  = V_sph[0]
              Vth(ir,ith,iph) = V_sph[1]
              Vph(ir,ith,iph) = V_sph[2]
           endif

        endfor
     endfor
  endfor
  close,1
stop
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
     if keyword_set(grilla_demt)     then nrads = n_elements(ok)
     if not keyword_set(grilla_demt) then nrads = Nr
     nlat2 = 90
     nlon2 = 180
goto,ahorano
     ne_awsom_interp       = fltarr(nrads,nlat2,nlon2)
     te_awsom_interp       = fltarr(nrads,nlat2,nlon2)
     rho_awsom_interp      = fltarr(nrads,nlat2,nlon2)
     qrad_awsom_interp     = fltarr(nrads,nlat2,nlon2)
     qheat_awsom_interp    = fltarr(nrads,nlat2,nlon2)
     qebyq_awsom_interp    = fltarr(nrads,nlat2,nlon2)
     ne_lasco_awsom_interp = fltarr(nrads,nlat2,nlon2)
ahorano:
     if keyword_set (B_out) then begin
        Br_interp     = fltarr(nrads,nlat2,nlon2)
        Bth_interp    = fltarr(nrads,nlat2,nlon2)
        Bph_interp    = fltarr(nrads,nlat2,nlon2)
     endif
     if keyword_set (v_out) then begin
        Vr_interp      = fltarr(nrads,nlat2,nlon2)
        Vth_interp     = fltarr(nrads,nlat2,nlon2)
        Vph_interp     = fltarr(nrads,nlat2,nlon2)
     endif

     for ir=0,nrads-1 do begin
goto,no1
        A1 = reform(ne_awsom(ir,*,*))
        B1 = reform(te_awsom(ir,*,*))
        C1 = reform(rho_awsom(ir,*,*))
        D1 = reform(qrad_awsom(ir,*,*))
        E1 = reform(qheat_awsom(ir,*,*))
        F1 = reform(qebyq_awsom(ir,*,*))
        G1 = reform(ne_lasco_awsom(ir,*,*))
        inter,A1=A1,A2=A2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
        inter,A1=B1,A2=B2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
        inter,A1=C1,A2=C2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
        inter,A1=D1,A2=D2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
        inter,A1=E1,A2=E2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
        inter,A1=F1,A2=F2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
        inter,A1=G1,A2=G2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
        ne_awsom_interp[ir,*,*]       = A2
        te_awsom_interp[ir,*,*]       = B2
        rho_awsom_interp[ir,*,*]      = C2     
        qrad_awsom_interp[ir,*,*]     = D2      
        qheat_awsom_interp[ir,*,*]    = E2   
        qebyq_awsom_interp[ir,*,*]    = F2   
        ne_lasco_awsom_interp[ir,*,*] = G2
no1:
        if keyword_set (B_out) then begin
           H1 = reform(Br (ir,*,*))
           I1 = reform(Bth(ir,*,*))
           J1 = reform(Bph(ir,*,*))
           inter,A1=H1,A2=H2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
           inter,A1=I1,A2=I2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
           inter,A1=J1,A2=J2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
           Br_interp  [ir,*,*]       = H2
           Bth_interp [ir,*,*]       = I2
           Bph_interp [ir,*,*]       = J2
        endif
        if keyword_set (v_out) then begin
           K1 = reform(Vr  (ir,*,*))
           L1 = reform(Vth (ir,*,*))
           M1 = reform(Vph (ir,*,*))
           inter,A1=K1,A2=K2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
           inter,A1=L1,A2=L2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
           inter,A1=M1,A2=M2,Nlat1=180,Nlon1=360,Nlat2=90,Nlon2=180
           Vr_interp   [ir,*,*]       = K2  
           Vth_interp  [ir,*,*]       = L2
           Vph_interp  [ir,*,*]       = M2
        endif
     endfor
  endif

stop
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
  if keyword_set(rho_out) then begin
     openw,3,dir_out+'rho_'+file_out
     writeu,3,rho_awsom_interp
     close,3
  endif
  if keyword_set(qrad_out) then begin
     openw,3,dir_out+'qrad_'+file_out
     writeu,3,qrad_awsom_interp
     close,3
  endif
  if keyword_set(qheat_out) then begin
     openw,3,dir_out+'qheat_'+file_out
     writeu,3,qheat_awsom_interp
     close,3
  endif
  if keyword_set(qebyq_out) then begin
     openw,3,dir_out+'qebyq_'+file_out
     writeu,3,qebyq_awsom_interp
     close,3
  endif
  if keyword_set(ne_lasco_out) then begin
     openw,3,dir_out+'ne_lasco_'+file_out
     writeu,3,ne_lasco_awsom_interp
     close,3
  endif
 

  if keyword_set(B_out) then begin
     openw,4,dir_out+'Br_'+file_out
     writeu,4,Br_interp
     close,4
     openw,5,dir_out+'Bth_'+file_out
     writeu,5,Bth_interp
     close,5
     openw,6,dir_out+'Bph_'+file_out
     writeu,6,Bph_interp
     close,6
  endif

  if keyword_set (v_out) then begin
     openw,4,dir_out+'Vr_'+file_out
     writeu,4,Vr_interp
     close,4
     openw,5,dir_out+'Vth_'+file_out
     writeu,5,Vth_interp
     close,5
     openw,6,dir_out+'Vph_'+file_out
     writeu,6,Vph_interp
     close,6
  endif
  
stop
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
                         THETA: ptr_new( (thg * 1.d )*!dtor)             ,$
                         PHI: ptr_new( (phg * 1.d )*!dtor)               ,$
                         LAT: ptr_new( 90.d - thg)                       ,$
                         LON: ptr_new( phg)                              ,$
                         LONBOUNDS: dblarr(2)-1.                         ,$
                         STR: ptr_new() ,STTH: ptr_new() ,STPH: ptr_new(),$
                         PTR: ptr_new() ,PTTH: ptr_new() ,PTPH: ptr_new(),$
                         NSTEP: ptr_new() ,EXTRA_OBJECTS: ptr_new()       }     
stop     
     save,sph_data,FILENAME = 'sph_data_'+file_out+'.sav'
  endif



  return
end

