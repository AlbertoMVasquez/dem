;read_awsom,'3dAWSoM_DEMT_LASCO_1.85.dat',grilla_demt=1.26,te_out='Te_awsom_2208_1.85',ne_out='Ne_awsom_2208_1.85'
;read_awsom,'CR2082_r=1-6_1deg_AWSoM.dat',grilla_demt=1.26,te_out='Te_awsom_2208_1.85',ne_out='Ne_awsom_2208_1.85',/interpol
;read_awsom,'CR2082_grid1X1_1.85_AWSOM_LASCO_3d.dat',grilla_demt=1.26,te_out='Te_awsom_2082_1.85',ne_out='Ne_awsom_2082_1.85',/interpol
pro read_awsom,inputfile,grilla_demt=grilla_demt,te_out=te_out,ne_out=ne_out,interpol=interpol
  common grilla_chip,r_grilla,theta_grilla,phi_grilla,ne_awsom,te_awsom,rho_awsom,er_awsom,ti_awsom,ne_lasco

;grilla_demt es input  para recortar las matrices entre 2 radios
;grilla_demt se espera que sea un escalar si va de 0 hasta
;grilla_demt(0) o bien un vector de 2 dimensiones y en tal caso la
;matriz ira de rilla_demt(0) a grilla_demt(1) en la parte radial
;sin modificar th y ph (90 y 180)

;te_out y ne_out son strings que se usan para guardar las grillas de
;26x90x180 para comparar con DEMT.

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
  N1=26
  r_grilla       = fltarr(nr,nth,nph)
  theta_grilla   = fltarr(nr,nth,nph)
  phi_grilla     = fltarr(nr,nth,nph)
  
  r_grilla2      = fltarr(nr,nth,nph)
  theta_grilla2  = fltarr(nr,nth,nph)
  phi_grilla2    = fltarr(nr,nth,nph)
  
  ne_awsom       = fltarr(nr,nth,nph)
  te_awsom       = fltarr(nr,nth,nph)
  rho_awsom      = fltarr(nr,nth,nph)
  er_awsom       = fltarr(nr,nth,nph)
  tp_awsom       = fltarr(nr,nth,nph)
  ne_lasco_awsom = fltarr(nr,nth,nph)
  
; inputfile = '3dAWSoM_DEMT_LASCO_1.85.dat'
  openr,1,'/data1/work/MHD/'+inputfile
  
  for i=1L,N1-1 do begin
     readf,1,xx
  endfor
;  stop
  for iph=0,Nphi-1 do begin
     for ith=0,Ntheta-1 do begin
        for ir =0, Nr-1 do begin
;           readf,1, x,y,z,vx,vy,vz,tp,te,bx,by,bz,i01,i02,qrad,qheat,qebyq,qparbyq,n_e,ne_lasco
        readf,1, x,y,z,rho,vx,vy,vz,te,tp,bx,by,bz,i01,i02,qrad,qheat,qebyq,n_e,ne_lasco
           V=[x,y,z]
           cart_to_sphcoord,V,sphcoord
           r  = sqrt(x^2+y^2+z^2)
           th = acos(z/r)
           ph =          atan(y/x)
           if x gt 0 and y lt 0. then ph = 2*!dpi + atan(y/x)
           if x lt 0             then ph =   !dpi - atan(y/abs(x))
           r_grilla(ir,ith,iph)     = sphcoord[0]         ;r          
           theta_grilla(ir,ith,iph) = sphcoord[1]*180/!pi ;th *180./!pi         
           phi_grilla(ir,ith,iph)   = sphcoord[2]*180/!pi ;ph *180./!pi         
           r_grilla2(ir,ith,iph)     = r
           theta_grilla2(ir,ith,iph) = th *180./!pi
           phi_grilla2(ir,ith,iph)   = ph *180./!pi
           
           ne_awsom(ir,ith,iph)       = n_e
           te_awsom(ir,ith,iph)       = te
           rho_awsom(ir,ith,iph)      = rho
           er_awsom(ir,ith,iph)       = qrad
           tp_awsom(ir,ith,iph)       = tp
           ne_lasco_awsom(ir,ith,iph) = ne_lasco
        endfor
     endfor
  endfor
  close,1

  if keyword_set(grilla_demt) then begin
     if n_elements(grilla_demt) eq 1 then begin
        ok= where(r_grilla2[*,0,0] le grilla_demt)
        ne_awsom = ne_awsom[ok,*,*]
        te_awsom = te_awsom[ok,*,*]
     endif
     if n_elements(grilla_demt) eq 2 then begin
       ok= where(r_grilla2[*,0,0] ge grilla_demt(0) and r_grilla2[*,0,0] le grilla_demt(2))
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
     openw,2,ne_out
     writeu,2,ne_awsom_interp
     close,2
  endif
  if keyword_set(te_out) then begin
     openw,3,te_out
     writeu,3,te_awsom_interp
     close,3
  endif
 
stop
  return
end


pro cart_to_sphcoord,V,sphcoord
;transforma x,y,z en r,th,ph                                                                                                                                                                                                                                                   
x=V[0]*1d
y=V[1]*1d
z=V[2]*1d
r  = sqrt(x^2+y^2+z^2)
th = acos(z/r)
; devuelve siempre un valor de ph entre 0 y 2*!pi
; x = 3. & r = x/cos(30.*!dtor) & y = r*sin(30.*!dtor) 
; x = +abs(x) & y = +abs(y)
                           ph =          atan(y/x)
if x gt 0 and y lt 0. then ph = 2*!dpi + atan(y/x)
if x lt 0             then ph =   !dpi - atan(y/abs(x))
;                                        print,ph/!dtor
sphcoord=[r,th,ph]
return
end
