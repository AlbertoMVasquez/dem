;------------------------------------------------------------------
; Codes by A.M.Vásquez
; LATEST version: August 21, 2017.
;
; CALL SEQUENCE Examples:
;
; NEW WAY: 
; create_gridded_MHD,file='3d__var_1_n00000005.out__chip0_xxxiii.txt',N1=6L,N2=196613L,r_min=1.02,r_max=2.50
;
; OLD WAY:
; create_gridded_MHD,/l5
; create_gridded_MHD,/l6
; create_gridded_MHD,/rona
; create_gridded_MHD,/newrona
; create_gridded_MHD,/CR2056
; create_gridded_MHD,/CR2106new
; create_gridded_MHD,/CR2106nalai
;------------------------------------------------------------------

pro create_gridded_MHD,l5=l5,l6=l6,rona=rona,newrona=newrona,CR2056=CR2056,CR2106new=CR2106new,CR2106nalai=CR2106nalai,jimmy=jimmy,r_min=r_min,r_max=r_max,dr=dr,dth=dth,dph=dph,N1=N1,N2=N2,file=file,input_dir=input_dir,output_dir=output_dir

  common nodesdata,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph
  common griddata,Nr,Nth,Nph,rg,thg,phg,Brg,Bthg,Bphg

; Default grid cell size in each dimension:
  if not keyword_set(dr ) then dr  = 0.01 ; Rsun
  if not keyword_set(dth) then dth = 1.0  ; deg
  if not keyword_set(dph) then dph = 1.0  ; deg

; Default I/O Directories:
 if not keyword_set( input_dir) then  input_dir = '/data1/DATA/MHD_SWMF/'
 if not keyword_set(output_dir) then output_dir = input_dir
 print,'Your  INPUT directory is: '+ input_dir
 print,'Your OUTPUT directory is: '+output_dir 
 print,'Change them if those do not work for you...'

;---------BEGIN OLD-WAY----------------------------------------------------------
; OLD-WAY to choose which model do you want to use
  if not keyword_set(N1) then N1 = 24L ; default value unless changed for specific runs.
  if keyword_set(l5) then begin 
     file = 'CR2106_GNG_5-level.dat'
     N2   =  3308953L
  endif     
  if keyword_set(l6) then begin
     file = 'CR2106_GNG_6-level.dat'
     N2   =  23923611L
  endif
  if keyword_set(rona) then begin
     file = 'CR2063_3D_B.dat'
     N1   = 46L
     N2   = 11885847L            
  endif 
  if keyword_set(newrona) then begin
     file = 'CR2063_3d_B_new.out'
     N1   = 2L
     N2   = 6406467L
  endif 
  if keyword_set(CR2056) then begin 
     file = 'CR_2056_MHD.dat'
     N1   = 24L   
     N2   = 3235107L
  endif
  if keyword_set(CR2106new) then begin
     file='3d__var_4_n00070000.out'
     N1=6L
    ; N2=7975683L ;hasta que pueda transferir el archivo completo :P
     N2=10574117L
  endif
  if keyword_set(rona) then begin
     file = 'CR2063_3D_B.dat'
     N1   = 46L
     N2   = 11885847L            
  endif 
  if keyword_set(jimmy) then begin
     file='3d__var_1_n00000005.out__chip0_xxxiii.txt'
     N1=6L
     N2=196613L
  endif
;---------END OLD-WAY----------------------------------------------------------

; read the cartesian MHD model, and convert vectors r and B to spherical coordinates 
  read_MHD,N1,N2,input_dir+file
;  if     keyword_set(CR2106new) or keyword_set(CR2106nalai) then read_MHD,N1,N2,'/data1/DATA/MHD_SWMF/'+file,/full
;  if not keyword_set(CR2106new) then 
   
; Select the data between r_min and r_max
  if not keyword_set(r_min) then r_min = 1.00
  if not keyword_set(r_max) then r_max = 2.50
  cutoff_MHD_data,r_min,r_max

; Create suffix strings for output filename:
  if r_min lt  10.                   then rmin_string = strmid(string(r_min),6,4)
  if r_max lt  10.                   then rmax_string = strmid(string(r_max),6,4)
  if r_min ge  10. and r_min lt 100. then rmin_string = strmid(string(r_min),6,5)
  if r_max ge  10. and r_min lt 100. then rmax_string = strmid(string(r_max),6,5)
  if r_min ge 100.                   then rmin_string = strmid(string(r_min),6,6)
  if r_max ge 100.                   then rmax_string = strmid(string(r_max),6,6)

; Create the regular grid
  rmin=r_min
  rmax=r_max
  make_Grid,rmin,rmax,dr,dth,dph

; Interpolate the MHD model to the regular grid
 print,'Interpolating...'
 interpol_MHD
 print,'Done.'

; Create a few output variables
 nMHD = long( n_elements(r) )
 Nr   = long(Nr)
 Nth  = long(Nth)
 Nph  = long(Nph)

; Write OUTPUT file
 print,'Writing PUTPUT file...'
 filename=file+'_gridded_'+rmin_string+'-'+rmax_string+'.dat'
 openw,1,output_dir+filename
 writeu,1,nMHD
 writeu,1,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph
 writeu,1,Nr,Nth,Nph
 writeu,1,rg,thg,phg,Brg,Bthg,Bphg
 close,1
 print,'Done.'
 print,'Your output is: '+output_dir+filename

return
end

pro interpol_MHD
common nodesdata,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph
common griddata,Nr,Nth,Nph,rg,thg,phg,Brg,Bthg,Bphg

; Por alguna razón hice este método previo, no recuerdo bien, pero no lo uso más.
; Habría que leer la documentación de la rutina GRID3 para entender,
; pero al parecer la que uso ahora (mucho más breve) terminó haciendo el
; trabajo bien.
goto,skip 
   rmin  = min(r)
   rmax  = max(r)
   thmin = min(th)
   thmax = max(th)
   phmin = min(ph)
   phmax = max(ph)

   r_sc = (r  - rmin)/( rmax- rmin)
  th_sc = (th -thmin)/(thmax-thmin)
  ph_sc = (ph -phmin)/(phmax-phmin)
  rg_sc = ( rg- rmin)/( rmax- rmin)
 thg_sc = (thg-thmin)/(thmax-thmin)
 phg_sc = (phg-phmin)/(phmax-phmin)

   r_sc = (r)/(rmax)
  th_sc = (th)/(thmax)
  ph_sc = (ph)/(phmax)
  rg_sc = (rg)/(rmax)
 thg_sc = (thg)/(thmax)
 phg_sc = (phg)/(phmax)

    Brg = GRID3(r_sc,th_sc,ph_sc,Br ,rg_sc,thg_sc,phg_sc,/grid)
   Bthg = GRID3(r_sc,th_sc,ph_sc,Bth,rg_sc,thg_sc,phg_sc,/grid)
   Bphg = GRID3(r_sc,th_sc,ph_sc,Bph,rg_sc,thg_sc,phg_sc,/grid)
skip:

    Brg = GRID3(r,th,ph,Br ,rg,thg,phg,/grid)
   Bthg = GRID3(r,th,ph,Bth,rg,thg,phg,/grid)
   Bphg = GRID3(r,th,ph,Bph,rg,thg,phg,/grid)

return
end

pro make_Grid,rmin,rmax,dr,dth,dph
common nodesdata,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph
common griddata,Nr,Nth,Nph,rg,thg,phg,Brg,Bthg,Bphg

; Convert the angular size of the grid cell to radians:
  dth = dth*!dtor  ; radians                    
  dph = dph*!dtor  ; radians  

; Create cell-centered radial Grid:
  Nr = (rmax-rmin)/dr
  rg = rmin+dr/2.+dr*findgen(Nr)

; Create cell-centered Theta Grid:
  thmin = 0.
  thmax = !pi
  Nth   = (thmax-thmin)/(dth)
  thg   = thmin+dth/2.+dth*findgen(Nth)
; Make theta DESCENDING, as required by Marc's routines:
  thg = reverse(thg)

; Make a theta grid uniform in cos(theta) with theta descending 
  costhg = -1. + findgen(Nth)*2/Nth + (2./Nth)/2 
     thg = acos(costhg)

; Create cell-centered Phi Grid:
  phmin = 0.
  phmax = 2.*!pi
  Nph = (phmax-phmin)/(dph)
  phg = phmin+dph/2.+dph*findgen(Nph)

return
end

pro read_MHD,N1,N2,file,full=full
common nodesdata,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph
 xx=''
 xp=0.
 yp=0.
 zp=0.
Bxp=0.
Byp=0.
Bzp=0.
if keyword_set(full) then begin
   rho=0.
   ux=0.
   uy=0.
   uz=0.
   ti=0.
   te=0.
   I1=0.
   I2=0.
   p=0.
   pe=0.
   ppar=0.
   ehot=0.
   qrad=0.
   qheat=0.
   qebyq=0.
   g=0.
   c=0.
   th=0.
   p1=0.
   p2=0.
   p3=0.
   Nx=0.
   Ny=0.
   Nz=0.
   R=0.
endif

  x=fltarr(N2-N1+1)
  y=fltarr(N2-N1+1)
  z=fltarr(N2-N1+1)
 Bx=fltarr(N2-N1+1)
 By=fltarr(N2-N1+1)
 Bz=fltarr(N2-N1+1)
  r=fltarr(N2-N1+1)
 th=fltarr(N2-N1+1)
 ph=fltarr(N2-N1+1)
 Br=fltarr(N2-N1+1)
Bth=fltarr(N2-N1+1)
Bph=fltarr(N2-N1+1)

openr,1,file
for i=1L,N1-1 do begin
   readf,1,xx 
   print,xx
endfor
for i=N1,N2   do begin
if not keyword_set(full) then readf,1,xp,yp,zp,Bxp,Byp,Bzp
if     keyword_set(full) then begin
   readf,1,xp,yp,zp,rho,ux,uy,uz,ti,te,Bxp,Byp,Bzp;,I1,I2,p,pe,ppar,ehot,qrad,qheat,qebyq,qparbyq,g,c,th,p1,p2,p3,Nx,Ny,Nz,R
endif
  ;if i ge N1 and i le N1+5 then print,xp,yp,zp,rho,ux,uy,uz,ti,te,Bxp,Byp,Bzp
  x(i-N1)= xp
  y(i-N1)= yp
  z(i-N1)= zp
 Bx(i-N1)=Bxp
 By(i-N1)=Byp
 Bz(i-N1)=Bzp
 ;
 cart_to_sphcoord,[xp,yp,zp],sphcoord
  r(i-N1)=sphcoord[0]
 th(i-N1)=sphcoord[1]
 ph(i-N1)=sphcoord[2]
 ;
 transform_B_cart_to_sph,sphcoord[1],sphcoord[2],[Bxp,Byp,Bzp],B_sph
  Br(i-N1)=B_sph[0]
 Bth(i-N1)=B_sph[1]
 Bph(i-N1)=B_sph[2]
endfor
close,1

return
end

pro transform_B_cart_to_sph,th,ph,B_cart,B_sph
sinth=sin(th)
costh=cos(th)
sinph=sin(ph)
cosph=cos(ph)
er =[sinth*cosph,sinth*sinph, costh]
eth=[costh*cosph,costh*sinph,-sinth]
eph=[-sinph     ,cosph      , 0    ]
Br =total(B_cart*er ) 
Bth=total(B_cart*eth) 
Bph=total(B_cart*eph) 
B_sph=[Br,Bth,Bph]
return
end

pro cart_to_sphcoord,V,sphcoord
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
;print,ph/!dtor
sphcoord=[r,th,ph]
return
end

pro cutoff_MHD_data,r_min,r_max
common nodesdata,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph
 p    = where( r ge r_min AND r le r_max AND ph ge 0. AND ph le 2.*!pi AND th ge 0. AND th le !pi)
 x   = x  (p)
 y   = y  (p)
 z   = z  (p)
 r   = r  (p)
 th  = th (p)
 ph  = ph (p)
 Bx  = Bx (p)
 By  = By (p)
 Bz  = Bz (p)
 Br  = Br (p)
 Bth = Bth(p)
 Bph = Bph(p)
return
end

pro analysis,r_min,r_max,delta_angle
common nodesdata,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph

 p    = where( r ge r_min AND r le r_max AND ph ge 0. AND ph le 2.*!pi AND th ge 0. AND th le !pi)
 vlat = 90.-th(p)/!dtor
 vlon =     ph(p)/!dtor

 bin       = delta_angle
 histo_lat = histogram(vlat,binsize=bin,locations=latitude)
 histo_lon = histogram(vlon,binsize=bin,locations=longitude)

 Nrads = (r_max-r_min)/.01

 ps1,'~/Downloads/histograms.eps',0
 !p.charsize=1.0
 !p.multi=[0,1,2]
 plot,latitude ,histo_lat,psym=10,xstyle=1,title='#  Data points per Latitudinal Bin of'+string(bin)+' deg'
 xyouts,[0.5],[0.9],['Desired number:'+string(Nrads*360.*bin)],/normal
 plot,longitude,histo_lon,psym=10,xstyle=1,title='#  Data points per Longitudinal Bin of'+string(bin)+' deg'
 xyouts,[0.5],[0.4],['Desired number:'+string(Nrads*180.*bin)],/normal
 ps2
 !p.multi=0
print,Nrads
stop
return
end


;-----------------------------------------------------------------------------------------------------------
; no tirar por favor:

pro leer

 openr,1,'test_gridded_MHD.dat'
 nMHD =0L
 readu,1,  nMHD
  x=fltarr(nMHD)
  y=fltarr(nMHD)
  z=fltarr(nMHD)
 Bx=fltarr(nMHD)
 By=fltarr(nMHD)
 Bz=fltarr(nMHD)
  r=fltarr(nMHD)
 th=fltarr(nMHD)
 ph=fltarr(nMHD)
 Br=fltarr(nMHD)
Bth=fltarr(nMHD)
Bph=fltarr(nMHD)
 readu,1,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph

 Nr   = 0L
 Nth  = 0L
 Nph  = 0L
 readu,1,Nr,Nth,Nph
    rg = fltarr(Nr ) 
   thg = fltarr(Nth)
   phg = fltarr(Nph)
   Brg = fltarr(Nr,Nth,Nph)
  Bthg = fltarr(Nr,Nth,Nph)
  Bphg = fltarr(Nr,Nth,Nph)
 readu,1,rg,thg,phg,Brg,Bthg,Bphg
 close,1

GOTO,SKIPALL
eps = 1.e-4
for ir =0,Nr -1 do begin
print,'Doing'+string(ir+1)+'/'+string(Nr)
    fr = abs( 1. -  rg_v /  rg(ir ) )
for ith=0,Nth-1 do begin
   fth = abs( 1. - thg_v / thg(ith) )

goto,skip
for iph=0,Nph-1 do begin
    fph = abs( 1. - phg_v / phg(iph) )
 index = where( fr le eps AND fth le eps AND fph le eps )
 index = where( fr le eps AND fth le eps )
 if index(0) eq -1 then begin
     print,ir,ith,iph
     STOP
 endif
  Brg(ir,ith,iph) =  Brg_v(index)
 Bthg(ir,ith,iph) = Bthg_v(index)
 Bphg(ir,ith,iph) = Bphg_v(index)
endfor
skip:

 index = where( fr le eps AND fth le eps )
 vph   = phg_v(index) & order=sort(vph)
  Brg(ir,ith,*) =  Brg_v(index(order))
 Bthg(ir,ith,*) = Bthg_v(index(order))
 Bphg(ir,ith,*) = Bphg_v(index(order))
endfor
endfor
SKIPALL:

; Re-order the dimensions of Br, Bth, and Bph, so that for each array: 
; the 3rd dimension (index=2) becomes 1st,
; the 2nd dimension (index=1) stays   2nd,
; the 1st dimension (index=0) becomes 3rd. 
  Br_new = TRANSPOSE(  Brg , [2, 1, 0] ) 
 Bth_new = TRANSPOSE( Bthg , [2, 1, 0] ) 
 Bph_new = TRANSPOSE( Bphg , [2, 1, 0] ) 

sph_data = {        BR: ptr_new( Br_new)                            ,$
                   BTH: ptr_new(Bth_new)                            ,$
                   BPH: ptr_new(Bph_new)                            ,$
               BDERIVS: ptr_new()                                   ,$
                    NR: long(Nr)                                    ,$
                  NLAT: long(Nth)                                   ,$
                  NLON: long(Nph)                                   ,$
                   RIX: ptr_new(  rg * 1.d )                        ,$
                 THETA: ptr_new( thg * 1.d )                        ,$
                   PHI: ptr_new( phg * 1.d )                        ,$
                   LAT: ptr_new( 90.d0-thg/!dtor )                  ,$
                   LON: ptr_new(  1.d0*phg/!dtor )                  ,$
             LONBOUNDS: dblarr(2)-1.                                ,$
                   STR: ptr_new() ,STTH: ptr_new() ,STPH: ptr_new() ,$
                   PTR: ptr_new() ,PTTH: ptr_new() ,PTPH: ptr_new() ,$
                 NSTEP: ptr_new() ,EXTRA_OBJECTS: ptr_new()          }

; openw,1,'structure.dat'
; writeu,1,sph_data
; close,1

stop
return
end


pro show_histograms,titulo=titulo
common nodesdata,x,y,z,r,th,ph,Bx,By,Bz,Br,Bth,Bph
common  griddata,Nr,Nth,Nph,rg,thg,phg,Brg,Bthg,Bphg
 !p.charsize=2
 !p.charthick=1
 window,1,xs=600,ys=400

 mini=min(rg)
 maxi=max(rg)
;bins=(maxi-mini)/25.
 bins=0.02

 p=where(r ge mini and r le maxi)
 histo_MHD =  histogram(r(p),binsize=bins,locations=value_MHD,min=mini,max=maxi) ;/ float(n_elements(r(p)))

rg_v=rg

 p=where(rg_v ge mini and rg_v le maxi)
 histo_GRID =  histogram(rg_v(p),binsize=bins,locations=value_GRID,min=mini,max=maxi) ;/ float(n_elements(rg_v(p)))
 histo_GRID = histo_GRID*Nth*Nph

 dr=(maxi-mini)/Nr
 Nradbin = bins/((maxi-mini+dr)/float(Nr))
 ;histo_GRID = histo_GRID * 0. +  Nradbin*Nth*Nph ; Histogram of a uniform grid. The formula above suffers of the "bin edge effect".

 maxy = max([histo_MHD,histo_GRID])
 maxy = max([median(histo_MHD),median(histo_GRID)])*2.

 plot , value_MHD , histo_MHD , xr=[mini,maxi],yr=[0,maxy],title=titulo,ytitle='# Data points',xtitle='r [Rsun]',psym=10,th=2,xstyle=1,/nodata
 loadct,39
 oplot ,value_MHD  , histo_MHD  ,color = 200 , psym=10 , th=2
 oplot ,value_GRID , histo_GRID ,color = 100 , psym=10 , th=2
 xyouts , [1,1]*0.5,.9-[0.1,0.2],['Desired Data Points','MHD Model Data Points'], color=[100,200],charthick=2,/normal
 loadct,0
 record_gif,'~/Downloads/',titulo+'.gif'
STOP
return
end

