pro loop_error,euvi=euvi,eit=eit,rmin=rmin,rmax=rmax,rloopmin=rloopmin,histos=histos

common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
common quemierda,alturas
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  
common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s,tm0_2,tm0_3,tm0_4,gradt_2,gradt_3,gradt_4,r2T_2,f_t,f_ne
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon 
common angle_box,rad_ini,rad_fin,lat_ini,lat_fin,lon_ini,lon_fin


rstart  = 1.035+0.02*findgen(10)
alturas = n_elements(rstart)
!except=2

if keyword_set (euvi) then begin
 instrumento='euvi'
 filesT= ['traceLDEM_CR2081_euviA-NODECON_reg0.75_safety0.5_errorbox_base_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_HLH_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_HHL_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_HLL_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_LLH_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_LHL_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_LHH_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_HLH_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_HHL_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_HLL_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_LLH_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_LHL_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR2081_euviA_reg0.75_safety0.5_errorbox_LHH_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat']
endif

goto,mitad_error_eit ;es decir el mismo error de euvi
if keyword_set (eit) then begin
 instrumento='eit'
 filesT= ['traceLDEM_CR1915_reg0.75_safety0.5_errorbox_base_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HHL_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LHL_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LHH_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HHL_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HLL_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LHL_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LHH_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HLH_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HLH_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LLH_oreg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LLH_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
          'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HLL_ureg_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat']
endif
mitad_error_eit:

if keyword_set (eit) then begin
 instrumento='eit'
filesT= ['traceLDEM_CR1915_reg0.75_safety0.5_errorbox_base_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HHL_ureg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HLH_ureg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HLL_ureg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LHH_ureg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LHL_ureg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LLH_ureg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HHL_oreg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HLH_oreg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_HLL_oreg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LHH_oreg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LHL_oreg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat',$
         'traceLDEM_CR1915_reg0.75_safety0.5_errorbox_LLH_oreg_erroreuvi_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat']

endif

if keyword_set (eit)  then begin
    lonmax=180.
    lowlatmax =20.
 endif

if keyword_set (euvi) then begin
    lonmax=360.
    lowlatmax=30.
 endif


;defino 13 structs con TODO lo que voy a seguir usando.

for i=0,n_elements(filesT)-1 do begin
read_trace_sampled,filesT(i),alturas
if i eq 0  then base=create_struct(name='base','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v,'wt_v',wt_v,$
                                   'enrad_v',enrad_v,'enlat_v',enlat_v,'enlon_v',enlon_v)
if i eq 1  then HLHU=create_struct(name='HLHU','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 2  then HHLU=create_struct(name='HHLU','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 3  then HLLU=create_struct(name='HLLU','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 4  then LLHU=create_struct(name='LLHU','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 5  then LHLU=create_struct(name='LHLU','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 6  then LHHU=create_struct(name='LHHU','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 7  then HLHO=create_struct(name='HLHO','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 8  then HHLO=create_struct(name='HHLO','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 9  then HLLO=create_struct(name='HLLO','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 10 then LLHO=create_struct(name='LLHO','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 11 then LHLO=create_struct(name='LHLO','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
if i eq 12 then LHHO=create_struct(name='LHHO','ne_v',ne_v,'tm_v',tm_v,'npts_v',npts_v,'opcls',opcls,'rad_v',rad_v,'lat_v',lat_v,'lon_v',lon_v,'midcell_v',midcell_v,'scorer_v',scorer_v)
;Podria definirlos ademas con vectores footlon y footlat que luego del
;statloop los rellenos con el valores correspondiente.
print, i
endfor
;OBS: quiero hacer un vector de strings donde tenga los sufijos de los
;nombres de los structs, de esta forma con un for puedo barrer para
;cada loop los 13 trace y hacer las cuentas pertinentes, agregando
;solamente un sufijo (variable) a cada reform, etc. --->pendiente
;perturbaciones=['hlhu','hhlu','hllu','llhu','lhlu','lhhu','hlho','hhlo','hllo','llhu','lhlo','lhho']

Nloop = n_elements(loopL) ;--> sale del common opclstatus 
if not keyword_set(rmin) then rmin = 1.03
if not keyword_set(rmax) then rmax = 1.20
if not keyword_set(rloopmin) then rloopmin = 1.07
rminloop=rloopmin

index0 = where(opcls eq 0.)
index1 = where(opcls eq 1.)
index2 = where(opcls eq 2.)

Nloop0 = n_elements(index0) & if index0(0) eq -1 then Nloop0=0
Nloop1 = n_elements(index1) & if index1(0) eq -1 then Nloop1=0
Nloop2 = n_elements(index2) & if index2(0) eq -1 then Nloop2=0

if Nloop0 + Nloop1 + Nloop2 ne Nloop then stop

Nlegs = Nloop0 + 2*Nloop1 + 2*Nloop2

stdev_loop_ne = fltarr(Nlegs)-555.
stdev_loop_t  = fltarr(Nlegs)-555.
opclstat   = fltarr(Nlegs)-555.
footrad = fltarr(Nlegs)-555.
footlat = fltarr(Nlegs)-555.
footlon = fltarr(Nlegs)-555.
rad_ini = fltarr(Nlegs)-555.
lat_ini = fltarr(Nlegs)-555.
lon_ini = fltarr(Nlegs)-555.
rad_fin = fltarr(Nlegs)-555.
lat_fin = fltarr(Nlegs)-555.
lon_fin = fltarr(Nlegs)-555.

 ileg = 0L
for il=0L,Nloop-1 do begin

if opcls(il) eq 0. then begin
   Ne_l     = reform (base.Ne_v(0:base.Npts_v(il)-1,il))
   Tm_l     = reform (base.Tm_v(0:base.Npts_v(il)-1,il))
   lat_l    = reform (base.lat_v(0:base.Npts_v(il)-1,il))
   lon_l    = reform (base.lon_v(0:base.Npts_v(il)-1,il))
   scoreR_l = reform (base.scoreR_v(0:base.Npts_v(il)-1,il))
   rad_l    = reform (base.rad_v(0:base.Npts_v(il)-1,il))
   WT_l     = reform (base.WT_v(0:Npts_v(il)-1,il))
;defino footlat, footlon, footrad
    rad_fin(ileg) = enrad_v(il)
    lat_fin(ileg) = enlat_v(il)
    lon_fin(ileg) = enlon_v(il)

    rad_ini(ileg) = rad_l(0)
    lat_ini(ileg) = lat_l(0)
    lon_ini(ileg) = lon_l(0)

    rad_fin(ileg) = enrad_v(il)
    lat_fin(ileg) = enlat_v(il)
    lon_fin(ileg) = enlon_v(il)
    footrad(ileg) = rad_ini(ileg)
    footlat(ileg) = lat_ini(ileg)
    footlon(ileg) = lon_ini(ileg)



p = where ( rad_l ge rmin and rad_l le rmax and Ne_l ne -999. and scoreR_l lt 0.10)
; esto lo necesito, xq no quiero hacer estadistica con los puntos que
; NO voy a utilizar!

if p(0) eq -1 then goto,skipnextloop_open
Ndata=5
if n_elements(p) lt Ndata then goto,skipfitloop_open

;debo definir los base, en particular rad_l
     Ne_l_base  =  Ne_l (p)
     Tm_l_base  =  Tm_l (p)
     rad_l_base = rad_l (p)
     lat_l_base = lat_l (p)
     lon_l_base = lon_l (p)

;rad_ok = fltarr (n_elements(rad_l_base))
;for i=0,n_elements(rad_l_base)-1 do begin
;rad_ok_ind = where(rad_l eq rad_l_base(i)) ;--> esto me da el indice en el vector geometrico del loop donde quiero evaluar.
;rad_ok(i)=rad_ok_ind
;endfor;chequear si p y rad_ok son iguales
;IF not ARRAY_EQUAL(p, rad_ok) THEN stop
;Ahora sí defino las 12 mierdas restantes
;Aca es donde quiero meter la recorrida en el vector de strings
;--------> Definir un struct de pointer, y cargar los valores dsp.

ptrs_ne = $
     [ Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap) ]
*ptrs_ne[0] =(base.ne_v(p,il))
*ptrs_ne[1] =(hlhu.ne_v(p,il))
*ptrs_ne[2] =(hhlu.ne_v(p,il))
*ptrs_ne[3] =(hllu.ne_v(p,il))
*ptrs_ne[4] =(llhu.ne_v(p,il))
*ptrs_ne[5] =(lhlu.ne_v(p,il))
*ptrs_ne[6] =(lhhu.ne_v(p,il))
*ptrs_ne[7] =(hlho.ne_v(p,il))
*ptrs_ne[8] =(hhlo.ne_v(p,il))
*ptrs_ne[9] =(hllo.ne_v(p,il))
*ptrs_ne[10]=(llho.ne_v(p,il))
*ptrs_ne[11]=(lhlo.ne_v(p,il))
*ptrs_ne[12]=(lhho.ne_v(p,il))

ptrs_t  = $
     [ Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap) ]
*ptrs_t[0] =(base.tm_v(p,il))
*ptrs_t[1] =(hlhu.tm_v(p,il))
*ptrs_t[2] =(hhlu.tm_v(p,il))
*ptrs_t[3] =(hllu.tm_v(p,il))
*ptrs_t[4] =(llhu.tm_v(p,il))
*ptrs_t[5] =(lhlu.tm_v(p,il))
*ptrs_t[6] =(lhhu.tm_v(p,il))
*ptrs_t[7] =(hlho.tm_v(p,il))
*ptrs_t[8] =(hhlo.tm_v(p,il))
*ptrs_t[9] =(hllo.tm_v(p,il))
*ptrs_t[10]=(llho.tm_v(p,il))
*ptrs_t[11]=(lhlo.tm_v(p,il))
*ptrs_t[12]=(lhho.tm_v(p,il))

goto,con_esto_acorto
;Tambien podria haber usado lo siguiente
struct = { ptr_ne:Ptr_New(/allocate_heap), ptr_t:Ptr_New(/allocate_heap) };define un struct de 2 pointers
array = Replicate(struct, 13) ;define un array de struct donde cada uno tiene 2 pointers
;entonces defino luego
*array[1].ptr_ne=[[1,2,3],[4,5,6],[7,8,9]]  
con_esto_acorto:

;estos ptrs tienen todos las mismas dimensiones
cant_pts=n_elements(p)
;defino una matriz donde en cada columna tiene los 13 valores de 
;1 punto. cant_pts es columnas
M_ne = MAKE_ARRAY(cant_pts, 13, /float, VALUE = -623)
M_t  = MAKE_ARRAY(cant_pts, 13, /float, VALUE = -623)
stdev_interno_ne= fltarr(cant_pts)-777.
stdev_interno_t = fltarr(cant_pts)-777.

for i=0,cant_pts-1 do begin;recorre puntos

for j=0,12 do begin;recorre los trace
M_ne[i,j]= (*ptrs_ne[j])(i)
M_t [i,j]= (*ptrs_t [j])(i)
endfor
vec_M_ne = M_ne(i,*)
vec_M_t  = M_t (i,*)
indice_M_ok_ne = where(vec_M_ne ne -666 and vec_M_ne ne -999)
indice_M_ok_t  = where(vec_M_t  ne -666 and vec_M_t  ne -999)
;if n_elements(indice_M_ok_ne) ne n_elements(vec_M_ne) then print, 'ver matriz' & stop
new_M_ne = vec_M_ne(indice_M_ok_ne) 
new_M_t  = vec_M_t (indice_M_ok_t ) 
stdev_interno_ne(i)=stdev(new_M_ne)   ;--> debo NO considerar los valores -999,-666, y no deberia haber ningun -623, chequear esto.
stdev_interno_t (i)=stdev(new_M_t )
;OBS:Si quiero hacer consideraciones para ver que considerar y que
;no, este es el lugar.
endfor
;print, stdev_interno_ne,'--> mean Ne',mean(stdev_interno_ne)
;print, stdev_interno_t ,'--> mean t ',mean(stdev_interno_t )
;print, il
stdev_loop_ne(ileg)=mean(stdev_interno_ne) ;--> esto debe ser definido mucho antes y acá queda guardado.
stdev_loop_t (ileg)=mean(stdev_interno_t )



  skipfitloop_open:
       opclstat(ileg) = opcls(il)  
  skipnextloop_open:
   ;indexloop(ileg) = il
              ileg = ileg+1

     endif else begin ;--> finaliza loop abierto

           
; Analysis closed loops:                                                                                                                                  
  if max(base.rad_v(0:base.Npts_v(il)-1,il)) lt rminloop then goto,skipnextloop ;si es menor a 1.07 entonces no lo considera
  
;<---------------------------------------                                                                                                                          
  if base.opcls(il) eq 2 then begin
      ifirs_1 = 0
      ilast_1 = base.midcell_v(il)
      ifirs_2 = base.midcell_v(il)
      ilast_2 = base.Npts_v(il)-1
   endif
  if base.opcls(il) eq 1 then begin
      ifirs_1 = 0
      ilast_1 = base.midcell_v(il)-1
      while base.Ne_v(ilast_1,il) eq -666. do ilast_1=ilast_1-1
      ifirs_2 = base.midcell_v(il)
      ilast_2 = base.Npts_v(il)-1
  endif

  Ne_l1     = reform ( base.Ne_v(ifirs_1:ilast_1,il))
  Ne_l2     = reform ( base.Ne_v(ifirs_2:ilast_2,il))
  Tm_l1     = reform ( base.Tm_v(ifirs_1:ilast_1,il))
  Tm_l2     = reform ( base.Tm_v(ifirs_2:ilast_2,il))
  scoreR_l1 = reform ( base.scoreR_v(ifirs_1:ilast_1,il))
  scoreR_l2 = reform ( base.scoreR_v(ifirs_2:ilast_2,il))
  rad_l1    = reform ( base.rad_v(ifirs_1:ilast_1,il))
  rad_l2    = reform ( base.rad_v(ifirs_2:ilast_2,il))
  lat_l1    = reform ( base.lat_v(ifirs_1:ilast_1,il))
  lat_l2    = reform ( base.lat_v(ifirs_2:ilast_2,il))
  lon_l1    = reform ( base.lon_v(ifirs_1:ilast_1,il))
  lon_l2    = reform ( base.lon_v(ifirs_2:ilast_2,il))
  WT_l1     = reform ( base.WT_v(ifirs_1:ilast_1,il))
  WT_l2     = reform ( base.WT_v(ifirs_2:ilast_2,il))

  p1 = where ( rad_l1 ge rmin and rad_l1 le rmax and Ne_l1 ne -999. and scoreR_l1 lt 0.1 and WT_l1 ge WTc*1.e6) ;Acá si, xq en open relajamos esta cuestion.
  p2 = where ( rad_l2 ge rmin and rad_l2 le rmax and Ne_l2 ne -999. and scoreR_l2 lt 0.1 and WT_l2 ge WTc*1.e6)

  if  p1(0) eq -1 or p2(0) eq -1 then goto,skipnextloop   ;Charlar sobre expandir esto aca y en statloop, para que si una sola pierna es buena, entonces usarla!

  if n_elements(p1) lt Ndata  or n_elements(p2) lt Ndata then goto,skipfitloop

   Ne_l1_ok =  Ne_l1 (p1)
   Ne_l2_ok =  Ne_l2 (p2)
   Tm_l1_ok =  Tm_l1 (p1)
   Tm_l2_ok =  Tm_l2 (p2)
   ;WT_l1_ok =  WT_l1 (p1)
   ;WT_l2_ok =  WT_l2 (p2)
  rad_l1_ok = rad_l1 (p1)
  rad_l2_ok = rad_l2 (p2)

;rad_ok_l1 = fltarr (n_elements(rad_l1_ok))
;for i=0,n_elements(rad_l1_ok)-1 do begin
;rad_ok_l1_ind = where(rad_l1 eq rad_l1_ok(i)) 
;rad_ok_l1(i)=rad_ok_l1_ind ;vector de indices
;endfor
;IF not ARRAY_EQUAL(p1, rad_ok_l1) THEN stop

;rad_ok_l2 = fltarr (n_elements(rad_l2_ok))
;for i=0,n_elements(rad_l2_ok)-1 do begin
;rad_ok_l2_ind = where(rad_l2 eq rad_l2_ok(i))
;rad_ok_l2(i)=rad_ok_l2_ind
;endfor
;IF not ARRAY_EQUAL(p2, rad_ok_l2) THEN stop
;rad_ok_l2 = rad_ok_l2 + midcell_v(il);xq quiero evaluar esto en las matrices.
p2 = p2 + midcell_v(il)

ptrsl1_ne = $
     [ Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap) ]
*ptrsl1_ne[0] =(base.ne_v(p1,il))
*ptrsl1_ne[1] =(hlhu.ne_v(p1,il))
*ptrsl1_ne[2] =(hhlu.ne_v(p1,il))
*ptrsl1_ne[3] =(hllu.ne_v(p1,il))
*ptrsl1_ne[4] =(llhu.ne_v(p1,il))
*ptrsl1_ne[5] =(lhlu.ne_v(p1,il))
*ptrsl1_ne[6] =(lhhu.ne_v(p1,il))
*ptrsl1_ne[7] =(hlho.ne_v(p1,il))
*ptrsl1_ne[8] =(hhlo.ne_v(p1,il))
*ptrsl1_ne[9] =(hllo.ne_v(p1,il))
*ptrsl1_ne[10]=(llho.ne_v(p1,il))
*ptrsl1_ne[11]=(lhlo.ne_v(p1,il))
*ptrsl1_ne[12]=(lhho.ne_v(p1,il))

ptrsl1_t = $
     [ Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap) ]
*ptrsl1_t[0] =(base.tm_v(p1,il))
*ptrsl1_t[1] =(hlhu.tm_v(p1,il))
*ptrsl1_t[2] =(hhlu.tm_v(p1,il))
*ptrsl1_t[3] =(hllu.tm_v(p1,il))
*ptrsl1_t[4] =(llhu.tm_v(p1,il))
*ptrsl1_t[5] =(lhlu.tm_v(p1,il))
*ptrsl1_t[6] =(lhhu.tm_v(p1,il))
*ptrsl1_t[7] =(hlho.tm_v(p1,il))
*ptrsl1_t[8] =(hhlo.tm_v(p1,il))
*ptrsl1_t[9] =(hllo.tm_v(p1,il))
*ptrsl1_t[10]=(llho.tm_v(p1,il))
*ptrsl1_t[11]=(lhlo.tm_v(p1,il))
*ptrsl1_t[12]=(lhho.tm_v(p1,il))
;estos ptrs tienen todos las mismas dimensiones                                          
cant_pts=n_elements(rad_l1_ok) ;no confundir con rad_ok_l1 :)
;defino una matriz donde en cada columna tiene los 13 valores de                                           
;1 punto. cant_pts es columnas                                                                   
M_l1_ne = MAKE_ARRAY(cant_pts, 13, /float, VALUE = -623)
M_l1_t  = MAKE_ARRAY(cant_pts, 13, /float, VALUE = -623)
stdev_interno_l1_ne= fltarr(cant_pts)
stdev_interno_l1_t = fltarr(cant_pts)

for i=0,cant_pts-1 do begin

for j=0,12 do begin
M_l1_ne[i,j]= (*ptrsl1_ne[j])(i)
M_l1_t [i,j]= (*ptrsl1_t [j])(i)

endfor
vec_M_ne = M_l1_ne(i,*)
vec_M_t  = M_l1_t (i,*)
indice_M_l1_ok = where(vec_M_ne ne -666 and vec_M_ne ne -999);el indice deberia ser el mismo para te y ne no??
new_M_ne = vec_M_ne(indice_M_l1_ok)
new_M_t  = vec_M_t (indice_M_l1_ok)
stdev_interno_l1_ne(i)=stdev(new_M_ne) 
stdev_interno_l1_t (i)=stdev(new_M_t ) 
endfor
;print, stdev_interno_l1_ne,'--> mean Ne',mean(stdev_interno_l1_ne)
;print, stdev_interno_l1_t ,'--> mean T ',mean(stdev_interno_l1_t )
;print, il
stdev_loop_ne(ileg)=mean(stdev_interno_l1_ne) 
stdev_loop_t (ileg)=mean(stdev_interno_l1_t ) 

ptrsl2_ne = $
     [ Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap) ]
*ptrsl2_ne[0] =(base.ne_v(p2,il))
*ptrsl2_ne[1] =(hlhu.ne_v(p2,il))
*ptrsl2_ne[2] =(hhlu.ne_v(p2,il))
*ptrsl2_ne[3] =(hllu.ne_v(p2,il))
*ptrsl2_ne[4] =(llhu.ne_v(p2,il))
*ptrsl2_ne[5] =(lhlu.ne_v(p2,il))
*ptrsl2_ne[6] =(lhhu.ne_v(p2,il))
*ptrsl2_ne[7] =(hlho.ne_v(p2,il))
*ptrsl2_ne[8] =(hhlo.ne_v(p2,il))
*ptrsl2_ne[9] =(hllo.ne_v(p2,il))
*ptrsl2_ne[10]=(llho.ne_v(p2,il))
*ptrsl2_ne[11]=(lhlo.ne_v(p2,il))
*ptrsl2_ne[12]=(lhho.ne_v(p2,il))

ptrsl2_t  = $
     [ Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap), $
       Ptr_New(/Allocate_Heap) ]
*ptrsl2_t[0] =(base.tm_v(p2,il))
*ptrsl2_t[1] =(hlhu.tm_v(p2,il))
*ptrsl2_t[2] =(hhlu.tm_v(p2,il))
*ptrsl2_t[3] =(hllu.tm_v(p2,il))
*ptrsl2_t[4] =(llhu.tm_v(p2,il))
*ptrsl2_t[5] =(lhlu.tm_v(p2,il))
*ptrsl2_t[6] =(lhhu.tm_v(p2,il))
*ptrsl2_t[7] =(hlho.tm_v(p2,il))
*ptrsl2_t[8] =(hhlo.tm_v(p2,il))
*ptrsl2_t[9] =(hllo.tm_v(p2,il))
*ptrsl2_t[10]=(llho.tm_v(p2,il))
*ptrsl2_t[11]=(lhlo.tm_v(p2,il))
*ptrsl2_t[12]=(lhho.tm_v(p2,il))

cant_pts=n_elements(rad_l2_ok) ;no confundir con rad_ok_l2 :)            
M_l2_ne = MAKE_ARRAY(cant_pts, 13, /float, VALUE = -623)
M_l2_t  = MAKE_ARRAY(cant_pts, 13, /float, VALUE = -623)
stdev_interno_l2_ne= fltarr(cant_pts)
stdev_interno_l2_t = fltarr(cant_pts)

for i=0,cant_pts-1 do begin

for j=0,12 do begin
M_l2_ne[i,j]= (*ptrsl2_ne[j])(i)
M_l2_t [i,j]= (*ptrsl2_t [j])(i)

endfor
vec_M_ne = M_l2_ne(i,*)
vec_M_t  = M_l2_t (i,*)
indice_M_l2_ok = where(vec_M_ne ne -666 and vec_M_ne ne -999);el indice deberia ser el mismo para te y ne no?? 
new_M_ne = vec_M_ne(indice_M_l2_ok)
new_M_t  = vec_M_t (indice_M_l2_ok)
stdev_interno_l2_ne(i)=stdev(new_M_ne)
stdev_interno_l2_t (i)=stdev(new_M_t )
endfor
;print, stdev_interno_l2_ne,'--> mean Ne',mean(stdev_interno_l2_ne)
;print, stdev_interno_l2_t ,'--> mean t ',mean(stdev_interno_l2_t )
;print, il
stdev_loop_ne(ileg+1)=mean(stdev_interno_l2_ne)
stdev_loop_t (ileg+1)=mean(stdev_interno_l2_t )

;defino footlat,footlon y footrad
  rad_ini(ileg)   = rad_l1(0)
  rad_ini(ileg+1) = rad_l1(0)
  rad_fin(ileg)   = rad_l2(n_elements(rad_l2)-1)
  rad_fin(ileg+1) = rad_l2(n_elements(rad_l2)-1)
  lat_ini(ileg)   = lat_l1(0)
  lat_ini(ileg+1) = lat_l1(0)
  lat_fin(ileg)   = lat_l2(n_elements(lat_l2)-1)
  lat_fin(ileg+1) = lat_l2(n_elements(lat_l2)-1)
  lon_ini(ileg)   = lon_l1(0)
  lon_ini(ileg+1) = lon_l1(0)
  lon_fin(ileg)   = lon_l2(n_elements(lon_l2)-1)
  lon_fin(ileg+1) = lon_l2(n_elements(lon_l2)-1)

  Footrad(ileg)   = rad_ini(ileg)
  Footrad(ileg+1) = rad_fin(ileg+1)
  Footlat(ileg)   = lat_ini(ileg)
  Footlat(ileg+1) = lat_fin(ileg+1)
  Footlon(ileg)   = lon_ini(ileg)
  Footlon(ileg+1) = lon_fin(ileg+1)

 
;----> finaliza el recorrido entre loops

skipfitloop:
     opclstat(ileg)   =      opcls(il)
     opclstat(ileg+1) =      opcls(il)
skipnextloop:
ileg = ileg+2

 
endelse
  endfor
;stop
if keyword_set(histos) then begin
if keyword_set(euvi) then begin
nombre='Loop_error_euvi_debug'
nbins=50
ind_St_LB = where(stdev_loop_ne ne -555. and opclstat eq 2. and abs(footlat) le lowlatmax and footlon le lonmax)
data1=stdev_loop_ne(ind_St_LB)
data2=stdev_loop_t (ind_St_LB)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_lowlat_Ne',ndig=5, tit='St-LB',max=11
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_lowlat_T ',ndig=5, tit='St-LB',max=3

i_midlat_small_norte_gradpos_HI = where(stdev_loop_ne ne -555.  AND opclstat eq 2. and footlat gt 30.  and footlon le lonmax)
data1=stdev_loop_ne(i_midlat_small_norte_gradpos_HI)
data2=stdev_loop_t (i_midlat_small_norte_gradpos_HI)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-LIN_Ne',ndig=5, tit='St-LIN',max=7
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-LIN_T ',ndig=5, tit='St-LIN',max=1.5

i_midlat_small_sur_gradpos_HI   = where(stdev_loop_ne ne -555.  AND opclstat eq 2. and footlat le -30. and footlon le lonmax )
data1=stdev_loop_ne(i_midlat_small_sur_gradpos_HI)
data2=stdev_loop_t (i_midlat_small_sur_gradpos_HI)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-LIS_Ne',ndig=5, tit='St-LIS',max=6
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-LIS_T ',ndig=5, tit='St-LIS',max=1.

i_large_OCN_gradpos_HI          = where(stdev_loop_ne ne -555.  AND opclstat eq 1. and footlat gt +40. and footlat lt  80. and footlon le lonmax )
data1=stdev_loop_ne(i_large_OCN_gradpos_HI)
data2=stdev_loop_t (i_large_OCN_gradpos_HI)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-FN_Ne',ndig=5, tit='St-FN'
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-FN_T ',ndig=5, tit='St-FN',max=1.5

i_large_OCS_gradpos_HI          = where(stdev_loop_ne ne -555.  AND opclstat eq 1. and footlat lt -40. and footlat gt -80. and footlon le lonmax )
data1=stdev_loop_ne(i_large_OCS_gradpos_HI)
data2=stdev_loop_t (i_large_OCS_gradpos_HI)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-FS_Ne',ndig=5, tit='St-FS',max=6.
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-FS_T ',ndig=5, tit='St-FS',max=1.25

i_open_norte_gradpos_HI_L       = where(stdev_loop_ne ne -555.  AND opclstat eq 0. and footlat lt 68.  and footlat gt 0.   and footlon le lonmax )
data1=stdev_loop_ne(i_open_norte_gradpos_HI_L)
data2=stdev_loop_t (i_open_norte_gradpos_HI_L)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LBN_Ne',ndig=5, tit='CH-LBN',max=10
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LBN_T ',ndig=5, tit='CH-LBN',max=2.

i_open_sur_gradpos_HI_L         = where(stdev_loop_ne ne -555.  AND opclstat eq 0. and footlat gt -68. and footlat le 0.   and footlon le lonmax )
data1=stdev_loop_ne(i_open_sur_gradpos_HI_L)
data2=stdev_loop_t (i_open_sur_gradpos_HI_L)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LBS_Ne',ndig=5, tit='CH-LBS',max=8
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LBS_T ',ndig=5, tit='CH-LBS'

i_open_norte_gradpos_HI_H       = where(stdev_loop_ne ne -555.  AND opclstat eq 0. and footlat ge 72.  and footlat le 80.  and footlon le lonmax )
data1=stdev_loop_ne(i_open_norte_gradpos_HI_H)
data2=stdev_loop_t (i_open_norte_gradpos_HI_H)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LAN_Ne',ndig=5, tit='CH-LAN'
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LAN_T ',ndig=5, tit='CH-LAN',max=1.

i_open_sur_gradpos_HI_H         = where(stdev_loop_ne ne -555.  AND opclstat eq 0. and footlat le -72. and footlat ge -80. and footlon le lonmax )
data1=stdev_loop_ne(i_open_sur_gradpos_HI_H)
data2=stdev_loop_t (i_open_sur_gradpos_HI_H)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LAS_Ne',ndig=5, tit='CH-LAS',max=6.
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LAS_T ',ndig=5, tit='CH-LAS',max=1.2
stop
endif 

if keyword_set(eit) then begin
lonmax =200.
lonmax2=320.
lonmax3=360.

lowlatmax=26
lowlatmin=-25

nombre='Loop_error_eit_error_euvi'
nbins=50
ind_St_LB = where(stdev_loop_ne ne -555. and opclstat eq 2. and footlat le lowlatmax and footlat ge lowlatmin and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)))
data1=stdev_loop_ne(ind_St_LB)
data2=stdev_loop_t (ind_St_LB)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_lowlat_Ne',ndig=5, tit='St-LB';,max=11
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_lowlat_T ',ndig=5, tit='St-LB';,max=3

St_LIN=26
i_midlat_small_norte_gradpos_HI = where(stdev_loop_ne ne -555.  AND opclstat eq 2. and footlat gt St_LIN  and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)))
data1=stdev_loop_ne(i_midlat_small_norte_gradpos_HI)
data2=stdev_loop_t (i_midlat_small_norte_gradpos_HI)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-LIN_Ne',ndig=5, tit='St-LIN';,max=7
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-LIN_T ',ndig=5, tit='St-LIN';,max=1.5

St_LIS=-25
i_midlat_small_sur_gradpos_HI   = where(stdev_loop_ne ne -555.  AND opclstat eq 2. and footlat le St_LIS and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)) )
data1=stdev_loop_ne(i_midlat_small_sur_gradpos_HI)
data2=stdev_loop_t (i_midlat_small_sur_gradpos_HI)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-LIS_Ne',ndig=5, tit='St-LIS';,max=6
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-LIS_T ',ndig=5, tit='St-LIS';,max=1.

St_FN=48
i_large_OCN_gradpos_HI          = where(stdev_loop_ne ne -555.  AND opclstat eq 1. and footlat gt St_FN and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)) )
data1=stdev_loop_ne(i_large_OCN_gradpos_HI)
data2=stdev_loop_t (i_large_OCN_gradpos_HI)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-FN_Ne',ndig=5, tit='St-FN'
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-FN_T ',ndig=5, tit='St-FN';,max=1.5

St_FS=-43
i_large_OCS_gradpos_HI          = where(stdev_loop_ne ne -555.  AND opclstat eq 1. and footlat lt St_FS and footlat gt -80. and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)) )
data1=stdev_loop_ne(i_large_OCS_gradpos_HI)
data2=stdev_loop_t (i_large_OCS_gradpos_HI)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-FS_Ne',ndig=5, tit='St-FS';,max=6.
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_St-FS_T ',ndig=5, tit='St-FS';,max=1.25

CH_LBN=67
i_open_norte_gradpos_HI_L       = where(stdev_loop_ne ne -555.  AND opclstat eq 0. and footlat lt CH_LBN and footlat gt 0.  and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)))
data1=stdev_loop_ne(i_open_norte_gradpos_HI_L)
data2=stdev_loop_t (i_open_norte_gradpos_HI_L)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LBN_Ne',ndig=5, tit='CH-LBN';,max=10
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LBN_T ',ndig=5, tit='CH-LBN';,max=2.

CH_LBS=-70
i_open_sur_gradpos_HI_L         = where(stdev_loop_ne ne -555.  AND opclstat eq 0. and footlat gt CH_LBS and footlat le 0.  and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)))
data1=stdev_loop_ne(i_open_sur_gradpos_HI_L)
data2=stdev_loop_t (i_open_sur_gradpos_HI_L)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LBS_Ne',ndig=5, tit='CH-LBS';,max=8
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LBS_T ',ndig=5, tit='CH-LBS'

CH_LAN=67
i_open_norte_gradpos_HI_H       = where(stdev_loop_ne ne -555.  AND opclstat eq 0. and footlat ge CH_LAN and footlat le 80. and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)))
data1=stdev_loop_ne(i_open_norte_gradpos_HI_H)
data2=stdev_loop_t (i_open_norte_gradpos_HI_H)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LAN_Ne',ndig=5, tit='CH-LAN'
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LAN_T ',ndig=5, tit='CH-LAN';,max=1.

CH_LAS=-70
i_open_sur_gradpos_HI_H         = where(stdev_loop_ne ne -555.  AND opclstat eq 0. and footlat le CH_LAS and footlat ge -80. and (footlon le lonmax or (footlon ge lonmax2 and footlon le lonmax3)))
data1=stdev_loop_ne(i_open_sur_gradpos_HI_H)
data2=stdev_loop_t (i_open_sur_gradpos_HI_H)
histoplot, data1/1.e6, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LAS_Ne',ndig=5, tit='CH-LAS';,max=6.
histoplot, data2/1.e5, nbins=nbins,xtit='Error Loop',ytit='Histograma de Frecuencia',filename=nombre+'_CH-LAS_T ',ndig=5, tit='CH-LAS';,max=1.2
stop
endif
endif


return
end

pro read_trace_sampled,file,alturas
  common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
  common B_sampled,B_v,Br_v,Bth_v,Bph_v
  common opclstatus,opcls,loopL,WTc  
;+
; PURPOSE:
; To read the output of the routine "trace_LDEM"
;-
  Nlin=0L      
  Npts_max=0.
  fieldtype=0.
  spacing =0. 
  radstart=fltarr(alturas)
  Rmax_tom=0.
  dr_tom=0.
  WTc=0.
  dir='/data1/DATA/MLDT/'
  ;dir='/data/DATA/trace/'
  openr,1,dir+file
  readu,1,fieldtype,spacing,radstart,Rmax_tom,dr_tom,WTc
  readu,1,Nlin,Npts_max
 
      Ne_v = fltarr(Npts_max,Nlin)
      Tm_v = fltarr(Npts_max,Nlin)
      WT_v = fltarr(Npts_max,Nlin)
      Er_v = fltarr(Npts_max,Nlin)
  scoreR_v = fltarr(Npts_max,Nlin)
     rad_v = fltarr(Npts_max,Nlin)
     lat_v = fltarr(Npts_max,Nlin)
     lon_v = fltarr(Npts_max,Nlin)
       s_v = fltarr(Npts_max,Nlin)
       B_v = fltarr(Npts_max,Nlin)
      Br_v = fltarr(Npts_max,Nlin)
     Bth_v = fltarr(Npts_max,Nlin)
     Bph_v = fltarr(Npts_max,Nlin)

 midCell_v = fltarr(Nlin) 
    Npts_v = fltarr(Nlin) 
     opcls = intarr(Nlin) 
     Loopl = fltarr(Nlin) 
     str_v = fltarr(Nlin)   
    stth_v = dblarr(Nlin)   
    stph_v = dblarr(Nlin)    
  readu,1,rad_v,lat_v,lon_v,s_v,npts_v,midcell_v,loopL,opcls
  readu,1,Ne_v,Tm_v,WT_v,Er_v,scoreR_v
  readu,1,str_v,stth_v,stph_v 
  readu,1,B_v,Br_v,Bth_v,Bph_v     
;------AGREGADO--------------
   enrad_v = fltarr(Nlin)   
   enlon_v = fltarr(Nlin)   
   enlat_v = fltarr(Nlin)  
;------AGREGADO--------------
 readu,1,enrad_v,enlon_v,enlat_v
;----------------------------
    Tmin=0.
    Tmax=0.
    L=0
 readu,1,Tmin,Tmax,L
    npar   = 0                      ;<--
    DEMc_v = fltarr(Npts_max,Nlin)  
 readu,1,npar,DEMc_v
  lambda_v = fltarr(Npts_max,Nlin,npar) 
 readu,1,lambda_v
  close,1
  return
end


function findel,value,array,matrix=matrix,array=retarray
  on_error,2;Return to caller if an error occurs                                                                                                                                               \
                                                                                                                                                                                                
diff = abs(array - value)
dum = min(diff,ind)

if keyword_set(retarray) then matrix=1
if keyword_set(matrix) then begin
    sz = size(array)
    ncol = sz[1]
    col = ind mod ncol
    row = ind / ncol
    ind = [col, row]
 endif
return,ind
end

pro histoplot,data,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename,ndig=ndig

ps1,'./newfigs/'+filename+'.eps',0
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
