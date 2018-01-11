pro test,ind=ind,filelabel=filelabel

common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base

common stat_filter,Nlegs_c,Nemean_c,Tmmean_c,WTmean_c,Nestddev_c,Tmstddev_c,WTstddev_c,loop_length_c,betamean_c,betaapex_c,Bmean_c,Br0_c
common stat_filter2,opclstat_c,lambda_N_c,lambda_p_c,Ne0_c,p0_c,Tefit_c,gradT_c,r2N_c,r2P_c,r2T_c,indexloop_c,leg_status_c,Tm0_c,Tm0s_c,Tm0_ts_c,gradT_ts_c,FTr_ts_c,Tm0s_ts_c,dTmds_ts_c,FTs_ts_c,Tm0s5_ts_c,dTmds5_ts_c,FTs5_ts_c    
common stat_filter3,Eh_c,sH_c,r2sH_c,Phir_c,Fcb_c,Ner0_c,Ner1_c,Ner2_c,Ner3_c,TmR1_c,NR1_c,Erlin_F_c,Erlin_d_c,Erlin_o_c,Erlin_r0_c,phir_lin_c
common stat_filter4,r2Tcuadr_c,Acuadr_a_c, s_r0_a_c,dTmds_c,r2Ts_c,Netech_c,Ne0_s_c,lambda_N_s_c
common stat_filter5,strad_c,stlat_c,stlon_c,footrad_c,footlat_c,footlon_c,Rp_rad_c,Rp_lat_c,Rp_lon_c
common stat_filter6,r2Er_c,Phirfit_c,eplegT_c,deltaS_c,Smaxxx_c,Sminnn_c,B_base_c,Fcb5_c

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_loop,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTs15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common loops_filter,Phir_loop_c,rad1_loop_c,rad2_loop_c,lat1_loop_c,lat2_loop_c,lon1_loop_c,lon2_loop_c,L_loop_c,opclstat_loop_c,r2Er1_loop_c,r2Er2_loop_c,Tmmean_loop_c,Fcb_loop_c,dTmds1_loop_c,dTmds2_loop_c,r2Tcuadr1_loop_c,r2Tcuadr2_loop_c,r2T1_loop_c,r2T2_loop_c,Ner01_loop_c,Ner02_loop_c,Ner11_loop_c,Ner12_loop_c,Ner21_loop_c,Ner22_loop_c,Ner31_loop_c,Ner32_loop_c,Rp_rad1_loop_c,Rp_lat1_loop_c,Rp_lon1_loop_c,Rp_rad2_loop_c,Rp_lat2_loop_c,Rp_lon2_loop_c,TmR11_loop_c,TmR12_loop_c,NR11_loop_c,NR12_loop_c,Nemean_loop_c,Long_cm_c,Phih_L_c,Ner0m_loop_c,lambda_N_loop_c,r2N1_loop_c,r2N2_loop_c,Tm01_ts_loop_c,gradT1_ts_loop_c,FTr1_ts_loop_c,Tm0s1_ts_loop_c,dTmds1_ts_loop_c,FTs1_ts_loop_c,Tm02_ts_loop_c,gradT2_ts_loop_c,FTr2_ts_loop_c,Tm0s2_ts_loop_c,dTmds2_ts_loop_c,FTs2_ts_loop_c,eplegT1_loop_c,eplegT2_loop_c,deltaS1_loop_c,deltaS2_loop_c,Smaxxx1_loop_c,Smaxxx2_loop_c,Sminnn1_loop_c,Sminnn2_loop_c,ISO_loop_c,B_basem_loop_c,Netech_loop_c,Ne0_s_loop_c,lambda_N_s_loop_c,Tm0s15_ts_loop_c,dTmds15_ts_loop_c,FTs15_ts_loop_c,Tm0s25_ts_loop_c,dTmds25_ts_loop_c,FTs25_ts_loop_c,Erlin1_F_loop_c,Erlin1_d_loop_c,Erlin1_o_loop_c,Erlin1_r0_loop_c,phir1_lin_loop_c,Erlin2_F_loop_c,Erlin2_d_loop_c,Erlin2_o_loop_c,Erlin2_r0_loop_c,phir2_lin_loop_c,Fcb5_loop_c

common label,T_label,Er_label

if not keyword_set(ind)       then ind=[0]
if not keyword_set(filelabel) then filelabel='crap'

filesT = ['traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_grid1_expand_radstart-1.045Rs_unifgrid.heating.sampled.v2.CECI.dat',$
          'traceLDEM_CR2081_euviA-DECON_reg0.75_safety1.0_grid1_expand_radstart-1.075Rs_unifgrid.heating.sampled.v2.CECI.dat',$
          'tracELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY1.0_GRID1_EXPAND_RADSTART-1.115RS_UNIFGRID.HEATING.SAMPLED.V2.CECI.DAT',$
          'TRACELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY1.0_EXPAND_RADSTART-1.035RS_UNIFGRID.HEATING.SAMPLED.V2.CECI.DAT',$
          'TRACELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY1.0_EXPAND_RADSTART-1.045RS_UNIFGRID.HEATING.SAMPLED.V2.CECI.DAT',$
          'TRACELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY1.0_EXPAND_RADSTART-1.075RS_UNIFGRID.HEATING.SAMPLED.V2.CECI.DAT',$
          'TRACELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY1.0_EXPAND_RADSTART-1.115RS_UNIFGRID.HEATING.SAMPLED.V2.CECI.DAT',$
          'TRACELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY1.0_EXPAND_RADSTART-1.155RS_UNIFGRID.HEATING.SAMPLED.V2.CECI.DAT',$
          'TRACELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY1.0_EXPAND_RADSTART-1.250RS_UNIFGRID.HEATING.SAMPLED.V2.CECI.DAT',$
          'TRACELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY1.0_EXPAND_RADSTART-1.500RS_UNIFGRID.HEATING.SAMPLED.V2.CECI.DAT']
RSTART      = [1.045,1.075,1.115,1.035,1.045,1.075,1.115,1.155,1.250,1.500]
 
;2081

;FILEST = ['TRACELDEM_CR2099_AIA3_REG0.75_SAFETY0.5_GRID0.5_RADSTART-1.035-1.215RS_UNIFGRID_V2.HEATING.SAMPLED.V2.DIEGO.DAT']
;FILEST = ['TRACELDEM_CR2081_EUVIA-DECON_REG0.75_SAFETY0.5_RADSTART-1.035-1.215RS_UNIFGRID_V2.HEATING.SAMPLED.V2.ALBERT.DAT']
;FILEST = ['TRACELDEM_CR2081_EUVIA-NODECON_REG0.75_SAFETY0.5_RADSTART-1.035-1.215RS_UNIFGRID_V2.HEATING.SAMPLED.V2.ALBERT.DAT']

;2099

;FILEST = ['TRACELDEM_CR2099_AIA4_REG0.75_SAFETY0.5_GRID0.5_RADSTART-1.035-1.215RS_UNIFGRID_V2.HEATING.SAMPLED.V2.DIEGO.DAT']
;FILEST = ['TRACELDEM_CR2099_EUVIA_REG1.0_SAFETY0.5_GRID0.5_RADSTART-1.035-1.215RS_UNIFGRID_V2.HEATING.SAMPLED.V2.DIEGO.DAT']
;FILEST = ['TRACELDEM_CR2099_AIA3_REG0.75_SAFETY0.5_GRID0.5_RADSTART-1.035-1.215RS_UNIFGRID_V2.HEATING.SAMPLED.V2.DIEGO.DAT']

;2099 nuevo y viejo

;filesT = ['traceLDEM_CR2099_aia3_reg0.75_safety0.5_grid0.5_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat']
;filesT = ['traceLDEM_CRCR2099_AIA3_reg0.75_safety0.5_Ce_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat']
;filest = ['traceldem_crcr2099_aia3_reg0.75_safety0.5_ce_prueba-18-05-16_radstart-1.035-1.215rs_unifgrid_v2.heating.sampled.v2.ceci.dat']
;filesT = ['traceLDEM_CRCR2099_AIA3_reg0.75_safety0.5_debug_19-05_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat']

;=========== archivos sin bug

;filesT=['traceLDEM_CR2081_euviA-NODECON_reg0.75_safety0.5_errorbox_base_DEBUG_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat'] ;2081 EUVIA
;filesT=['traceLDEM_CR2099_euviA_reg1.0_safety0.5_debug_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat']                         ;2099 EUVIA
;filesT=['traceLDEM_CRCR2099_AIA3_reg0.75_safety0.5_debug_20-5_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat']                  ;2099 AIA 3

;AIA-4

;filesT=['traceLDEM_CRCR2099_AIA4-full_reg0.75_safety0.5_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat']
 filesT=['traceLDEM_CRCR2099_AIA4-warm_reg0.75_safety0.5_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat']
;filesT=['traceLDEM_CRCR2099_AIA4-hot_reg0.75_safety0.5_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat']    

;HOLLOW
;filesT=['traceLDEM_CR2081_euviA-HOLLOW_reg0.75_safety0.5_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat']


rstart      = 1.035+0.02*findgen(10)

nfilest     = n_elements(filest)
nlineas     = findgen(nfilest)*0

nsmallclose = findgen(nfilest)*0
nlargeclose = findgen(nfilest)*0
nopen       = findgen(nfilest)*0

nsmall_leg  = findgen(nfilest)*0
nlarge_leg  = findgen(nfilest)*0
 nopen_leg  = findgen(nfilest)*0

nof1 = findgen(nfilest)*0
nof2 = findgen(nfilest)*0
nof  = findgen(nfilest)*0

nlf1 = findgen(nfilest)*0
nlf2 = findgen(nfilest)*0
nlf3 = findgen(nfilest)*0
nlf4 = findgen(nfilest)*0
nlf  = findgen(nfilest)*0

nsf1 = findgen(nfilest)*0
nsf2 = findgen(nfilest)*0
nsf3 = findgen(nfilest)*0
nsf4 = findgen(nfilest)*0
nsf  = findgen(nfilest)*0

marker_ind = intarr(nfilest)
marker_ind(ind)=1

for  i=0,nfilest-1 do begin
if marker_ind(i) eq 0 then goto,lainsoportablelevedad

   read_trace_sampled,filest(i)

   nlineas(i)=n_elements(rad_v(1,*))

   small_close = where(opcls eq 2) & nsmallclose(i) = n_elements(small_close)
   large_close = where(opcls eq 1) & nlargeclose(i) = n_elements(large_close)
   open        = where(opcls eq 0) &      nopen (i) = n_elements(open)

goto,ejemplo_grafico
window,i,xsize=800, ysize=900, title=rstart(i)
!p.multi=[0,2,3]

plot,rad_v(0:npts_v(5000)-1,5000), ne_v(0:npts_v(5000)-1,5000),yrange=[-0.3,1.5]*1.e8,xstyle=1,ystyle=1,title='density vs rad' ,charsize=2
plot,lat_v(0:npts_v(5000)-1,5000),rad_v(0:npts_v(5000)-1,5000),                       xstyle=1,ystyle=1,title='loop rad vs lat',charsize=2

plot,rad_v(0:npts_v(10000)-1,10000), ne_v(0:npts_v(10000)-1,10000),yrange=[-0.3,1.5]*1.e8,xstyle=1,ystyle=1,charsize=2
plot,lat_v(0:npts_v(10000)-1,10000),rad_v(0:npts_v(10000)-1,10000),                       xstyle=1,ystyle=1,charsize=2

plot,rad_v(0:npts_v(14000)-1,14000), ne_v(0:npts_v(14000)-1,14000),yrange=[-0.3,1.5]*1.e8,xstyle=1,ystyle=1,charsize=2
plot,lat_v(0:npts_v(14000)-1,14000),rad_v(0:npts_v(14000)-1,14000),                       xstyle=1,ystyle=1,charsize=2

!p.multi=0
ejemplo_grafico:

;record_gif,'./','cambio_ne_rstart_distintos.gif','x'

  statloop,filest(i),rloopmin=1.05,/linear;,/Tfitlow;,/fitcuadr

stop

 footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel=filelabel,indexloop_c,rotacion

stop


  ; select legs with enough data points
  iok = where(gradt ne -555.)
  footlat_s   = footlat(iok)
  footlon_s   = footlon(iok)
  opclstat_s  = opclstat(iok) 
  indexloop_s = indexloop(iok)
  
  ; create concatened data arrays for the footpoint maps
  if i eq ind(0) then begin
  print,'defino _c'
  footlat_c   = footlat_s
  footlon_c   = footlon_s  
  opclstat_c  = opclstat_s
  indexloop_c = indexloop_s
  help,footlat_c
  endif
  if i gt ind(0) and marker_ind(i) eq 1 then begin
  print,'concateno'
  footlat_c =[footlat_c,footlat_s]
  footlon_c =[footlon_c,footlon_s]
  opclstat_c=[opclstat_c,opclstat_s]
  indexloop_c=[indexloop_c,indexloop_s]
  help,footlat_c
  endif

;;=====================================energy=========================================================================
goto,cookbooks

window,6,xsize=800, ysize=900, title=rstart(i)
!p.multi=[0,1,3]

plot,footlat,phir,psym=3,charsize=3,xrange=[-90,90],ystyle=1,ytit='phir'

plot,footlat,fcb,psym=3,charsize=3,xrange=[-90,90],ystyle=1,ytit='fcbb'

;plot,latt,phihh,charsize=3,xrange=[-90,90],ystyle=1,ytit='phih'

!p.multi=0
cookbooks:
;===========================análisis de piernas=======================================================================

;===========open

;goto,lainsoportablelevedad
   open_leg=where(opclstat eq 0)            
   nopen_leg(i)=n_elements(open_leg)
   flopen=footlat(where(opclstat eq 0))

        open_foot1 = where(flopen le  80. and flopen ge  50.) & nof1(i)=n_elements(open_foot1) ;[50,80]
        open_foot2 = where(flopen le -50. and flopen ge -80.) & nof2(i)=n_elements(open_foot2) ;[-80,-50]
      
        nof(i)    = nof1(i) + nof2(i)

ofoot=findgen(nof(i))*0
ofoot(0:nof1(i)-1)=flopen(open_foot1)
ofoot(nof1(i):nof(i)-1)=flopen(open_foot2)

;=========small

   if rstart(i) lt 1.200 then begin ; lo separo porque para rstart altos, esto es -1 y ya no puede hacer nleg(-1)

      small_leg=where(opclstat eq 2) 
      nsmall_leg(i)=n_elements(small_leg)

      flsmall=footlat(where(opclstat eq 2))
  
        small_foot1 = where(flsmall lt  30. and flsmall ge   0.) & nsf1(i)=n_elements(small_foot1) ;[0,30)
        small_foot2 = where(flsmall le  70. and flsmall ge  30.) & nsf2(i)=n_elements(small_foot2) ;[30,70]
        small_foot3 = where(flsmall lt   0. and flsmall ge -30.) & nsf3(i)=n_elements(small_foot3) ;[-30,0)
        small_foot4 = where(flsmall lt -30. and flsmall ge -70.) & nsf4(i)=n_elements(small_foot4) ;[-70,-30)

        nsf(i)     = nsf1(i) + nsf2(i) + nsf3(i) + nsf4(i)       
 
sfoot=findgen(nsf(i))*0
sfoot(0:nsf1(i)-1)=flsmall(small_foot1)
sfoot(nsf1(i):nsf1(i)+nsf2(i)-1)=flsmall(small_foot2)
sfoot(nsf1(i)+nsf2(i):nsf1(i)+nsf2(i)+nsf3(i)-1)=flsmall(small_foot3)
sfoot(nsf1(i)+nsf2(i)+nsf3(i):nsf(i)-1)=flsmall(small_foot4)

endif else begin

if rstart(i) gt 1.200 then sfoot=findgen(7000)*0

endelse

;=========large
      
   large_leg=(where(opclstat eq 1))            
   nlarge_leg(i)=n_elements(large_leg)
   fllarge=footlat(where(opclstat eq 1))

        large_foot1 = where(fllarge lt  30. and fllarge ge   0.) & nlf1(i)=n_elements(large_foot1) ;[0.30)
        large_foot2 = where(fllarge le  70. and fllarge ge  30.) & nlf2(i)=n_elements(large_foot2) ;[30,70]
        large_foot3 = where(fllarge lt   0. and fllarge ge -30.) & nlf3(i)=n_elements(large_foot3) ;[-30,0)
        large_foot4 = where(fllarge lt -30. and fllarge ge -70.) & nlf4(i)=n_elements(large_foot4) ;[-70.-30)

        nlf(i)     = nlf1(i) + nlf2(i) + nlf3(i) + nlf4(i)            

lfoot=findgen(nlf(i))*0
lfoot(0:nlf1(i)-1)=fllarge(large_foot1)
lfoot(nlf1(i):nlf1(i)+nlf2(i)-1)=fllarge(large_foot2)
lfoot(nlf1(i)+nlf2(i):nlf1(i)+nlf2(i)+nlf3(i)-1)=fllarge(large_foot3)
lfoot(nlf1(i)+nlf2(i)+nlf3(i):nlf(i)-1)=fllarge(large_foot4)

;=============histograma
;goto,chauhisto

   xmin=-100
   xmax= 100
   nb  = 100 
   no1 = histogram(ofoot,min=xmin,max=xmax,nbins=nb,locations=xrat)
   ns1 = histogram(sfoot,min=xmin,max=xmax,nbins=nb,locations=xrat) 
   nl1 = histogram(lfoot,min=xmin,max=xmax,nbins=nb,locations=xrat)

window,i,xsize=800, ysize=900, title=rstart(i)

!p.multi=[0,1,3]

  plot,xrat,no1,charsize=3,yrange=[0,max([no1,1])],ytit='open_loops',/nodata
    oplot,xrat,no1,psym=10

  plot,xrat,nl1,charsize=3,yrange=[0,max([nl1,1])],ytit='large_loops',/nodata
     oplot,xrat,nl1,psym=10

  plot,xrat,ns1,charsize=3,yrange=[0,max([ns1,1])],xtit='footlat',ytit='small_loops',/nodata
    oplot,xrat,ns1,psym=10

!p.multi=0

chauhisto:
lainsoportablelevedad:

endfor

 footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel=filelabel,indexloop_c,rotacion


;====================================================================================================================

;goto,tografic

  print,'rstart        =',rstart
  print,'nlineas       =',nlineas
  print,'nsmallclose   =',nsmallclose
  print,'nlargeclose   =',nlargeclose
  print,'nopen         =',nopen
  print,''
  print,'análisis de piernas'

  print,'nsmall_leg    =',nsmall_leg
  print,'nlarge_leg    =',nlarge_leg
  print,'nopen_leg     =',nopen_leg
  print,''
  print,'open'
  print,'rsatrt          =',rstart
  print,'footlat[50,80]  =',nof1
  print,'footlat[-80,-50]=',nof2
  print,''
  print,'large_close'
  print,'rsatrt          =',rstart
  print,'footlat[-70,-30)=',nlf4
  print,'footlat[-30,0)  =',nlf3
  print,'footlat[0,30)   =',nlf1
  print,'footlat[30,70]  =',nlf2
  print,''
  print,'small_close'
  print,'rsatrt          =',rstart
  print,'footlat[-70,-30)=',nsf4
  print,'footlat[-30,0)  =',nsf3
  print,'footlat[0,30)   =',nsf1
  print,'footlat[30,70]  =',nsf2

tografic:

stop

return
end

pro hacer_trace

goto,hacerlobien
; ceci & albert:
;fdips_file='fdips_field_151x180x360_synop_mr_0.polfil.2081.dat'

;rad0=[1.045, 1.075, 1.115, 1.155, 1.250, 1.500]
rad0=[1.045,1.075,1.115]
for i=1,1 do begin

 fdips_file='fdips_field_150x180x360_synop_mr_0.polfil.2081.ubdat'
ldem_file ='ldem.v3_cr2081_l0.75_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_euvi.a_l171_decon_gauss1_lin_norm-median_singlstart'
   period ='2081_euvia-decon_reg0.75_safety1.0_grid1'
trace_ldem,fdips_file=fdips_file,ldem_file=ldem_file,period=period,radstart=rad0(i),safety=1.0,stepmax=15000,/unifgrid,dlat=1,dlon=1,/expand
;trace_ldem,fdips_file=fdips_file,ldem_file=ldem_file,period=period,radstart=rad0,safety=1.0,stepmax=15000,/unifgrid,dlat=2,dlon=2
endfor

hacerlobien:

radstart= 1.035 + 0.02 *findgen(10)
Nrad = n_elements(radstart)
dlat = 2. + fltarr(Nrad)
dlon = 2. + fltarr(Nrad)

   fdips_file='fdips_field_150x180x360_synop_Mr_0.polfil.2081.ubdat'
;  fdips_file='fdips_field_150x180x360_synop_Mr_0.polfil.2099.ubdat'

;  ldem_file='LDEM.v3_CR2099_l0.75_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_AIA3_FULLCORONA-ALBERT_171_corregido_gauss1_lin_Norm-median_singlStart' ;AIA3
;  ldem_file='LDEM.v3_cr2099_l1.0_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_euvi.A_L171_NODECON_gauss1_lin_Norm-median_singlStart'                   ;EUVI
;  ldem_file='LDEM.v3_cr2081_l0.75_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_euvi.A_L171_DECON_gauss1_lin_Norm-median_singlStart'                    ;EUVI


; ldem_file='ldem_cr2099_AIA4_gauss_warm.dat'
; ldem_file='ldem_cr2099_AIA4_gauss_hot.dat'
; ldem_file='LDEM.v3_CR2099_l0.75_chianti.ioneq_sun_coronal_1992_feldman_ext.abund_AIA4_FULLCORONA-ALBERT_256_corregido_gauss2FW_lin_Norm-median_singlStart'

;HOLLOW
  ldem_file='LDEM.v3.error_box.2081.euvi.A.base_HOLLOW_gauss1_lin_Norm-median_singlStart'

;  period = '2099_euviA_reg1.0_safety0.5_debug'
;  period ='CR2099_AIA3_reg0.75_safety0.5_debug_20-5'
;  period ='2081_euviA-DECON_reg0.75_safety0.5_Ce'

; period ='CR2099_AIA4-hot_reg0.75_safety0.5'
; period ='CR2099_AIA4-warm_reg0.75_safety0.5'
; period ='CR2099_AIA4-full_reg0.75_safety0.5'

  period = '2081_euviA-HOLLOW_reg0.75_safety0.5'

  trace_LDEM,fdips_file=fdips_file,ldem_file=ldem_file,period=period,safety=.5,stepmax=3000,dlat=dlat,dlon=dlon,radstart=radstart,/unifgrid_v2;,/dgfw

RETURN
end

pro trace_LDEM,fdips_file=fdips_file,$
               ldem_file=ldem_file,$
               period=period,$
               outputfile=outputfile,$
               marcgrid=marcgrid,$
               unifgrid=unifgrid,$
               radstart=radstart,$
               spacing=spacing,$
               fieldtype=fieldtype,$
               safety=safety,$
               stepmax=stepmax,$
               Box=box,$
               dlat=dlat,$
               dlon=dlon,$
               mhd=mhd,$
               dgfw=dgfw,$
               expand=expand,$
               unifgrid_v2=unifgrid_v2

  common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
  common results_tomo,tfbe,sfbe,N_e
  common loss_rate,Er
  common structure ,sph_data
  common structure2,pfss_data 

;+
; PURPOSE: 
; To trace tomographic results along magnetic field lines. 
; To sample the LDEM results that are already traced along magnetic fieldlines,
; keeping only one data point per tomographic cell (voxel), chosen as the median
; point of all the points that are located within each voxel.
;
; INPUTS:
; fdips_file = a file containing a FDIPS output.
; ldem_file  = a file containing a LDEM output.
; period = a string containing the Carrington rotation number.
; outputfile = not in use, output filename is currently created within the code.
; radstart = height [Rsun] where to start to trace, both inwards and outwards.
; spacing, fieldtype, safety = various keywords needed if /marcgrid is used, see
;                              comments in "spherical_field_start_coord.pro".
; box = [lon1,lat1,lon2,lat2] is the angular box where the starting
; points are to be located. If not set the default is full corona.
; nlat,nlon = needed if /unifgrid is used. Default values are: 90,180.
;
; KEYWORDS:
; /unifgrid = set up a uniform angular grid for the starting points.
; /marcgrid = use Marc's tools starting points routine.
; /dgfw     = set up if the double normal LDEM parametrization is used.
;
; OUTPUTS:
; A file containing the results, to be afterwards used as INPUT by the routine
; "sample_traced_ldem". Modificar

;
; FUTURE CHANGES: 
; Make it capable of using magnetic models other than FDIPS.
;
; HISTORY:
; Created by F.A. Nuevo & A.M. Vasquez.
;
; 7/24/15
; Modified by C. Mac Cormack & A.M. Vasquez.
;    - Trazado de los parámetros de la LDEM
;    - Expansión del trazado geométrico hasta 2.5 Rsun
;-
!EXCEPT=2
  if not keyword_set(fdips_file) then begin
     print,'set the PFSS model to trace the DEMT results'
     return
  endif
  if not keyword_set(ldem_file ) then begin
     print,'set a DEMT file to be traced'
     return
  endif
  if not keyword_set(period)     then begin
     print,'set a string with the period'
     return
  endif

if keyword_set(expand) then period=period+'_expand'

; Add radstart suffix to the output filename:
  if n_elements(radstart) eq 1 then $
  period=period+'_radstart-'+strmid(string(radstart),6,5)+'Rs' 
  if n_elements(radstart) gt 1 then $
  period=period+'_radstart-'+strmid(string(radstart(0)),6,5)+'-'+strmid(string(radstart(n_elements(radstart)-1)),6,5)+'Rs'

; the output filename:
  if keyword_set(marcgrid) then suffix='_marcgrid'  
  if keyword_set(unifgrid) then suffix='_unifgrid'
  if keyword_set(unifgrid_v2) then suffix='_unifgrid_v2'
  output_file='traceLDEM_CR'+period+suffix+'.heating.sampled.v2.CECI.dat'

; Set parameters for Marc's line-tracing routines:
  if NOT keyword_set(radstart ) then radstart  = 1.5
  if NOT keyword_set(safety   ) then safety    = 0.2
  if NOT keyword_set(stepmax  ) then stepmax   = 30000
  if NOT keyword_set(fieldtype) then fieldtype = 5.0
  if NOT keyword_set(spacing)   then spacing   = 2.0

  print,'-------------------------------------------'
  print,'     Period: ',period
  print,'  LDEM file: ',ldem_file
  print,' FDIPS file: ',fdips_file
  print,'Output file: ',output_file
  print,'-------------------------------------------'

  print,'safety: ',safety
  print,'stepmax: ',stepmax

; Set the FDIPS filename to read:
; PFSSM_model='/data1/DATA/PFSSM/'+fdips_file
  PFSSM_model= fdips_file
; Read the FDIPS model and create a structure to serve as input to Marc's routines:
  if not keyword_set(mhd)  then create_structure    ,PFSSM_model
  if     keyword_set(mhd)  then create_structure_MHD,'/data1/DATA/MHD_SWMF/'+fdips_file
; change the name of the created structure to a new name:
  pfss_data = sph_data

; Set the uniform grid size, in case /unifgrid is used for the starting points. 
; Default size is 90x180.
  if NOT keyword_set(dlat) then dlat = 2   
  if NOT keyword_set(dlon) then dlon = 2

; If BOX was not set use the full corona:
  if NOT keyword_set(box)  then box = [0.,-90,360.,+90.] 
  box=float(box)

; Set up the starting points:
  if keyword_set(marcgrid)    then spherical_field_start_coord,pfss_data,fieldtype,spacing,radstart=radstart,bbox=box
  if keyword_set(unifgrid)    then sph_field_str_coord_unifang,pfss_data,dlat,dlon        ,radstart=radstart,bbox=box
  if keyword_set(unifgrid_v2) then sph_field_str_coord_unifang_v2,pfss_data,dlatv=dlat,dlonv=dlon,radstartv=radstart,bbox=box


; And now, do trace the field lines:
  spherical_trace_field,pfss_data,linekind=linekind,linelengths=linelengths,safety=safety,stepmax=stepmax 

; Change the coding for linekind:
  linekind=linekind-2           ; so that 0=open and 1=closed

; Number of total traced lines:
  Nlin_all   = (size(*pfss_data.ptr))(2)
; Maximum number of points along those lines:
  Nptmax     = (size(*pfss_data.ptr))(1)
; Select field lines that were identified as open or closed by
  iOC        = where ( linekind eq 0 or linekind eq 1)
; Number of selected field lines (open or closed)
  Nlin     = 0L
  Nlin     = n_elements(iOC) 
  print,'there are '+string(Nlin)+' of '+string(Nlin_all)+' field lines that are within  the boundaries'
; only keep the values of the array for selected field lines (open or
; closed) 
;---------------------------------------------------------------
  loopL       = linelengths       (iOC) ; N_lineas
  opcls       = linekind          (iOC) ; N_lineas
  nstep       = (*pfss_data.nstep)(iOC) ; N_lineas
  str_v       = (*pfss_data.str)  (iOC) ; N_lineas
  stth_v      = (*pfss_data.stth) (iOC) ; N_lineas  
  stph_v      = (*pfss_data.stph) (iOC) ; N_lineas  
;-----------------------------------------------------------

; Read the tomographics results and set a few parameters concerning
; the tomographic grid:
  if not keyword_set(dgfw) then $
     read_ldem,ldem_file,/ldem,/gauss1
  if     keyword_set(dgfw) then $
     read_ldem,ldem_file,/ldem,/dgfw
  dr_tom = rad(1)-rad(0)        ; grid radial bin size
  Rmax_tom = rad(nr-3)          ; maximum height for which LDEM was computed

;<--
if keyword_set(expand) then begin
nr=150
rad=1.+dr_tom/2+dr_tom*findgen(Nr)
endif
;<--

; Compute the scoreR for quality-selection purposes:
  ratio = sfbe/tfbe
 ;scoreR=total(    (1.-ratio)^2 , 4 ) / float(nband)
  scoreR=total( abs(1.-ratio)   , 4 ) / float(nband)

  Nptmax_v = 150                ; ESTO NO ES ROBUSTO,                           
  if keyword_set(expand) then Nptmax_v = 1500

;  sin embargo, por la experiencia de haber realizado varios trazados
;  creo que va funcionar. 
;  Ningun sampleo supera este valor de puntos por linea

; Generate the tomographic grid based results
; one data point per tomographic voxel crossed by line.
      Ne_v = fltarr(Nptmax_v,Nlin)
      Tm_v = fltarr(Nptmax_v,Nlin)
      WT_v = fltarr(Nptmax_v,Nlin)
      Er_v = fltarr(Nptmax_v,Nlin)

      npar = (size(lambda))(4)         
  lambda_v = fltarr(Nptmax_v,Nlin,npar)
    DEMc_v = fltarr(Nptmax_v,Nlin)     

  scoreR_v = fltarr(Nptmax_v,Nlin) 
     rad_v = fltarr(Nptmax_v,Nlin)
     lat_v = fltarr(Nptmax_v,Nlin)
     lon_v = fltarr(Nptmax_v,Nlin)
       s_v = fltarr(Nptmax_v,Nlin)
       B_v = fltarr(Nptmax_v,Nlin)
      Br_v = fltarr(Nptmax_v,Nlin)
     Bth_v = fltarr(Nptmax_v,Nlin)
     Bph_v = fltarr(Nptmax_v,Nlin)
 midCell_v = fltarr(Nlin) - 666.
    Npts_v = fltarr(Nlin) - 666. 
   enrad_v = 0. * str_v - 555.
   enlon_v = 0. * str_v - 555.
   enlat_v = 0. * str_v - 555.

; The following double-loop traces the tomographic results along the
; selected field lines:
xxx=0L
  for il = 0L, Nlin-1 do begin

;     il=100  ;<--
     ;stop    ;<--

     print,'tracing the DEMT results along the line '+string(il+1)+'    of '+string(Nlin)
     il_all=(findgen(Nlin_all))(iOC(il))
     Np_l      = Nstep(il)      ;  Number of points along the il-line
     
; Build more arrays:   
           s_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          Ne_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          Tm_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          WT_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          Er_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.  
      lambda_l = fltarr(Np_l,npar) -666. ;fltarr(Nptmax,Nlin) -666.
        DEMc_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
      scoreR_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
          Br_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
         Bth_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
         Bph_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
           B_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
         lab_l = fltarr(Np_l)      -666. ;fltarr(Nptmax,Nlin) -666.
        
; These next five arrays are futures implementacion
  ;Happix    = fltarr(Np_l) -666.
  ;Bappix    = fltarr(Np_l) -666.
  ;Bmean     = fltarr(Np_l) -666.
  ;Bfoot1    = fltarr(Np_l) -666.
  ;Bfoot2    = fltarr(Np_p) -666. 

     
   rad_l  = reform((*pfss_data.ptr) (0:Np_l-1,il_all))
    th_l  = reform((*pfss_data.ptth)(0:Np_l-1,il_all))
   lat_l  = 90. - th_l / !dtor
    ph_l  = reform((*pfss_data.ptph)(0:Np_l-1,il_all))
; Make sure ph_l is in the range [0,2pi]:
  iphneg  = where( ph_l lt 0.    ) & if iphneg(0) ne -1 then ph_l(iphneg)=ph_l(iphneg)+2.*!pi  
  iph5th  = where( ph_l gt 2.*!pi) & if iph5th(0) ne -1 then ph_l(iph5th)=ph_l(iph5th)-2.*!pi
   lon_l  = ph_l/!dtor
      xl  = rad_l * sin(th_l) * cos(ph_l)
      yl  = rad_l * sin(th_l) * sin(ph_l)
      zl  = rad_l * cos(th_l)
     ds2l = (xl(1:Np_l-1)-xl(0:Np_l-2))^2 + $
            (yl(1:Np_l-1)-yl(0:Np_l-2))^2 + $
            (zl(1:Np_l-1)-zl(0:Np_l-2))^2 
     ds2l = [0,ds2l]
;-------------AGREGADO---------------------------------------
; Store coordinates of ending point of all OPEN field lines
if opcls(il) eq 0 then begin
enrad_v(il)=rad_l(Np_l-1)
enlat_v(il)=lat_l(Np_l-1)
enlon_v(il)=lon_l(Np_l-1)
endif 
;-----------------------------------------------------------
;
xxx=0L
  for ir = 0,Np_l-1 do begin
      s_l (ir) = total(sqrt(ds2l(0:ir)))
      r0 = rad_l (ir)
     th0 =  th_l(ir)
     ph0 =  ph_l(ir)
     ptc = [r0,th0,ph0]
     irc = get_interpolation_index(*pfss_data.rix,ptc(0))
     ithc= get_interpolation_index(*pfss_data.lat,90-ptc(1)*!radeg)
     iphc= get_interpolation_index(*pfss_data.lon,(ptc(2)*!radeg+360) mod 360)
      Brc= interpolate(*pfss_data.Br ,iphc,ithc,irc)
     Bthc= interpolate(*pfss_data.Bth,iphc,ithc,irc)
     Bphc= interpolate(*pfss_data.Bph,iphc,ithc,irc)  
     
     if r0 eq max(rad)+dr_tom/2 then r0=r0*(1.-1.e-5)
     if r0 lt max(rad)+dr_tom/2 then begin
      determindex,r0,th0,ph0,irad,ilat,ilon
     if irad ne -1 and ilon ne -1 and ilat ne -1 then begin
         lab_l(ir) = (Nth*Np)*irad+(Np)*ilat+ilon+1 ; Voxel label
        if  rad_l(ir) le Rmax_tom+dr_tom/2 then begin ;<--
             Ne_l(ir)   = N_e(irad,ilat,ilon)
             Tm_l(ir)   = Tm (irad,ilat,ilon)
             WT_l(ir)   = WT (irad,ilat,ilon)
             Er_l(ir)   = Er (irad,ilat,ilon)
         lambda_l(ir,*) = lambda(irad,ilat,ilon,*)  ;<-- grabo cada componente
         DEMc_l  (ir)   = DEMc  (irad,ilat,ilon)  ;<--
         scoreR_l(ir)   = scoreR (irad,ilat,ilon) 
        endif                
          Br_l(ir) = Brc
         Bth_l(ir) = Bthc
         Bph_l(ir) = Bphc
           B_l(ir) = sqrt(Brc^2 + Bthc^2 + Bphc^2) 
      endif 
   endif

  endfor                        ; closes radial loop

; beginning the sampled
;--------------------------------------------------------------------------------------------------

if rad_l(0) gt 2.4 then stop

        line_end = 0
        ivox     = 0
        is       = 0 
        lab0     = lab_l(is)
        while line_end eq 0 do begin
           npp=0
           while lab_l(is) eq lab0 do begin
              npp=npp+1        
              is=is+1
               if lab0 eq -666. or is-1 eq Np_l-1 then goto,nextvoxel
               ;if lab0 eq -666.                   then goto,nextvoxel 
;this line was modified 2013-08-16 by F.A.N. there was not robust enough
 
           endwhile
           ; "npp" should be now the number of points within voxel "lab0" 
           ; "is" should be now the index of the 1st point in the NEXT voxel  
           index=is-npp+indgen(npp)
           ; "index" should be now the 1-D index of all points in
           ; voxel lab0, to be used in arrays *_l
           ind = (median(index))(0)
                Ne_v(ivox,il) =     Ne_l(ind)
                Tm_v(ivox,il) =     Tm_l(ind)
                WT_v(ivox,il) =     WT_l(ind)
                Er_v(ivox,il) =     Er_l(ind)
            lambda_v(ivox,il,*)=lambda_l(ind,*)
              DEMc_v(ivox,il) =   DEMc_l(ind)  
            scoreR_v(ivox,il) = scoreR_l(ind)
           if npp mod 2 eq 1 then begin  ; does this if npp=odd 
               rad_v(ivox,il) =    rad_l(ind)
               lat_v(ivox,il) =    lat_l(ind)
               lon_v(ivox,il) =    lon_l(ind)
                 s_v(ivox,il) =      s_l(ind)
                 B_v(ivox,il) =      B_l(ind)
                Br_v(ivox,il) =     Br_l(ind) 
               Bth_v(ivox,il) =    Bth_l(ind)
               Bph_v(ivox,il) =    Bph_l(ind)     
           endif else begin             ; does this if npp=even 
             rad_v(ivox,il) = 0.5*(rad_l(ind)+rad_l(ind-1))
             lat_v(ivox,il) = 0.5*(lat_l(ind)+lat_l(ind-1))
             lon_v(ivox,il) = 0.5*(lon_l(ind)+lon_l(ind-1))
               s_v(ivox,il) = 0.5*(  s_l(ind)+  s_l(ind-1))
               B_v(ivox,il) = 0.5*(  B_l(ind)+  B_l(ind-1))
              Br_v(ivox,il) = 0.5*( Br_l(ind)+ Br_l(ind-1))
             Bth_v(ivox,il) = 0.5*(Bth_l(ind)+Bth_l(ind-1))
             Bph_v(ivox,il) = 0.5*(Bph_l(ind)+Bph_l(ind-1))   
           endelse
           ; If this voxel is the maximum of the tom grid
           ; but there are MORE points traced => it is a closed
           ; loop with appex > Rmax_tom. If so, store midCell_v.
           ; Also, do it only if midCell_v was not stored already.
           ; See in Zhenguang's Matlab code that this midCell_v 
           ; is same as his.
           ; Rmax_tom = 1.255 ---> 1.24 
           ; dr_tom = 0.01

           if (rad_l(ind) ge Rmax_tom - dr_tom/2) AND (max(index) lt Np_l-1) AND opcls(il) eq 1. then midCell_v(il) = ivox
           ivox = ivox+1 ; increase ivox 
           nextvoxel:
            if is-1 eq Np_l-1 then line_end = 1
            if is-1 lt Np_l-1 then     lab0 = lab_l(is)          

         endwhile; closes each line's while
        Npts_v(il) = ivox; For each line, record the number of points in vector
; Label small-closed-loops as "2"
   if opcls(il) eq 1. and midCell_v(il) eq -666. then begin
       opcls(il) = 2.
;     midcell_v(il) = where ( rad_v(0:npts_v(il)-1,il) eq max (rad_v(0:npts_v(il)-1,il)))
       escalar = where ( rad_v(0:npts_v(il)-1,il) eq max (rad_v(0:npts_v(il)-1,il)))
       midcell_v(il)= escalar(0)
                                        ;Mofied 12/05/2016 by D.G.L, evading a 
                                        ;vector prevents an erroneous loop definition  
       endif

;if opcls(il) eq 1. and max(rad_v(*,il)) le (Rmax_tom - dr_tom/2) then stop

;end the sampled
;-------------------------------------------------------------------------------------------
;il=il+1
;endif

endfor; closes lines loop

; Trim all unnecesary information from resulting arrays:
 Npts_max = max(Npts_v)
    Ne_v  = reform(     Ne_v(0:Npts_max-1,*) ) 
    Tm_v  = reform(     Tm_v(0:Npts_max-1,*) ) 
    WT_v  = reform(     WT_v(0:Npts_max-1,*) )
    Er_v  = reform(     Er_v(0:Npts_max-1,*) )
lambda_v  = reform( lambda_v(0:Npts_max-1,*,*) )
  DEMc_v  = reform(   DEMc_v(0:Npts_max-1,*) )  
scoreR_v  = reform( scoreR_v(0:Npts_max-1,*) ) 
   rad_v  = reform(    rad_v(0:Npts_max-1,*) ) 
   lat_v  = reform(    lat_v(0:Npts_max-1,*) ) 
   lon_v  = reform(    lon_v(0:Npts_max-1,*) ) 
     s_v  = reform(      s_v(0:Npts_max-1,*) )
     B_v  = reform(      B_v(0:Npts_max-1,*) )
    Br_v  = reform(     Br_v(0:Npts_max-1,*) )
   Bth_v  = reform(    Bth_v(0:Npts_max-1,*) )
   Bph_v  = reform(    Bph_v(0:Npts_max-1,*) )  
; Save the sampled data:
      openw,1,output_file
   writeu,1,fieldtype,spacing,radstart,Rmax_tom,dr_tom,WTc
   writeu,1,Nlin,Npts_max
   writeu,1,rad_v,lat_v,lon_v,s_v,npts_v,midcell_v,loopL,opcls
   writeu,1,Ne_v,Tm_v,WT_v,Er_v,scoreR_v
   writeu,1,str_v,stth_v,stph_v
   writeu,1,B_v,Br_v,Bth_v,Bph_v   
;------AGREGADO--------------
   writeu,1,enrad_v,enlon_v,enlat_v
;<---------------
   L=0
   if Tmax gt 3.4e6 and Tmax lt 3.6e6 then L=171
   if Tmax gt 3.9e6 and Tmax lt 4.1e6 then L=192
   writeu,1,Tmin,Tmax,L
   writeu,1,npar,DEMc_v
   writeu,1,lambda_v   
;<---------------
  close,  1
return
end

pro determindex,r0,th0,ph0,irad,ilat,ilon
  common comunes,tm,wt,nband,demc,PHI,parametrizacion,Tmin,Tmax,nr,nth,np,rad,lat,lon,lambda,WTc
  common structure2,pfss_data
 ; Purpose: Given the position vector coordinates (r0,th0,ph0)
 ;          find the 3 1D-indexes of the tomographic grid cell
 ;          that contains that position.
 drad = rad(1) - rad (0)
 dlat = lat(1) - lat (0)
 dlon = lon(1) - lon (0)   
 rad0 = r0
 lat0 = 90 - th0/!dtor
 lon0 =      ph0/!dtor
 irad = (where(rad0 ge rad-drad/2 AND rad0 lt rad+drad/2))(0) 
 ilat = (where(lat0 ge lat-dlat/2 AND lat0 lt lat+dlat/2))(0) 
 ilon = (where(lon0 ge lon-dlon/2 AND lon0 lt lon+dlon/2))(0)                
 return
end

pro read_trace_sampled,file
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
  radstart=fltarr(10)
  Rmax_tom=0.
  dr_tom=0.
  WTc=0.
  dir='/data1/DATA/MLDT/'
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

;make_mapoc,'traceLDEM_CR1914_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1914_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR1915_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1915_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR1919_eit_l0.75_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR1919_90X180blines_r_',1.075,/mdi
;make_mapoc,'traceLDEM_CR2081_euviA_l1.0_radstart-1.075Rs_unifgrid.heating.sampled.v2.dat','CR2081_90X180blines_r_',1.075,/mdi
pro MAKE_MAPOC,file_input,filesuffix,rc,mdi=mdi,gng=gng
common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  

dir='/data1/DATA/PFSSM/'

 if keyword_set(mdi) then postsuffix='_MDI.dat'
 if keyword_set(gng) then postsuffix='_GNG.dat'

 stringheight=strmid(string(rc),6,5)
 file_output=filesuffix+stringheight+'_open-close-map'+postsuffix
 print,'-----> O/C: '+file_output
 
read_trace_sampled,file_input

stth = stth_v
stph = stph_v

nlat= 90
nlon=180
dlat=180./nlat
dlon=360./nlon
latmin = -90.
lonmin =   0.
lat = latmin + dlat /2. + dlat * findgen(nlat)
lon = lonmin + dlon /2. + dlon * findgen(nlon)

stlon = round(    stph/!dtor)*1.
stlat = round(90.-stth/!dtor)*1.

N = n_elements(stlon)

mapoc = fltarr(1,nlat,nlon)

for i=0,N-1 do begin
  ilat = where(lat eq stlat(i))
  ilon = where(lon eq stlon(i))
  mapoc(0,ilat,ilon) = opcls(i)
endfor
;quedo: 0=abierto o indeterminado
;       1=cerrado
;cambiar al final a:  (open = 10) y (closed = 0.1)

iopen   = where ( mapoc eq 0 )
iclosed = where ( mapoc eq 1 or mapoc eq 2)

mapoc(iopen)   = 10
mapoc(iclosed) = 0.1

 openw,1,dir+file_output
 writeu,1,mapoc
 close,1

return
end


pro statloop,file,rmin=rmin,rmax=rmax,rloopmin=rloopmin,linear=linear,fitcuadr=fitcuadr,Tfitlow=Tfitlow
common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v   
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc
common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin

common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base 
common angle_box,rad_ini,rad_fin,lat_ini,lat_fin,lon_ini,lon_fin

device, retain     = 2
device, true_color = 24
device, decomposed = 0

; Physical constants needed for the HS fits:
  rsun = 6.955e10    ; cm
  gsun = 2.74e4      ; cm/sec²
    kB = 1.38e-16    ; erg/K
    mH = 1.6726e-24  ; gr
     a = 0.08        ; N_He / N_H
    mu = (1.+4.*a)/(1.+2.*a)
    bb = (1.+2.*a)/(2.+3.*a)
 kappa = 9.2e-7      ; erg s ^-1 cm ^-1 K ^-7/2 



if not keyword_set(rmin) then rmin = 1.03 
if not keyword_set(rmax) then rmax = 1.20
if not keyword_set(rloopmin) then rloopmin = 1.07
rminloop=rloopmin

; Read the sampled tomographic traced data
read_trace_sampled,file

Nloop = n_elements(loopL)

index0 = where(opcls eq 0.)
index1 = where(opcls eq 1.)
index2 = where(opcls eq 2.)

Nloop0 = n_elements(index0) & if index0(0) eq -1 then Nloop0=0
Nloop1 = n_elements(index1) & if index1(0) eq -1 then Nloop1=0
Nloop2 = n_elements(index2) & if index2(0) eq -1 then Nloop2=0

if Nloop0 + Nloop1 + Nloop2 ne Nloop then stop

Nlegs = Nloop0 + 2*Nloop1 + 2*Nloop2 

; Mean and standard desviation values for each leg
 Nemean  = fltarr(Nlegs)-555.
 Tmmean  = fltarr(Nlegs)-555.
 WTmean  = fltarr(Nlegs)-555.
Nestddev = fltarr(Nlegs)-555.
Tmstddev = fltarr(Nlegs)-555.
WTstddev = fltarr(Nlegs)-555.
; HS fits results for each leg
     Ne0 = fltarr(Nlegs)-555.
    Ner0 = fltarr(Nlegs)-555.

     Ne0_s = fltarr(Nlegs)-555.   
lambda_N_s = fltarr(Nlegs)-555.   
    Netech = fltarr(Nlegs)-555.

    Ner1 = fltarr(Nlegs)-555.
    Ner2 = fltarr(Nlegs)-555.
    Ner3 = fltarr(Nlegs)-555.

lambda_N = fltarr(Nlegs)-555.
   Tefit = fltarr(Nlegs)-555.
     r2N = fltarr(Nlegs)-555.
      P0 = fltarr(Nlegs)-555.
lambda_P = fltarr(Nlegs)-555.
     r2P = fltarr(Nlegs)-555.
   gradT = fltarr(Nlegs)-555.
     Tm0 = fltarr(Nlegs)-555.  ; esto es r

    Tm0s = fltarr(Nlegs)-555.  ; en cada pierna. esto es s

;========================TS

   FTr_ts = fltarr(Nlegs)-555.
 gradT_ts = fltarr(Nlegs)-555.
   Tm0_ts = fltarr(Nlegs)-555.  ; esto es r
   FTs_ts = fltarr(Nlegs)-555.
 dTmds_ts = fltarr(Nlegs)-555.
  Tm0s_ts = fltarr(Nlegs)-555.  ; en cada pierna. esto es s

   eplegT = fltarr(Nlegs)-555.
   deltaS = fltarr(Nlegs)-555.
   Smaxxx = fltarr(Nlegs)-555.
   Sminnn = fltarr(Nlegs)-555.


;AJUSTES DE POCOS PUNTOS :<=====================================
;===============================================================

  FTs5_ts = fltarr(Nlegs)-555.  
dTmds5_ts = fltarr(Nlegs)-555.  
 Tm0s5_ts = fltarr(Nlegs)-555.  

     Fcb5 = fltarr(Nlegs)-555.  

 Erlin_F  = fltarr(Nlegs)-555.  
 Erlin_d  = fltarr(Nlegs)-555.  
 Erlin_o  = fltarr(Nlegs)-555.  
 Erlin_r0 = fltarr(Nlegs)-555.  
 phir_lin = fltarr(Nlegs)-555.  

;==========================

     r2T = fltarr(Nlegs)-555.
   dTmds = fltarr(Nlegs)-555.
  Tmbase = fltarr(Nlegs)-555.

    TmR1 = fltarr(Nlegs)-555.  ;Temperatura a 1.075 ================
     NR1 = fltarr(Nlegs)-555.  ;Densidad    a 1.075 ================

    r2Ts = fltarr(Nlegs)-555.
    r2Er = fltarr(Nlegs)-555.
r2Tcuadr = fltarr(Nlegs)-555.
Acuadr_a = fltarr(Nlegs,3)-555.
  s_r0_a = fltarr(Nlegs)-555.
      Eh = fltarr(Nlegs)-555.

    Phir = fltarr(Nlegs)-963.  ; flujo radiativo
     Fcb = fltarr(Nlegs)-963.  ; Fc en la base
 Phirfit = fltarr(Nlegs)-963.  ; flujo radiativo

 deltaEh = fltarr(Nlegs)-555.
      sH = fltarr(Nlegs)-555.
    r2sH = fltarr(Nlegs)-555.
betamean = fltarr(Nlegs)-555.
betaapex = fltarr(Nlegs)-555.
   Bmean = fltarr(Nlegs)-555.
     Br0 = fltarr(Nlegs)-555. ;only applied for open lines 

  B_base = fltarr(Nlegs)-555.                                                                               ;<---

; opclstat=0. if loop is open, opclstat=1. if closed large; opclstat=2 if closed small. 
opclstat  = fltarr(Nlegs)-555.
indexloop = fltarr(Nlegs)-555.
; The following arrays will contain the LOOP length (in Rsun) to which each LEG belongs: 
loop_length = fltarr(Nlegs)-555.
; The following arrays will contain the coordinates of 
; the starting point used for the LOOP to which the LEG belongs:
strad = fltarr(Nlegs)-555.
stlat = fltarr(Nlegs)-555.
stlon = fltarr(Nlegs)-555.
; The following arrays will contain the coordinates of 
; the initial and final points of the LOOP to which each LEG belongs:
rad_ini = fltarr(Nlegs)-555.
lat_ini = fltarr(Nlegs)-555.
lon_ini = fltarr(Nlegs)-555.
rad_fin = fltarr(Nlegs)-555.
lat_fin = fltarr(Nlegs)-555.
lon_fin = fltarr(Nlegs)-555.
; The following arrays will contain the coordinates of 
; the FOOT-POINT of each LEG:
footrad = fltarr(Nlegs)-555.
footlat = fltarr(Nlegs)-555.
footlon = fltarr(Nlegs)-555.

;=======================================
Rp0_rad = fltarr(Nlegs)-555.
Rp0_lat = fltarr(Nlegs)-555.
Rp0_lon = fltarr(Nlegs)-555.

 Rp_rad = fltarr(Nlegs)-555.
 Rp_lat = fltarr(Nlegs)-555.
 Rp_lon = fltarr(Nlegs)-555.
;=======================================

; This array will code the "STATUS of LEG" so that:
; leg_status=1 if leg contains the starting point used for the loop,
; leg_status=2 if not.
  leg_status = fltarr(Nlegs) + 1. 

; Initialize ileg index:
  ileg = 0L


; Define minimum number of data points required by leg 
  Ndata=5

; Start analysis of each loop
  for il=0L,Nloop-1 do begin

;stop

; Analysis for OPEN loops:
  if opcls(il) eq 0. then begin

     Ne_l = reform ( Ne_v(0:Npts_v(il)-1,il))
     Tm_l = reform ( Tm_v(0:Npts_v(il)-1,il))
     WT_l = reform ( WT_v(0:Npts_v(il)-1,il))
     Er_l = reform ( Er_v(0:Npts_v(il)-1,il))
 scoreR_l = reform ( scoreR_v(0:Npts_v(il)-1,il)) 
    rad_l = reform (rad_v(0:Npts_v(il)-1,il))
    lat_l = reform (lat_v(0:Npts_v(il)-1,il))
    lon_l = reform (lon_v(0:Npts_v(il)-1,il))
      s_l = reform (  s_v(0:Npts_v(il)-1,il))   
      B_l = reform (  B_v(0:Npts_v(il)-1,il))   
     Br_l = reform ( Br_v(0:Npts_v(il)-1,il))   

;if lat_v(0,il) gt -75 then stop
 
    rad_ini(ileg) = rad_l(0)
    lat_ini(ileg) = lat_l(0)
    lon_ini(ileg) = lon_l(0)
        Br0(ileg) =  Br_l(0)
;----------------------------------AGREGADO----------------
    rad_fin(ileg) = enrad_v(il)
    lat_fin(ileg) = enlat_v(il) 
    lon_fin(ileg) = enlon_v(il)
;------------------------------------------------------------ 
    footrad(ileg) = rad_ini(ileg)
    footlat(ileg) = lat_ini(ileg)
    footlon(ileg) = lon_ini(ileg)
;------------------------------------------------------------   

    rrr=findel(1.075,rad_l)
    Rp0_rad(ileg) = rad_l(rrr)
    Rp0_lat(ileg) = lat_l(rrr)
    Rp0_lon(ileg) = lon_l(rrr)

     Rp_rad(ileg) = Rp0_rad(ileg)
     Rp_lat(ileg) = Rp0_lat(ileg)
     Rp_lon(ileg) = Rp0_lon(ileg)

      ;Select useful data 

     p = where ( rad_l ge rmin and rad_l le rmax and Ne_l ne -999. and scoreR_l lt 0.10)  

    if p(0) eq -1 then goto,skipnextloop_open

     Ne_l =  Ne_l (p)
     Tm_l =  Tm_l (p)
     WT_l =  WT_l (p)
     Er_l =  Er_l (p) 
    rad_l = rad_l (p)
    lat_l = lat_l (p)
    lon_l = lon_l (p)
      s_l =   s_l (p)
      B_l =   B_l (p)    
    ;make tomographic pressure
    p_l   = kB/bb *Ne_l*Tm_l 
    ;make Beta plasma parameter
   beta_l = p_l/(B_l^2/(8*!pi))

    rr1=findel(1.075,rad_l)     ;=======================================
        TmR1(ileg) = Tm_l(rr1)  
         NR1(ileg) = Ne_l(rr1)  

      Nemean(ileg) =   mean(Ne_l)
      Tmmean(ileg) =   mean(Tm_l)
      WTmean(ileg) =   mean(WT_l)
    Nestddev(ileg) = stddev(Ne_l)
    Tmstddev(ileg) = stddev(Tm_l)
    WTstddev(ileg) = stddev(WT_l)
    betamean(ileg) =   mean(beta_l)
       Bmean(ileg) =   mean(B_l)

    if n_elements(p) lt Ndata then goto,skipfitloop_open

;========VER TERCIOS
    rrr1 = 1.07
    rrr2 = 1.12
;    if min(rad_l) gt rrr1 OR median(rad_l) lt rrr1 OR median(rad_l) gt rrr2 OR max(rad_l) lt rrr2 then goto,skipfitloop_open
;if min(rad_l) gt rr1 or max(rad_l) lt rrr3 then
    lefts  = where(rad_l le rrr1) ;n_elements(where(rad_l le rrr1))
    rights = where(rad_l ge rrr2) ;n_elements(where(rad_l ge rrr2))
    diomes = where(rad_l gt rrr1 and rad_l lt rrr2) ;n_elements(where(rad_l gt rrr1 and rad_l lt rrr2))
;   if lefts(0) eq -1 or diomes(0) eq -1 or rights(0) eq -1 then goto,skipfitloop_open 
    if lefts(0) eq -1 or rights(0) eq -1 then goto,skipfitloop_open 

;------------- eps por REGIONES

if opcls(il) eq 0. then eps  = 5.6*1.e4
if opcls(il) eq 0. then epsN = 2.6*1.e6

   ;Make HS-fit to Ne(r) for each open leg/loop
      xfit = rad_l
      yfit =  Ne_l
    rminhs = min(rad_l);rmin
    rmaxhs = max(rad_l);rmax 

if n_elements(xfit) ne n_elements(yfit) then stop

        if      keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,epsN,A,corr2
        if not  keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2
    
         Ne0(ileg) = A[0] 
    lambda_N(ileg) = 1./A[1]  ; Rsun
              Tfit = mu * mH * gsun * (lambda_N(ileg)*rsun) / kB
       Tefit(ileg) = bb*Tfit
         r2N(ileg) = corr2


    ;Make HS-fit to p(r) for each open leg/loop    
      yfit = p_l
   
        if      keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,epsN,A,corr2 ;acá serìa otro el eps
        if not  keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2

          p0(ileg) = a[0] 
    lambda_p(ileg) = 1./A[1]  ; Rsun
         r2P(ileg) = corr2

   ;Make LINEAR-fit to T(r) for each open leg/loop
           yfit = Tm_l
;            ep = 0.15
 
if n_elements(xfit) ne n_elements(yfit) then stop

    fit_ts,xfit,yfit,eps,a1,b1,F
;    fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2
;           T0r = A[0]
;      Tm0(ileg) = T0r
;    gradT(ileg) = A[1]
;      r2T(ileg) = corr2 

           T0r_ts = b1
     Tm0_ts(ileg) = T0r_ts
   gradT_ts(ileg) = a1
     FTr_ts(ileg) = F 

    ;Make LINEAR-fit to T(s) for each open leg/loop ======================
    
           xfit = s_l*rsun    
         rminhs = min(s_l*rsun);smin
         rmaxhs = max(s_l*rsun);smax 
           yfit = Tm_l
;            ep = 0.15
 
    fit_ts,xfit,yfit,eps,a1,b1,F
 ;   fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2
 ;           T0s = A[0] ; K
 ;    Tm0s(ileg) = T0s  
 ;   dTmds(ileg) = A[1] ; K/cm
 ;    r2Ts(ileg) = corr2 

            T0s_ts = b1 ; K
     Tm0s_ts(ileg) = T0s_ts  
    dTmds_ts(ileg) = a1 ; K/cm
      FTs_ts(ileg) = F 

      eplegT(ileg) = eps
      deltaS(ileg) = abs(max(xfit)-min(xfit))


;==================================================================================================
;================================= TESTEO TS PRIMEROS PUNTOS ======================================

;           nump = 4

if n_elements(s_l) ge 10 then begin
     nump = floor((n_elements(s_l))*0.5)
     endif else begin 
if n_elements(s_l) lt 10 then nump = 4
     endelse

           xfit = (s_l(0:nump-1))*rsun    
         rminhs = min((s_l(0:nump-1))*rsun);smin
         rmaxhs = max((s_l(0:nump-1))*rsun);smax 
           yfit = Tm_l(0:nump-1)
 
    fit_ts,xfit,yfit,eps,a1,b1,F

            T0s5_ts = b1
     Tm0s5_ts(ileg) = T0s5_ts
    dTmds5_ts(ileg) = a1
      FTs5_ts(ileg) = F 

;==================================================================================================
;==================================================================================================
;============================================ AJUSTE Er PRIMEROS PUNTOS LINEAL l
;===============================================================================

;ajusto los primeros nErlin puntos de Er con una lineal para extrapolar el punto en la base

    r0 = 1.025

  xfit = rad_l[0:3]
  yfit = Er_l[0:3]

;stop
epsEr= 0.1*Er_l[0]    ;<============================================== VER ERRORES Er

fit_ts,xfit,yfit,epsEr,a1,b1,F
   Erlin_F(ileg) = F    ;calidad de ajuste
   Erlin_d(ileg) = a1   ;pendiente
   Erlin_o(ileg) = b1   ;ordenada

     Erlin_l_r0 = a1*r0+b1   ;punto en la base
 Erlin_r0(ileg) = Erlin_l_r0
;=============================================================================

      
;   ->STOP
;   if  r2N(ileg) lt 0.5  then  r2N(ileg) = -555.
;   if  r2P(ileg) lt 0.5  then  r2P(ileg) = -555.
    if  r2T(ileg) lt 0.5  then  r2T(ileg) = -555.
    if r2Ts(ileg) lt 0.5  then r2Ts(ileg) = -555.
    if r2Ts(ileg) lt 0.5  then goto,skiptestloopopen

      s_l = s_l * rsun ; pass s_l to cm 

; make dT/ds(s), asumiendo que ese gradiente es constante: 
  dTmds_l = dTmds (ileg)  ; DERIV(s_l,Tm_l) esto permite que el gradiente varie, pero es ruidoso.

; make conductive flux 

     ;Fc_l= -kappa*Tm_l^(5./2)*dTmds_l
      Fc_l = -kappa*(T0s +dTmds_l*s_l)^(5./2)*dTmds_l 

; make conductive loss  ;======================================================
     Ec_l = B_l * DERIV(s_l,Fc_l/B_l)   
; make heating rate  =============== 
     Eh_l = Er_l + Ec_l   
      s_l = s_l / rsun ; back s_l to Rsun 

; test diferent models of heating rate    
   Eh(ileg)= mean(Eh_l)
     index = where( Eh_l gt 0) 
   
   if n_elements(index) lt n_elements(Eh_l) then Eh(ileg) = -555.
      
   deltaEh(ileg) = stdev(Eh_l)
            smin = min(s_l)
            smax = max(s_l)
   fitEh,s_l,Eh_l,smin,smax,A,r2

       sH (ileg) = A[1]
      r2sH(ileg) = r2

;..........................................
goto,skiptestloopopen
wn=0
 window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 X=1.+.25*findgen(100)/99.

 plot,rad_l,ne_l/1.e8,psym=4,title='Ne'
 oplot,X,Ne0(ileg)* exp( -(1/lambda_N(ileg))*(1.-1./X) ) /1.e8
 xyouts,0.5*[1,1,1],1.-[.1,.15,.2],[string(Ne0(ileg)/1.e8),string(lambda_N(ileg)),string(r2n(ileg))],/normal

 plot,rad_l,p_l,psym=4,title='P'
 oplot,X,p0(ileg)* exp( -(1/lambda_p(ileg))*(1.-1./X) )
 xyouts,0.5*[1,1,1],(2./3)*1.-[.1,.15,.2],[string(P0(ileg)),string(lambda_p(ileg)),string(r2p(ileg))],/normal

  plot,rad_l,tm_l/1.e6,psym=4,title='Tm'
  oplot,X,(T0r+gradT(ileg)*X)/1.e6
  xyouts,0.5*[1,1,1],(1./3)*1.-[.15,.2,.25],[string(T0r/1.e6),string(gradT(ileg)/1.e6),string(r2t(ileg))],/normal

wn=1

 window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1

  X=1.+.25*findgen(100)/99. 
  
  plot,rad_l,tm_l/1.e6,psym=4,title='Tm(r)'
  oplot,X,(T0r+gradT(ileg)*X)/1.e6
  xyouts,0.5*[1,1,1],1.-[.1,.15,.2],[string(T0r/1.e6),string(gradT(ileg)/1.e6),string(r2t(ileg))],/normal

  x = min(s_l) + (max(s_l)-min(s_l))*findgen(100)/99
  
  plot,s_l,tm_l/1.e6,psym=4,title='Tm(s)'
  oplot,X,(T0s+dTmds(ileg)*rsun*X)/1.e6
  xyouts,0.5*[1,1,1],(2./3)*1.-[.1,.15,.2],[string(T0s/1.e6),string(dTmds(ileg)/1.e6*rsun),string(r2Ts(ileg))],/normal
  
  plot,s_l,Fc_l,psym=4,title='Fc'

wn=2

 window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=3
 !p.charthick=1
 
 plot,rad_l,Er_l,psym=4,title='radiative loss'
 plot,rad_l,Ec_l,psym=4,title='conductive loss'
 plot,rad_l,Eh_l,psym=4,title='heating rate'
 
stop
skiptestloopopen:
;...........................................

   skipfitloop_open:
       opclstat(ileg) = opcls(il)   
    loop_length(ileg) = loopL(il)
          strad(ileg) = str_v(il)
          stlat(ileg) = (90-stth_v(il)/!dtor)
          stlon(ileg) = stph_v(il)/!dtor
   skipnextloop_open:
   indexloop(ileg) = il 
              ileg = ileg+1

 endif else begin 

; Analysis closed loops:
  if max(rad_v(0:Npts_v(il)-1,il)) lt rminloop then goto,skipnextloop

;<---------------------------------------                     
  if opcls(il) eq 2 then begin
      ifirs_1 = 0
      ilast_1 = midcell_v(il)
      ifirs_2 = midcell_v(il)
      ilast_2 = Npts_v(il)-1
  endif
  if opcls(il) eq 1 then begin
      ifirs_1 = 0
      ilast_1 = midcell_v(il)-1
      while Ne_v(ilast_1,il) eq -666. do ilast_1=ilast_1-1
      ifirs_2 = midcell_v(il)
      ilast_2 = Npts_v(il)-1
  endif
 ;<-----------------------------------------
 
  Ne_l1 = reform ( Ne_v(ifirs_1:ilast_1,il))
  Ne_l2 = reform ( Ne_v(ifirs_2:ilast_2,il))
  Tm_l1 = reform ( Tm_v(ifirs_1:ilast_1,il))
  Tm_l2 = reform ( Tm_v(ifirs_2:ilast_2,il))
  WT_l1 = reform ( WT_v(ifirs_1:ilast_1,il))
  WT_l2 = reform ( WT_v(ifirs_2:ilast_2,il))
  Er_l1 = reform ( Er_v(ifirs_1:ilast_1,il))
  Er_l2 = reform ( Er_v(ifirs_2:ilast_2,il))
  scoreR_l1 = reform ( scoreR_v(ifirs_1:ilast_1,il))
  scoreR_l2 = reform ( scoreR_v(ifirs_2:ilast_2,il))
  rad_l1 = reform( rad_v(ifirs_1:ilast_1,il))
  rad_l2 = reform( rad_v(ifirs_2:ilast_2,il))
  lat_l1 = reform( lat_v(ifirs_1:ilast_1,il))
  lat_l2 = reform( lat_v(ifirs_2:ilast_2,il))
  lon_l1 = reform( lon_v(ifirs_1:ilast_1,il))
  lon_l2 = reform( lon_v(ifirs_2:ilast_2,il))
    s_l1 = reform(   s_v(ifirs_1:ilast_1,il))
    s_l2 = loopL(il) - reform(   s_v(ifirs_2:ilast_2,il))
    B_l1 = reform(   B_v(ifirs_1:ilast_1,il))
    B_l2 = reform(   B_v(ifirs_2:ilast_2,il))

  switching = 'no'
; Make "ileg"   the LEG that contains the starting point, and "ileg+1" the other one:
 ;switch_legs,rad_l1,rad_l2,lat_l1,lat_l2,lon_l1,lon_l2,Ne_l1,Ne_l2,Tm_l1,Tm_l2,WT_l1,WT_l2,scoreR_l1,scoreR_l2,il,switching 
; Make leg_status=2 for ileg+1 (for ileg we already set leg_status=1, by default)
  leg_status(ileg+1) = 2.
; IMPORTANT NOTE: leg_status contains now the correct information ONLY
; if switch_legs was used. If not, this information may be incorrect
; but it will NOT be used anyhow.
; ALBERT'S ADVICE: please *ALWAYS* USE SWITCHING!! It is what you want anyway.

; According to the switching status, associate to each LEG the 
; INITIAL and FINAL coordinates of the LOOP wo which BOTH belong:
  if switching eq 'no' then begin
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
  endif
  if switching eq 'yes' then begin
  rad_ini(ileg)     = rad_l1(n_elements(rad_l1)-1)
  rad_iOAni(ileg+1) = rad_l1(n_elements(rad_l1)-1)
  rad_fin(ileg)   = rad_l2(0)
  rad_fin(ileg+1) = rad_l2(0)
  lat_ini(ileg)   = lat_l1(n_elements(lat_l1)-1)
  lat_ini(ileg+1) = lat_l1(n_elements(lat_l1)-1)
  lat_fin(ileg)   = lat_l2(0)
  lat_fin(ileg+1) = lat_l2(0)
  lon_ini(ileg)   = lon_l1(n_elements(lon_l1)-1)
  lon_ini(ileg+1) = lon_l1(n_elements(lon_l1)-1)
  lon_fin(ileg)   = lon_l2(0)
  lon_fin(ileg+1) = lon_l2(0)
  endif

; These lines store the FOOT-POINT coordinates for each LEG,
; and they do it correctly independently of the switching status:
  Footrad(ileg)   = rad_ini(ileg)    
  Footrad(ileg+1) = rad_fin(ileg+1)  
  Footlat(ileg)   = lat_ini(ileg)    
  Footlat(ileg+1) = lat_fin(ileg+1)  
  Footlon(ileg)   = lon_ini(ileg)    
  Footlon(ileg+1) = lon_fin(ileg+1)  

   rrr1=findel(1.075,rad_l1)
   rrr2=findel(1.075,rad_l2)
  Rp0_rad(ileg)   = rad_l1(rrr1)
  Rp0_rad(ileg+1) = rad_l2(rrr2)
  Rp0_lat(ileg)   = lat_l1(rrr1)
  Rp0_lat(ileg+1) = lat_l2(rrr2)
  Rp0_lon(ileg)   = lon_l1(rrr1)
  Rp0_lon(ileg+1) = lon_l2(rrr2)
 
   Rp_rad(ileg)   = Rp0_rad(ileg)
   Rp_rad(ileg+1) = Rp0_rad(ileg+1)
   Rp_lat(ileg)   = Rp0_lat(ileg)
   Rp_lat(ileg+1) = Rp0_lat(ileg+1)
   Rp_lon(ileg)   = Rp0_lon(ileg)
   Rp_lon(ileg+1) = Rp0_lon(ileg+1)


  p1 = where ( rad_l1 ge rmin and rad_l1 le rmax and Ne_l1 ne -999. and scoreR_l1 lt 0.1 and WT_l1 ge WTc*1.e6)
  p2 = where ( rad_l2 ge rmin and rad_l2 le rmax and Ne_l2 ne -999. and scoreR_l2 lt 0.1 and WT_l2 ge WTc*1.e6)

  if  p1(0) eq -1 or p2(0) eq -1 then goto,skipnextloop

   rr11=findel(1.075,rad_l1)
   rr12=findel(1.075,rad_l2)
     TmR1(ileg)   = Tm_l1(rr11)
     TmR1(ileg+1) = Tm_l2(rr12)

      NR1(ileg)   = Ne_l1(rr11)
      NR1(ileg+1) = Ne_l2(rr12)

;----------------------------------------------------------------------------------------------------------------
  r0 = 1.025
  s_l1_max = max(s_l1) ; Rsun
  s_l2_max = max(s_l2) ; Rsun
  r_l1_max = max(rad_l1) ; Rsun
  r_l2_max = max(rad_l2) ; Rsun

min_s1 = s_l1(findel(r0,rad_l1))
if max(rad_l1) le 1.2 then begin
max_s1 = s_l1(where(rad_l1 eq max(rad_l1)))
endif else begin
max_s1 = s_l1(findel(1.2,rad_l1))
endelse

min_s2 = s_l2(findel(r0,rad_l2))
if max(rad_l2) le 1.2 then begin
max_s2 = s_l2(where(rad_l2 eq max(rad_l2)))
endif else begin
max_s2 = s_l2(findel(1.2,rad_l2))
endelse

  Smaxxx(ileg  ) = max_s1 ;s_l1_max
  Smaxxx(ileg+1) = max_s2 ;s_l2_max
  Sminnn(ileg  ) = min_s1
  Sminnn(ileg+1) = min_s2

;stop

  Nl1 = n_elements(B_l1)
  Nl2 = n_elements(B_l2)
  B_l1_max = B_l1(Nl1-1)
  B_l2_max = B_l2(0    )
  B_l1_r0  = interpol(B_l1,rad_l1,r0)
  B_l2_r0  = interpol(B_l2,rad_l2,r0)

B_base(ileg)   = B_l1_r0
B_base(ileg+1) = B_l2_r0
;stop

  Ner0_l1  = interpol(Ne_l1,rad_l1,r0)
  Ner0_l2  = interpol(Ne_l2,rad_l2,r0)

  Ner0(ileg)   = Ner0_l1
  Ner0(ileg+1) = Ner0_l2

;======================================================================================================
  Ner1_l1  = -555
  Ner1_l2  = -555
  Ner1(ileg)   = Ner1_l1
  Ner1(ileg+1) = Ner1_l2

  Ner2_l1  = -555
  Ner2_l2  = -555
  Ner2(ileg)   = Ner2_l1
  Ner2(ileg+1) = Ner2_l2

  Ner3_l1  = -555
  Ner3_l2  = -555
  Ner3(ileg)   = Ner3_l1
  Ner3(ileg+1) = Ner3_l2
;======================================================================================================

 ; Fit lineal s_l1 = m1 rad_l1 + b1, usar primeros 5 datos.
 ; Fit lineal s_l2 = m2 rad_l2 + b2
   Npoints=Ndata
  if n_elements(rad_l1) ge Npoints AND n_elements(rad_l2) ge Npoints then begin 

   rad_l1_base = rad_l1(0:Npoints-1) 
   rad_l2_rev  = reverse(rad_l2)  ; da vuelta rad_l2 
   rad_l2_base = rad_l2_rev (0:Npoints-1)

     s_l1_base =   s_l1(0:Npoints-1) 
     s_l2_rev  =   reverse(s_l2)     ; también necesito los últimos s_l2
     s_l2_base =   s_l2_rev(0:Npoints-1) 

;fit s_l1_base = m1 * rad_l1_base + s0r1 
  xfit = rad_l1_base
  yfit =   s_l1_base
  fitr0,xfit,yfit,A,corr2
         S0r1 = A[0]
         m1   = A[1]
         r2s1 = corr2

;fit s_l2_base = m2 * rad_l2_base + s0r2
  xfit = rad_l2_base
  yfit =   s_l2_base
  fitr0,xfit,yfit,A,corr2
         S0r2 = A[0]
         m2   = A[1]
         r2s2 = corr2

    s_l1_r0 = m1 * r0 + s0r1 
    s_l2_r0 = m2 * r0 + s0r2

  s_r0_a(ileg  ) = s_l1_r0 
  s_r0_a(ileg+1) = s_l2_r0 

  endif
 
;----------------------------------------------------------------------------------------------------------------
;STOP
  
   Ne_l1 =  Ne_l1 (p1)
   Ne_l2 =  Ne_l2 (p2)
   Tm_l1 =  Tm_l1 (p1)
   Tm_l2 =  Tm_l2 (p2)
   WT_l1 =  WT_l1 (p1)
   WT_l2 =  WT_l2 (p2)
   Er_l1 =  Er_l1 (p1)
   Er_l2 =  Er_l2 (p2)
  rad_l1 = rad_l1 (p1)
  rad_l2 = rad_l2 (p2)
    s_l1 =   s_l1 (p1)
    s_l2 =   s_l2 (p2)
    B_l1 =   B_l1 (p1)
    B_l2 =   B_l2 (p2)
;tomographic pressure      
  p_l1 = kB/bb *Ne_l1*Tm_l1   
  p_l2 = kB/bb *Ne_l2*Tm_l2   
;make Beta plasma parameter
 beta_l1 = p_l1/(B_l1^2/(8*!pi))
 beta_l2 = p_l2/(B_l2^2/(8*!pi))
 
    Nemean(ileg)   = mean(Ne_l1)
    Nemean(ileg+1) = mean(Ne_l2)
    Tmmean(ileg)   = mean(Tm_l1)
    Tmmean(ileg+1) = mean(Tm_l2)
    WTmean(ileg)   = mean(WT_l1)
    WTmean(ileg+1) = mean(WT_l2)
  Nestddev(ileg)   = stddev(Ne_l1)
  Nestddev(ileg+1) = stddev(Ne_l2)
  Tmstddev(ileg)   = stddev(Tm_l1)
  Tmstddev(ileg+1) = stddev(Tm_l2)
  WTstddev(ileg)   = stddev(WT_l1)
  WTstddev(ileg+1) = stddev(WT_l2)
  betamean(ileg)   = mean(beta_l1)
  betamean(ileg+1) = mean(beta_l2)
  betaapex(ileg)   = beta_l1(n_elements(rad_l1(p1))-1)
  betaapex(ileg+1) = beta_l2(0)
     Bmean(ileg)   = mean(B_l1)
     Bmean(ileg+1) = mean(B_l2)
;if you want to use beta at some height instead <beta> uncomment the
;following lines
;...........................................................
rad0= 1.035
index_l1 = where (abs(rad_l1-rad0) eq min(abs(rad_l1-rad0)))
index_l2 = where (abs(rad_l2-rad0) eq min(abs(rad_l2-rad0)))
;betamean(ileg)   = beta_l1(index_l1(0))
;betamean(ileg+1) = beta_l2(index_l2(0))
;.......................................................... 

if n_elements(p1) lt Ndata or n_elements(p2) lt Ndata then goto,skipfitloop

r_max1 = max(rad_l1)
r_max2 = max(rad_l2)


if r_max1 ge 1.2 then begin  ;igual que las piernas abiertas 
   rrr1_l1 = 1.07   ;(max(rad_l1)-1.)*0.25 +1.0  
   rrr2_l1 = 1.12   ;(max(rad_l1)-1.)*0.5 +1.0                                            
    lefts_l1  = where(rad_l1 le rrr1_l1) ;n_elements(where(rad_l1 le rrr1_l1))
    rights_l1 = where(rad_l1 ge rrr2_l1) ;n_elements(where(rad_l1 ge rrr2_l1))
    diomes_l1 = where(rad_l1 gt rrr1_l1 and rad_l1 lt rrr2_l1) ;n_elements(where(rad_l1 gt rrr1_l1 and rad_l1 le rrr2_l1))
 endif else begin

rrr_min = 1.035
    rrr_max = max(rad_l1)
    Drr     = rrr_max - rrr_min
    rr1_l1  = rrr_min + Drr * 1./3.
    rr2_l1  = rrr_min + Drr * 2./3.
    lefts_l1  = (where(rad_l1 le rr1_l1))
    rights_l1 = (where(rad_l1 ge rr2_l1))
    diomes_l1 = (where(rad_l1 gt rr1_l1 and rad_l1 lt rr2_l1))

;    rrr1_l1 = 1.07
;    rrr2_l1 = (max(rad_l1)-1.)*0.75 +1.0
;    lefts_l1  = where(rad_l1 le rrr1_l1) ;n_elements(where(rad_l1 le rrr1_l1))
;    rights_l1 = 1.  ;n_elements(where(rad_l1 ge rrr2_l1))
;    diomes_l1 = where(rad_l1 gt rrr1_l1 and rad_l1 lt rrr2_l1) ;n_elements(where(rad_l1 gt rrr1_l1 and rad_l1 le rrr2_l1))
 endelse

if r_max2 ge 1.2 then begin
    rrr1_l2 = 1.07   ;(max(rad_l2)-1.)*0.25 +1.0 
    rrr2_l2 = 1.12   ;(max(rad_l2)-1.)*0.5 +1.0  
    lefts_l2  = where(rad_l2 le rrr1_l2) ;n_elements(where(rad_l2 le rrr1_l2))
    rights_l2 = where(rad_l2 ge rrr2_l2) ;n_elements(where(rad_l2 ge rrr2_l2))
    diomes_l2 = where(rad_l2 gt rrr1_l2 and rad_l2 lt rrr2_l2) ;n_elements(where(rad_l2 gt rrr1_l2 and rad_l2 le rrr2_l2))
 endif else begin

    rrr_min = 1.035
    rrr_max = max(rad_l2)
    Drr     = rrr_max - rrr_min
    rr1_l2  = rrr_min + Drr * 1./3.
    rr2_l2  = rrr_min + Drr * 2./3.
    lefts_l2  = (where(rad_l2 le rr1_l2))
    rights_l2 = (where(rad_l2 ge rr2_l2))
    diomes_l2 = (where(rad_l2 gt rr1_l2 and rad_l2 lt rr2_l2))

;    rrr1_l2 = 1.07
;    rrr2_l2 = (max(rad_l2)-1.)*0.75 +1.0
;    lefts_l2  = where(rad_l2 le rrr1_l2) ;n_elements(where(rad_l2 le rrr1_l2))
;    rights_l2 = 1.   ;n_elements(where(rad_l2 ge rrr2_l2))  
;    diomes_l2 = where(rad_l2 gt rrr1_l2 and rad_l2 lt rrr2_l2) ;n_elements(where(rad_l2 gt rrr1_l2 and rad_l2 le rrr2_l2))
 endelse

;if (lefts_l2(0) eq -1) or (diomes_l2(0) eq -1) or (rights_l2(0) eq -1) then stop
;if (lefts_l1(0) eq -1) or (diomes_l1(0) eq -1) or (rights_l1(0) eq -1) then stop

;if (diomes_l2(0) eq -1) then stop

if lefts_l1(0) eq -1 or diomes_l1(0) eq -1 or rights_l1(0) eq -1 then goto,skipfitloop
if lefts_l2(0) eq -1 or diomes_l2(0) eq -1 or rights_l2(0) eq -1 then goto,skipfitloop

;if lefts_l1(0) eq -1 or rights_l1(0) eq -1 then goto,skipfitloop
;if lefts_l2(0) eq -1 or rights_l2(0) eq -1 then goto,skipfitloop

;rrr1 = 1.10
;if min(rad_l1) gt rrr1 OR min(rad_l2) gt rrr1 then goto,skipfitloop

;-------------------ep por REGIONES
st_lb_ne= 3.8* 1.e6
st_li_ne= 2.9* 1.e6
st_f_ne = 2.7* 1.e6
ch_lb_ne= 3.0* 1.e6
ch_la_ne= 2.2* 1.e6

st_lb_t = 6.4* 1.e4
st_li_t = 5.1* 1.e4
st_f_t  = 5.4* 1.e4
ch_lb_t = 6.4* 1.e4
ch_la_t = 4.8* 1.e4

if opcls(il) eq 1. then begin
 eps1  = 5.4*1.e4 
 eps2  = 5.4*1.e4 
 epsN1 = 2.7*1.e6 
 epsN2 = 2.7*1.e6 
 eplegT(ileg)   = eps1
 eplegT(ileg+1) = eps2
endif


if opcls(il) eq 2. then begin

    if abs(footlat(ileg)) le 30. then begin
       eps1  = 6.4*1.e4
       epsN1 = 3.8*1.e6
       eplegT(ileg) = eps1
    endif
    if abs(footlat(ileg)) gt 30. then begin
       eps1  = 5.1*1.e4
       epsN1 = 2.9*1.e6
       eplegT(ileg) = eps1
    endif

    if abs(footlat(ileg+1)) le 30. then begin
       eps2  = 6.4*1.e4
       epsN2 = 3.8*1.e6
       eplegT(ileg+1) = eps2
    endif
    if abs(footlat(ileg+1)) gt 30. then begin
       eps2  = 5.1*1.e4
       epsN2 = 2.9*1.e6
       eplegT(ileg+1) = eps2
    endif

endif

;fits for leg1-------------------------------------- 
  rminhs = min(rad_l1) 
  rmaxhs = max(rad_l1)

if opcls(il) eq 1 and max(rad_v(*,il)) lt 1.225 then print,il

;HS-fit to Ne(r)
  xfit = rad_l1
  yfit =  Ne_l1
if n_elements(xfit) ne n_elements(yfit) then stop

    if     keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,epsN1,A,corr2
    if not keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2

       Ne0(ileg) = a[0] 
  lambda_N(ileg) = 1./A[1]  ; Rsun
            Tfit = mu * mH * gsun * (lambda_N(ileg)*rsun) / kB
     Tefit(ileg) = bb*Tfit
     r2N  (ileg) = corr2

  ;HS-fit to P(r)
  yfit = p_l1
  
    if     keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,epsN1,A,corr2 ;idem antes
    if not keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2

        P0(ileg) = a[0] 
  lambda_P(ileg) = 1./A[1]  ; Rsun
       r2P(ileg) = corr2

  ;linear-fit to T(r)
  yfit = Tm_l1
;    ep = 0.15
if n_elements(xfit) ne n_elements(yfit) then stop

    fit_ts,xfit,yfit,eps1,a1,b1,F
;  fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

          T0r1_ts = b1
     Tm0_ts(ileg) = T0r1_ts
   gradT_ts(ileg) = a1
     FTr_ts(ileg) = F 

;         T0r1 = A[0]
;    Tm0(ileg) = T0r1
;  gradT(ileg) = A[1]
;  r2T  (ileg) = corr2

 ; linear-fit to T(s) ==================================
      xfit = s_l1 *rsun    
    rminhs = min(s_l1*rsun);smin
    rmaxhs = max(s_l1*rsun);smax 
      yfit = Tm_l1
;        ep = 0.15

    fit_ts,xfit,yfit,eps1,a1,b1,F
;   fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

          T0s1_ts = b1
    Tm0s_ts(ileg) = T0s1_ts
   dTmds_ts(ileg) = a1
     FTs_ts(ileg) = F 

      deltaS(ileg) = abs(max(xfit)-min(xfit))

;            T0s1 = A[0]
;      Tm0s(ileg) = T0s1
;    dTmds (ileg) = A[1]
;    r2Ts  (ileg) = corr2

;==================================================================================================
;================================= TESTEO TS PRIMEROS PUNTOS ======================================

;          nump1 = 4
;stop
if n_elements(s_l1) ge 10 then begin
     nump1 = floor((n_elements(s_l1))*0.5)
     endif else begin 
if n_elements(s_l1) lt 10 then nump1 = 4
     endelse
;stop
           xfit = (s_l1(0:nump1-1))*rsun    
         rminhs = min((s_l1(0:nump1-1))*rsun);smin
         rmaxhs = max((s_l1(0:nump1-1))*rsun);smax 
           yfit = Tm_l1(0:nump1-1)
 
    fit_ts,xfit,yfit,eps,a1,b1,F

          T0s15_ts = b1
    Tm0s5_ts(ileg) = T0s15_ts
   dTmds5_ts(ileg) = a1
     FTs5_ts(ileg) = F 

;==================================================================================================
;==================================================================================================

   if keyword_set(fitcuadr) then begin
  ; cuadratic-fit to T(s) 
      xfit = s_l1 ;*rsun    
    rminhs = min(s_l1);*rsun);smin
    rmaxhs = max(s_l1);*rsun);smax 
    yfit   = Tm_l1
   fitcuadrTemp,xfit,yfit,rminhs,rmaxhs,A,corr2,T0s1,dTmds(ileg)*rsun
        Acuadr1 = A
 r2Tcuadr(ileg) = corr2
 Acuadr_a(ileg,*) = Acuadr1
  endif

;fits for leg2-------------------------------------- 
  rminhs = min(rad_l2) 
  rmaxhs = max(rad_l2)

  ;HS-fit to Ne(r)
  xfit = rad_l2
  yfit =  Ne_l2

if n_elements(xfit) ne n_elements(yfit) then stop

    if     keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,epsN2,A,corr2
    if not keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2  

       Ne0(ileg+1) = a[0] 
  lambda_N(ileg+1) = 1./A[1]  ; Rsun
              Tfit = mu * mH * gsun * (lambda_N(ileg+1)*rsun) / kB
     Tefit(ileg+1) = bb*Tfit
     r2N  (ileg+1) = corr2  

  ;HS-fit to P(r)
  yfit = p_l2
    if     keyword_set(linear) then fithslinear,xfit,yfit,rminhs,rmaxhs,epsN2,A,corr2
    if not keyword_set(linear) then fiths,xfit,yfit,rminhs,rmaxhs,A,corr2  
        P0(ileg+1) = a[0] 
  lambda_P(ileg+1) = 1./A[1]  ; Rsun
     r2P  (ileg+1) = corr2

  ;HS-fit to T(r)  
  yfit = Tm_l2
;    ep = 0.15
if n_elements(xfit) ne n_elements(yfit) then stop

    fit_ts,xfit,yfit,eps2,a1,b1,F
;   fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

            T0r2_ts = b1
     Tm0_ts(ileg+1) = T0r2_ts
   gradT_ts(ileg+1) = a1
     FTr_ts(ileg+1) = F 
 
;           T0r2 = A[0]
;    Tm0(ileg+1) = T0r2
;  gradT(ileg+1) = A[1]
;  r2T  (ileg+1) = corr2

  ;linear-fit to T(s) ==========================================
      xfit = s_l2 *rsun   
    rminhs = min(s_l2*rsun);smin
    rmaxhs = max(s_l2*rsun);smax 
      yfit = Tm_l2
;        ep = 0.15

    fit_ts,xfit,yfit,eps2,a1,b1,F
;  fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2 

            T0s2_ts = b1
    Tm0s_ts(ileg+1) = T0s2_ts
   dTmds_ts(ileg+1) = a1
     FTs_ts(ileg+1) = F 

;              T0s2 = A[0]
;      Tm0s(ileg+1) = T0s2  ;<---
;    dTmds (ileg+1) = A[1]
;    r2Ts  (ileg+1) = corr2  

;==================================================================================================
;================================= TESTEO TS PRIMEROS PUNTOS ======================================

;           nump2 = 4

if n_elements(s_l2) ge 10 then begin
     nump2 = floor((n_elements(s_l2))*0.5)
     endif else begin 
if n_elements(s_l2) lt 10 then nump2 = 4
     endelse

           xfit = (s_l2(0:nump2-1))*rsun    
         rminhs = min((s_l2(0:nump2-1))*rsun);smin
         rmaxhs = max((s_l2(0:nump2-1))*rsun);smax 
           yfit = Tm_l2(0:nump2-1)
 
    fit_ts,xfit,yfit,eps,a1,b1,F

            T0s25_ts = b1
    Tm0s5_ts(ileg+1) = T0s25_ts
   dTmds5_ts(ileg+1) = a1
     FTs5_ts(ileg+1) = F 

;==================================================================================================
;==================================================================================================

 if keyword_set(fitcuadr) then begin
   ;cuadratic-fit to T(s) 
      xfit = s_l2 ;*rsun    
    rminhs = min(s_l2);*rsun;smin
    rmaxhs = max(s_l2);*rsun;smax 
    yfit   = Tm_l2
    fitcuadrTemp,xfit,yfit,rminhs,rmaxhs,A,corr2,T0s2,dTmds(ileg+1)
         Acuadr2 = A
r2Tcuadr(ileg+1) = corr2
Acuadr_a(ileg+1,*) = Acuadr2
 endif
 
;STOP

goto,nocambiarlosr2
   if r2T(ileg) lt 0.5 or r2T(ileg+1) lt 0.5 then begin
      r2T(ileg)   = -555.   
      r2T(ileg+1) = -555.       
   endif

   if r2Ts(ileg) lt 0.5 or r2Ts(ileg+1) lt 0.5 then begin
      r2Ts(ileg)   = -555.   
      r2Ts(ileg+1) = -555.       
    goto,skiptestloopclosed
   endif
nocambiarlosr2:
;stop
;make dT/ds 
  dTmds_l1 = dTmds_ts(ileg  );DERIV(s_l1,Tm_l1)
  dTmds_l2 = dTmds_ts(ileg+1);DERIV(s_l2,Tm_l2)     

if keyword_set(fitcuadr) then  dTmds_l1= (2*Acuadr1[0]*s_l1 + Acuadr1[1])/rsun
if keyword_set(fitcuadr) then  dTmds_l2= (2*Acuadr2[0]*s_l2 + Acuadr2[1])/rsun

;make conductive flux 
  Fc_l1= -kappa*Tm_l1^(5./2)*dTmds_l1
  Fc_l2= -kappa*Tm_l2^(5./2)*dTmds_l2   
  Fc_l1 = -2*kappa*DERIV(S_l1,Tm_l1^(7./2))/7
  Fc_l2 = -2*kappa*DERIV(S_l2,Tm_l2^(7./2))/7


 ; NOTA ALBERT
 ; AQUI HAY UNA INCONSISTENCIA: [s]=Rsun y [dTmds]=K/cm
                                ; con lo cual [Fc_l1,2] está MAL
                                ; CALCULADO AQUI, pues lo que está
                                ; entre () NO DA EN K!!
;   Fc_l1= -kappa*(T0s1 +dTmds_l1*s_l1)^(5./2)*dTmds_l1
;   Fc_l2= -kappa*(T0s2 +dTmds_l2*s_l2)^(5./2)*dTmds_l2   

; NOTA ALBERT
; ESTO EN CAMBIO SI ES CONSISTE pues el ajuste cuadrático ASUME
; [s]=Rsun, como se usa en esta fórmula. Lo que está entre ()
; da en K, como corresponde, y luego [dTmds] = K/cm, con lo cual 
; [Fc_l1,2] aqui está en CGS: erg / cm² / sec.
if keyword_set(fitcuadr) then   Fc_l1= -kappa*(Acuadr1[0]*s_l1^2 + Acuadr1[1]*s_l1+ Acuadr1[2])^(5./2)*dTmds_l1
if keyword_set(fitcuadr) then   Fc_l2= -kappa*(Acuadr2[0]*s_l2^2 + Acuadr2[1]*s_l2+ Acuadr2[2])^(5./2)*dTmds_l2   

;stop

; NOTA ALBERT
; TODO ES CONSISTENTE AQUI ABAJO
; [Fc2_l1,2] aqui está en CGS: erg/cm²/sec,
; tanto en el ajuste lineal como en el cuadrático.
;
; Fcb con el ajuste lineal

     Tmbase_l1=Tm0_ts(ileg)  +gradT_ts(ileg)  *r0
     Tmbase_l2=Tm0_ts(ileg+1)+gradT_ts(ileg+1)*r0
     Fc2_l1 = -kappa*Tmbase_l1^(5./2)*dTmds_ts(ileg) 
     Fc2_l2 = -kappa*Tmbase_l2^(5./2)*dTmds_ts(ileg+1)
     Fcb(ileg  ) = Fc2_l1 * B_l2_r0/(B_l1_r0+B_l2_r0)
     Fcb(ileg+1) = Fc2_l2 * B_l1_r0/(B_l1_r0+B_l2_r0)

if keyword_set(Tfitlow) then begin
     Tmbase_l1 = Tm0s5_ts(ileg  ) + dTmds5_ts(ileg  )*rsun*s_l1_r0
     Tmbase_l2 = Tm0s5_ts(ileg+1) + dTmds5_ts(ileg+1)*rsun*s_l2_r0
     Fc2_l1 = -kappa*Tmbase_l1^(5./2)*dTmds5_ts(ileg  ) 
     Fc2_l2 = -kappa*Tmbase_l2^(5./2)*dTmds5_ts(ileg+1)
     Fcb5(ileg  ) = Fc2_l1 * B_l2_r0/(B_l1_r0+B_l2_r0)
     Fcb5(ileg+1) = Fc2_l2 * B_l1_r0/(B_l1_r0+B_l2_r0)
endif

goto,skip_test_fcb
    if dTmds(ileg) gt 0. AND dTmds(ileg+1) gt 0. then begin
       print,Fcb(ileg),Fcb(ileg+1)
       print,B_l1_r0,B_l2_r0
       print,footlat(ileg),footlat(ileg+1)
       print,footlon(ileg),footlon(ileg+1)
       print,footrad(ileg),footrad(ileg+1)
       print,loopL(il)
       stop  
    endif
skip_test_fcb:

; Fcb con el ajuste cuadrático
if keyword_set(fitcuadr) then begin
     Tmbase_l1=Acuadr1[0]*s_l1_r0^2 + Acuadr1[1]*S_l1_r0+ Acuadr1[2]
     Tmbase_l2=Acuadr2[0]*s_l2_r0^2 + Acuadr2[1]*S_l2_r0+ Acuadr2[2]
     dTmds_l1_base = (2*Acuadr1[0]*s_l1_r0 + Acuadr1[1])/rsun
     dTmds_l2_base = (2*Acuadr2[0]*s_l2_r0 + Acuadr2[1])/rsun
     Fc2_l1 = -kappa*Tmbase_l1^(5./2)*dTmds_l1_base 
     Fc2_l2 = -kappa*Tmbase_l2^(5./2)*dTmds_l2_base
     Fcb(ileg  ) = Fc2_l1 * B_l2_r0/(B_l1_r0+B_l2_r0)
     Fcb(ileg+1) = Fc2_l2 * B_l1_r0/(B_l1_r0+B_l2_r0)
   dTmds(ileg)   = dTmds_l1_base
   dTmds(ileg+1) = dTmds_l2_base
  Tmbase(ileg)   = Tmbase_l1
  Tmbase(ileg+1) = Tmbase_l2
endif

;========================================================================;<---
;nuevo ajuste de Temperatura. Necesito T(s(r0))=T(s_li_r0) y dT/ds
;stop

; COMMENT DE ALBERT
; BAD IDEA
s_l1 = s_l1 * rsun ; pass s_l1 to cm 
s_l2 = s_l2 * rsun ; pass s_l2 to cm 

ss1=n_elements(s_l1)
ss2=n_elements(s_l2)

Numpts = 7

if ss1 gt Numpts then begin
s_ref_l1 = s_l1(0:Numpts-1)
endif else begin
if ss1 le Numpts then s_ref_l1=s_l1
endelse

if ss2 gt Numpts then begin
s_ref_l2 = s_l2((ss2-Numpts):ss2-1)
endif else begin
if ss2 le Numpts then s_ref_l2=s_l2
endelse

Tm_ref_l1 = Tm_l1(0:n_elements(s_ref_l1)-1)
Tm_ref_l2 = Tm_l2((n_elements(Tm_l2)-n_elements(s_ref_l2)):n_elements(Tm_l2)-1)

;<---l1
   xfit = s_ref_l1
   yfit = Tm_ref_l1
   fitr0,xfit,yfit,A,r2
       T0_fit1 = A[0]
     dTds_fit1 = A[1]
     r2Ts_fit1 = r2  
      T0_s0_l1 = T0_fit1+dTds_fit1*s_l1_r0*rsun 

;<---l2
   xfit = s_ref_l2
   yfit = Tm_ref_l2
   fitr0,xfit,yfit,A,r2
       T0_fit2 = A[0]
     dTds_fit2 = A[1]
     r2Ts_fit2 = r2
      T0_s0_l2 = T0_fit2+dTds_fit2*s_l2_r0*rsun

;-----------------------Fc2 con el ajuste de Numpts puntos ;<---------------------------------
 
     FcN2_l1 = -kappa*T0_s0_l1^(5./2)*dTds_fit1 
     FcN2_l2 = -kappa*T0_s0_l2^(5./2)*dTds_fit2 
;Aún no lo grabo, vemos cual decidimos usar ;<---

;---------------------prueba de puntos--------------------
goto,yesterday
ss=(findgen(100)/99)*rsun

window,0,XSIZE=800, YSIZE=900
!p.multi=[0,1,2]
!p.charsize=2

  plot,s_l1,Tm_l1,xrange=[2.,20.]*1.e9,yrange=[0.,2.]*1.e6,xstyle='s*rsun',ytitle='Temp_l1',title='Ajuste lineal de Temp vs s. r=1.075',psym=4
 oplot,s_ref_l1,Tm_ref_l1,psym=2
 oplot,ss,T0_fit1+dTds_fit1*ss
 xyouts,[0.3,0.3],[0.65,0.69],['Fcb_old_l1='+string(Fc2_l1),'Fcb_new_l1='+string(FcN2_l1)],/normal

  plot,s_l2,Tm_l2,xrange=[2.,20.]*1.e9,yrange=[0.,2.]*1.e6,xtitle='s*rsun',ytitle='Temp_l2',title='Ajuste lineal de Temp vs s. r=1.075',psym=4 
 oplot,s_ref_l2,Tm_ref_l2,psym=2  
 oplot,ss,T0_fit2+dTds_fit2*ss
 xyouts,[0.3,0.3],[.15,.19],['Fcb_old_l2='+string(Fc2_l2),'Fcb_new_l2='+string(FcN2_l2)],/normal

!p.multi=0

;record_gif,'./','selección_y_ajuste_'+string(il)+'_'+string(opcls(il))+'.gif','X'

yesterday:
;=====================================================================================
;=====================================================================================

;===============================================================================================================================================================
;======================================================================== Promedio Ne con fiteo ================================================================
;  stop
;fits for leg1--------------------------------------                                                                                                                                                                                      

  xfit = (s_l1/rad_l1)
  yfit = Ne_l1
  fitEr,xfit,yfit,A,corr2
       Ne0_s(ileg) = A[0]
  lambda_N_s(ileg) = (-1./A[1])/rsun  ; Rsun

Ne_l1_s0  = A[0] * exp( A[1]*(s_l1_r0 /r0      )*rsun);*rsun))
Ne_l1_max = A[0] * exp( A[1]*(s_l1_max/r_l1_max)*rsun);*rsun))

;stop

;fits for leg2--------------------------------------                                                                                                                                                                              

  xfit =  (s_l2/rad_l2) ;s_l2/rsun
  yfit =  Ne_l2
  fitEr,xfit,yfit,A,corr2
       Ne0_s(ileg+1) = A[0]
  lambda_N_s(ileg+1) = (-1./A[1])/rsun  ; Rsun                                                                                                                                                                                                      

Ne_l2_s0  = A[0] * exp( A[1]*((s_l2_r0  /r0     )*rsun));(s_l2_r0)); *rsun))
Ne_l2_max = A[0] * exp( A[1]*((s_l2_max/r_l2_max)*rsun));(s_l2_max));*rsun))

;===============================================================================================================================================================
;===============================================================================================================================================================

;stop

; make conductive loss
    Ec_l1 = B_l1 * DERIV(s_l1,Fc_l1/B_l1)
    Ec_l2 = B_l2 * DERIV(s_l2,Fc_l2/B_l2)    
; make heating rate
    Eh_l1 = Er_l1 + Ec_l1
    Eh_l2 = Er_l2 + Ec_l2

;  extrapolate Er_l down to r0

Er_l1_base  =  Er_l1
Er_l2_base  =  Er_l2

rr_l1 = rad_l1
rr_l2 = rad_l2

;----l1
xfit = rr_l1
yfit = Er_l1_base

fitEr,xfit,yfit,A,r2
Er_l1_r0  = A[0] * exp( A[1]*(r0) )
Er_l1_max = A[0] * exp( A[1]*(r_l1_max) )
r2Er(ileg) = r2

if max(s_l1) lt s_l1_max * rsun then begin   

;        s_l1_e = [ s_l1_r0 , s_l1/rsun, s_l1_max]
         s_l1_e = [ s_l1_r0  * rsun, s_l1, s_l1_max * rsun]
         r_l1_e = [r0,rad_l1,r_l1_max]
        Er_l1_e = [Er_l1_r0,Er_l1,Er_l1_max]
         B_l1_e = [ B_l1_r0, B_l1, B_l1_max]
    Er_l1_e_fit = A[0] * exp( A[1] * r_l1_e )

;    Ne_l1_s_fit = Ne0_s(ileg)*exp(((-1./ lambda_N_s(ileg))/rsun) * ((s_l1_e/r_l1_e)*rsun) ) ;<-----------------------------

;   s1fine=s_l1_r0*rsun+(s_l1_max * rsun-s_l1_r0*rsun)*findgen(100)/99.
   ;r1fine=r0+(r_l1_max-r0)*findgen(100)/99.
   ;Er_l1_e_fit = A[0] * exp( A[1] * r1fine )
endif

if max(s_l1) eq s_l1_max * rsun then begin   

;        s_l1_e = [ s_l1_r0 , s_l1/rsun]
         s_l1_e = [ s_l1_r0  * rsun, s_l1]
         r_l1_e =  [r0,rad_l1]
        Er_l1_e = [Er_l1_r0,Er_l1]
         B_l1_e = [ B_l1_r0, B_l1]
    Er_l1_e_fit = A[0] * exp( A[1] * r_l1_e )

;    Ne_l1_s_fit = Ne0_s(ileg)*exp(((-1./ lambda_N_s(ileg))/rsun) * ((s_l1_e/r_l1_e)*rsun) ) ;<-----------------------------

   ;s1fine=s_l1_r0*rsun+(s_l1(n_elements(s_l1)-1)-s_l1_r0*rsun)*findgen(100)/99.
   ;r1fine=r0+(r_l1(n_elements(r_l1)-1)-r0)*findgen(100)/99.
   ;Er_l1_e_fit = A[0] * exp( A[1] * r1fine )
endif

;if opcls(il) eq 2 and footlat(ileg)*footlat(ileg+1) lt 0. AND abs(footlat(ileg)) le 30. AND abs(footlat(ileg+1)) le 30. AND $
;r2T(ileg) ge 0.6 and r2T(ileg+1) ge 0.6 AND r2N(ileg) ge 0.95 AND r2N(ileg+1) ge 0.95 then stop;goto,grafica

;============================================ AJUSTE Er PRIMEROS PUNTOS LINEAL l1
;================================================================================

;ajusto los primeros puntos de Er con una lineal para extrapolar el punto en la base

  xfit = rr_l1[0:3]
  yfit = Er_l1_base[0:3]

;stop
epsEr= 0.1*Er_l1_base[0]    ;<============================================== VER ERRORES Er

fit_ts,xfit,yfit,epsEr,a1,b1,F
   Erlin_F(ileg) = F    ;calidad de ajuste
   Erlin_d(ileg) = a1   ;pendiente
   Erlin_o(ileg) = b1   ;ordenada

     Erlin_l1_r0 = a1*r0+b1   ;punto en la base
  Erlin_r0(ileg) = Erlin_l1_r0

       s_l1_lin = [ s_l1_r0  * rsun, s_l1]
       r_l1_lin = [r0,rad_l1]
      Er_l1_lin = [Erlin_l1_r0,Er_l1]
       B_l1_lin = [ B_l1_r0, B_l1]


           B_eq = (B_l1_r0*B_l2_r0) / (B_l1_r0+B_l2_r0)

           xtmp = s_l1_lin
           ytmp = Er_l1_lin *(B_eq/B_l1_lin)
    phir_lin_l1 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<---
 phir_lin(ileg) = phir_lin_l1

;=============================================================================

;----l2
xfit = rr_l2
yfit = Er_l2_base

fitEr,xfit,yfit,A,r2
    Er_l2_r0  = A[0] * exp( A[1]*(r0) )
    Er_l2_max = A[0] * exp( A[1]*(r_l2_max) )
 r2Er(ileg+1) = r2

if max(s_l2) lt s_l2_max * rsun then begin   

;        s_l2_e = [ s_l2_max, s_l2/rsun, s_l2_r0]
         s_l2_e = [ s_l2_max * rsun, s_l2, s_l2_r0  * rsun]     ;<--- lo defino acá
         r_l2_e = [r_l2_max,rad_l2,r0]
        Er_l2_e = [Er_l2_max,Er_l2,Er_l2_r0] ;<---
         B_l2_e = [ B_l2_max, B_l2, B_l2_r0] ;<---
    Er_l2_e_fit = A[0] * exp( A[1] * r_l2_e )

;    Ne_l2_s_fit = Ne0_s(ileg+1)*exp(((-1./ lambda_N_s(ileg+1))/rsun) * ((s_l2_e/r_l2_e)*rsun) ) ;<-----------------------------

   ;s2fine = s_l2_max+(s_l2_r0  * rsun-s_l2_max)*findgen(100)/99.
   ;r2fine=r_l2_max+(r0-r_l2_max)*findgen(100)/99.   
   ;Er_l2_e_fit = A[0] * exp( A[1] * r2fine )
endif 

if max(s_l2) eq s_l2_max * rsun then begin   

;        s_l2_e = [ s_l2/rsun, s_l2_r0 ]
         s_l2_e = [ s_l2, s_l2_r0  * rsun]     ;<--- lo defino acá
         r_l2_e = [rad_l2,r0]
        Er_l2_e = [Er_l2,Er_l2_r0] ;<---
         B_l2_e = [ B_l2, B_l2_r0] ;<---
    Er_l2_e_fit = A[0] * exp( A[1] * r_l2_e )

;    Ne_l2_s_fit = Ne0_s(ileg+1)*exp(((-1./ lambda_N_s(ileg+1))/rsun) * ((s_l2_e/r_l2_e)*rsun) ) ;<-----------------------------

   ;s2fine = s_l2(0)+(s_l2_r0  * rsun-s_l2(0))*findgen(100)/99.
   ;r2fine=r_l2(0)+(r0-r_l2(0))*findgen(100)/99.   
   ;Er_l2_e_fit = A[0] * exp( A[1] * r2fine )
endif 

;============================================ AJUSTE Er PRIMEROS PUNTOS LINEAL l2
;================================================================================

;ajusto los primeros nErlin de Er con una lineal para extrapolar el punto en la base

  xfit = rr_l2[(n_elements(rr_l2)-4):n_elements(rr_l2)-1]
  yfit = Er_l2_base[(n_elements(Er_l2_base)-4):n_elements(Er_l2_base)-1]

;stop
epsEr= 0.1*Er_l2_base[0]    ;<============================================== VER ERRORES Er

fit_ts,xfit,yfit,epsEr,a1,b1,F
   Erlin_F(ileg+1) = F    ;calidad de ajuste
   Erlin_d(ileg+1) = a1   ;pendiente
   Erlin_o(ileg+1) = b1   ;ordenada

       Erlin_l2_r0 = a1*r0+b1     ;punto en la base
  Erlin_r0(ileg+1) = Erlin_l1_r0

       s_l2_lin = [s_l2,  s_l2_r0  * rsun]
       r_l2_lin = [rad_l2,r0]
      Er_l2_lin = [Er_l2,Erlin_l2_r0]
       B_l2_lin = [B_l2, B_l2_r0]

             xtmp = s_l2_lin
             ytmp = Er_l2_lin *(B_eq/B_l2_lin)
      phir_lin_l2 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<---
 phir_lin(ileg+1) = phir_lin_l2

;=============================================================================

           xtmp = s_l1_e
           ytmp = Er_l1_e *(B_eq/B_l1_e)
        phir_l1 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<---
     phir(ileg) = phir_l1
           xtmp = s_l2_e                                    ;<--
           ytmp = Er_l2_e *(B_eq/B_l2_e)
        phir_l2 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<--- di vuelta los datos. si doy vuelta s_l1 sale un - que debería poner?
   phir(ileg+1) = phir_l2                                    ;<---     aunque tmb haya dado vuelta er? (lo pongo) 

           xtmp = s_l1_e
           ytmp = Er_l1_e_fit *(B_eq/B_l1_e)
        phir_l1 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<---
  phirfit(ileg) = phir_l1                                   ;<--
           xtmp = s_l2_e                                    ;<--
           ytmp = Er_l2_e_fit *(B_eq/B_l2_e)
        phir_l2 = int_tabulated(xtmp,ytmp,/sort) ; flujo radiaivo ;<--- di vuelta los datos. si doy vuelta s_l1 sale un - que debería poner?
phirfit(ileg+1) = phir_l2                                    ;<---     aunque tmb haya dado vuelta er? (lo pongo) 


;if phir_l2 lt 0. and r2N(ileg+1) gt 0.8 then stop
; if phir_l1 lt 0. and r2N(ileg  ) gt 0.8 and phir_l2 lt 0. and r2N(ileg+1) gt 0.8 then print,il ;stop
;if phir_l1 lt 0. then stop


;======================================================DENSIDAD 
goto,notech
;stop

unit_l1 = findgen(n_elements(s_l1_e))+1
 uni_l1 = unit_l1/unit_l1

          xtmp = s_l1_e
          ytmp = Ne_l1_s_fit
     Netech_l1 = int_tabulated(xtmp,ytmp,/sort)/int_tabulated(xtmp,uni_l1,/sort) 
  Netech(ileg) = Netech_l1

unit_l2 = findgen(n_elements(s_l2_e))+1
 uni_l2 = unit_l2/unit_l2

          xtmp = s_l2_e
          ytmp = Ne_l2_s_fit
     Netech_l2 = int_tabulated(xtmp,ytmp,/sort)/int_tabulated(xtmp,uni_l2,/sort) 
Netech(ileg+1) = Netech_l2

;stop
notech:
;=========================================================================================================================================================
 

;stop
;if opcls(il) eq 2 then stop

s_l1 = s_l1 / rsun ; back s_l1 to Rsun 
s_l2 = s_l2 / rsun ; back s_l2 to Rsun

; test diferent models of the heating rate

   Eh(ileg)   = mean (Eh_l1)
   Eh(ileg+1) = mean (Eh_l2)

       index1 = where( Eh_l1 gt 0) 
       index2 = where( Eh_l2 gt 0) 

if n_elements(index1) lt n_elements(Eh_l1) then Eh(ileg)   = -555.
if n_elements(index2) lt n_elements(Eh_l2) then Eh(ileg+1) = -555.

  deltaEh (ileg)   = stdev (Eh_l1)
  deltaEh (ileg+1) = stdev (Eh_l2)
              smin = min(s_l1)
              smax = max(s_l1)
              xfit = s_l1
              yfit = Eh_l1

  fitEh,xfit,yfit,smin,smax,A,r2
              Eh01 = A[0]
          sH(ileg) = A[1]
        r2sH(ileg) = r2
              smin = min(s_l2)
              smax = max(s_l2)
              xfit = s_l2
              yfit = Eh_l2

  fitEh,xfit,yfit,smin,smax,A,r2
              Eh02 = A[0]
        sH(ileg+1) = A[1]
      r2sH(ileg+1) = r2

;...........................................
;if Eh(ileg) lt 0 or Eh(ileg+1) lt 0 then  goto,skiptestloopclosed
;if r2sh(ileg) lt 0.75  or r2sh(ileg+1) lt 0.75  then
goto,skipmordor
if  r2t(ileg) gt 0.98 and r2t(ileg+1) gt 0.5  and opcls(il) eq 2 and gradt(ileg) gt 0 and gradt(ileg+1) gt 0 then begin
  wn=0
  window,wn,xs=400,ys=800
 !p.multi=[0,1,2]
 !p.charsize=2
 !p.charthick=1
 X=1.+.25*findgen(100)/99.
plot,rad_l1,ne_l1/1.e8,psym=4,title='Ne'
oplot,X,Ne0(ileg)* exp( -(1/lambda_N(ileg))*(1.-1./X) ) /1.e8
;oplot,rad_l2,ne_l2/1.e8,psym=5
;oplot,X,Ne0(ileg+1)* exp( -(1/lambda_N(ileg+1))*(1.-1./X) ) /1.e8
 xyouts,0.5*[1,1,1],1.-[.1,.15,.2],[string(Ne0(ileg)/1.e8),string(lambda_N(ileg)),string(r2n(ileg))],/normal
 plot,rad_l1,tm_l1/1.e6,psym=4,title='Tm'
  oplot,X,(T0r1+gradT(ileg)*X)/1.e6
; oplot,rad_l2,tm_l2/1.e6,psym=5
;oplot,X,(T0r2+gradT(ileg+1)*X)/1.e6
  xyouts,0.5*[1,1,1],(1./3)*1.-[.15,.2,.25],[string(T0r1/1.e6),string(gradT(ileg)/1.e6),string(r2t(ileg))],/normal
stop
endif
skipmordor:

;==================================================== Qué loop grafico?
;iii=0
;if n_elements(rad_l1) le 3 then  iii=iii+1
;print,iii
 ;X = min(s_l1) + (max(s_l1)-min(s_l1))*findgen(100)/99
 ;r2crit_Ne = 0.85
 ;FTscrit   = 0.85
 ;if opcls(il) eq 2 and r2N(ileg) ge r2crit_Ne and r2N(ileg+1) ge r2crit_Ne and FTs_ts(ileg) ge FTscrit and FTs_ts(ileg+1) ge FTscrit then goto,grafica

;QS2081
; if opcls(il) eq 2 and footlat(ileg) ge 25. and footlat(ileg+1) ge 25.  then goto,grafica
;

; if opcls(il) eq 2 and (Erlin_F(ileg) lt 0.5 or Erlin_F(ileg+1) lt 0.5) then goto,grafica

;and footlat(ileg+1) ge 60. and footlat(ileg) ge 60 and footlat(ileg) le 90. and footlat(ileg+1) le 90. and footlon(ileg+1) ge 0. and footlon(ileg) ge 0 and footlon(ileg) le 45. and footlon(ileg+1) le 45. 
; if opcls(il) eq 2 and FTs_ts(ileg) lt 0.5 and FTs5_ts(ileg) lt 0.8 AND footlat(ileg+1) ge 0. and footlat(ileg) le 30. and footlat(ileg+1) le 30. then goto,grafica

;if opcls(il) eq 2 AND r2Er(ileg) ge r2crit AND r2Er(ileg+1) ge r2crit
;AND FTs_ts(ileg) ge FTscrit AND FTs_ts(ileg+1) ge FTscrit then goto,grafica
; if abs((Tm0s_ts(ileg)+dTmds_ts(ileg)*rsun*Smaxxx(ileg))-(Tm0s_ts(ileg)+dTmds_ts(ileg)*rsun*S_r0_a(ileg))) lt 2*eplegT(ileg) then goto,grafica
;if abs(dTmds_ts(ileg))*(Smaxxx(ileg)-S_r0_a(ileg))*rsun lt eplegT(ileg) and abs(dTmds_ts(ileg+1))*(Smaxxx(ileg+1)-S_r0_a(ileg+1))*rsun lt eplegT(ileg+1) then goto,grafica
;if FTs_ts(ileg) ge FTscrit and opcls(il) eq 2 then goto,grafica

;AR2099
;if opcls(il) eq 2 AND footlat(ileg) ge 0. AND footlat(ileg+1) ge 0. and footlat(ileg) le 30. and footlat(ileg+1) le 30. ;$
;    and footlon(ileg) le 100. AND footlon(ileg+1) le 100. and footlon(ileg) ge 70. AND footlon(ileg+1) ge 70. then goto,grafica

;QS2099
;if opcls(il) eq 2 AND footlat(ileg) le 0. AND footlat(ileg+1) le 0. $
;    and footlon(ileg) le 100. AND footlon(ileg+1) le 100. and footlon(ileg) ge 50. AND footlon(ileg+1) ge 50. then goto,grafica

;if opcls(il) eq 2 AND footlat(ileg)*footlat(ileg+1) lt 0. then goto,grafica

;if opcls(il) eq 2 and footlat(ileg)*footlat(ileg+1) lt 0. AND abs(footlat(ileg)) le 30. AND abs(footlat(ileg+1)) le 30. AND $
;r2T(ileg) ge 0.6 and r2T(ileg+1) ge 0.6 AND r2N(ileg) ge 0.95 AND r2N(ileg+1) ge 0.95 then goto,grafica

; if opcls(il) eq 2 AND footlat(ileg)*footlat(ileg+1) lt 0. AND abs(footlat(ileg)) ge 60. AND abs(footlat(ileg+1)) ge 60. then goto,grafica

goto,skiptestloopclosed
;--------------------COMIENZA--------------------------------------
grafica:

goto,fin1

 wn=0
 window,wn,xs=800,ys=400
 plot,[r_l1_e,r_l2_e],1.e5*[Er_l1_e,Er_l2_e],yr=[1.e5*min([Er_l1_e,Er_l2_e]),1.e5*max([Er_l1_e,Er_l2_e])],xtitle='r [Rsun]',ytitle='Er',title='Radiative Loss Rate',Psym=4,th=4,charsize=2.25,/nodata,font=0
; [10!U-5!Nergcm!U-3!Ns!U-1!N]'
loadct,12
 oplot,[r_l1_e],1.e5*[Er_l1_e],Psym=4,th=2,color=110,symsize=1.5
 oplot,[r_l2_e],1.e5*[Er_l2_e],Psym=4,th=2,color=206,symsize=1.5
 oplot,r_l1_e,1.e5*Er_l1_e_fit,th=2
 oplot,r_l2_e,1.e5*Er_l2_e_fit,th=2
 xyouts,0.6*[1,1],1.-[.2,.3],['r2 =',string(r2Er(ileg  ))],color=[110,0],/normal
 xyouts,0.6*[1,1],1.-[.2,.3],['r2 =',string(r2Er(ileg+1))],color=[206,0],/normal
loadct,0

fin1:

 wn=0
 window,wn,xs=900,ys=900
 ; ps1,'./newfigs/'+'loop_example.eps',0
 ;DEVICE,/INCHES,YSIZE=10.,XSIZE=5.,SCALE_FACTOR=2

 !p.multi=[0,2,2]
 !p.charsize=2.25
 !p.charthick=1
 X=1.+.25*findgen(100)/99.

 plot,rad_l1,ne_l1/1.e8,psym=4,title='Ne(r)-leg1'
 xyouts,0.2*[1,1],1.-[.1,.125],[string(footlat(ileg)),string(footlon(ileg))],/normal

eps1plot = 0.1*Er_l1[0]*1.e5

plot,[rad_l1],1.e5*[Er_l1],psym=4,title='Er_l1'
 oplot,rad_l1[0:3],(Erlin_d(ileg)*rad_l1[0:4]+Erlin_o(ileg))*1.e5
 oplot,rad_l1[0:3],(Erlin_d(ileg)*rad_l1[0:4]+Erlin_o(ileg))*1.e5-eps1plot
 oplot,rad_l1[0:3],(Erlin_d(ileg)*rad_l1[0:4]+Erlin_o(ileg))*1.e5+eps1plot
 xyouts,0.7*[1],1.-[.1],[string(Erlin_F(ileg))],/normal

 plot,rad_l2,ne_l2/1.e8,psym=4,title='Ne-leg2'
 xyouts,0.2*[1,1],1.-[.6,.625],[string(footlat(ileg+1)),string(footlon(ileg+1))],/normal

eps2plot = 0.1*Er_l2[n_elements(Er_l2)-1]*1.e5

plot,[rad_l2],1.e5*[Er_l2],psym=4,title='Er_l2'
 oplot,rad_l2[(n_elements(rad_l2)-4):(n_elements(rad_l2)-1)],(Erlin_d(ileg+1)*rad_l2[(n_elements(rad_l2)-4):(n_elements(rad_l2)-1)]+Erlin_o(ileg+1))*1.e5
 oplot,rad_l2[(n_elements(rad_l2)-4):(n_elements(rad_l2)-1)],(Erlin_d(ileg+1)*rad_l2[(n_elements(rad_l2)-4):(n_elements(rad_l2)-1)]+Erlin_o(ileg+1))*1.e5-eps2plot
 oplot,rad_l2[(n_elements(rad_l2)-4):(n_elements(rad_l2)-1)],(Erlin_d(ileg+1)*rad_l2[(n_elements(rad_l2)-4):(n_elements(rad_l2)-1)]+Erlin_o(ileg+1))*1.e5+eps2plot
 xyouts,0.7*[1],1.-[.6],[string(Erlin_F(ileg+1))],/normal

;stop
; 

; 

!p.multi=0

goto,no1
; diferents plots for leg1
 
 wn=0
 window,wn,xs=400,ys=900
 ; ps1,'./newfigs/'+'loop_example.eps',0
 ;DEVICE,/INCHES,YSIZE=10.,XSIZE=5.,SCALE_FACTOR=2

 !p.multi=[0,1,3]
 !p.charsize=2.25
 !p.charthick=1
 X=1.+.25*findgen(100)/99.

 plot,rad_l1,ne_l1/1.e8,psym=4,title='Ne(r)-leg1'
 oplot,X,Ne0(ileg)* exp( -(1/lambda_N(ileg))*(1.-1./X) ) /1.e8
 xyouts,0.5*[1,1,1],1.-[.1,.125,.15],[string(Ne0(ileg)/1.e8),string(lambda_N(ileg)),string(r2n(ileg))],/normal

 plot,rad_l1,p_l1,psym=4,title='P(r)-leg1'
 oplot,X,p0(ileg)* exp( -(1/lambda_p(ileg))*(1.-1./X) )
 xyouts,0.5*[1,1,1],(2./3)*1.-[.1,.125,.15],[string(P0(ileg)),string(lambda_p(ileg)),string(r2p(ileg))],/normal

 plot,rad_l1,tm_l1/1.e6,psym=4,title='Tm(r)-leg1'
; oplot,X,(T0r1+gradT(ileg)*X)/1.e6
; xyouts,0.5*[1,1,1],(1./3)*1.-[.2,.225,.25],[string(T0r1/1.e6),string(gradT(ileg)/1.e6),string(r2t(ileg))],/normal

 !p.multi=0
no1:
goto,chau
 wn=0
 window,wn,xs=1400,ys=800
 !p.multi=0
; !p.multi=[0,1,2]
;!p.charsize=2.5 
!p.charsize=1. 
 X = min(s_l1) + (max(s_l1)-min(s_l1))*findgen(100)/99

xx = min(s_l1(0:nump1-1)) + (max(s_l1(0:nump1-1))-min(s_l1(0:nump1-1)))*findgen(100)/99

  plot,s_l1,tm_l1/1.e6,psym=4,title='Tm(s)-leg1 linear'
 oplot,X,(Tm0s_ts(ileg)+dTmds_ts(ileg)*rsun*X)/1.e6
; xyouts,0.5*[1,1,1],(1.)*1.-[.25,.28,.31],[string(Tm0_ts/1.e6),string(dTmds_ts(ileg)/1.e6*rsun),string(FTs_ts(ileg))],/normal
 oplot,X,[eplegT(ileg)*(DINDGEN(n_elements(X))-DINDGEN(n_elements(X))+1)]/1.e6+(Tm0s_ts(ileg)+dTmds_ts(ileg)*rsun*X)/1.e6;,psym=4
 oplot,X,(Tm0s_ts(ileg)+dTmds_ts(ileg)*rsun*X)/1.e6-[eplegT(ileg)*(DINDGEN(n_elements(X))-DINDGEN(n_elements(X))+1)]/1.e6;,psym=4
; plot,s_l1,tm_l1/1.e6,psym=4,title='Tm(s)-leg1 cuadr'
; oplot,X,(Acuadr1[0]*(X)^2 + Acuadr1[1]*(X) + Acuadr1[2])/1.e6
; xyouts,0.5*[1],(1./2)*1.-[.35],[string(r2Tcuadr(ileg))],/normal

 oplot,xx,(Tm0s5_ts(ileg)+dTmds5_ts(ileg)*rsun*xx)/1.e6
 oplot,xx,[eplegT(ileg)*(DINDGEN(n_elements(xx))-DINDGEN(n_elements(xx))+1)]/1.e6+(Tm0s5_ts(ileg)+dTmds5_ts(ileg)*rsun*xx)/1.e6;,psym=4
 oplot,xx,(Tm0s5_ts(ileg)+dTmds5_ts(ileg)*rsun*xx)/1.e6-[eplegT(ileg)*(DINDGEN(n_elements(xx))-DINDGEN(n_elements(xx))+1)]/1.e6;,psym=4

 !p.multi=0
chau:
goto,fin
; plots for leg2
 wn=2
 window,wn,xs=400,ys=900
 !p.multi=[0,1,3]
 !p.charsize=2.25 
 X=1.+.25*findgen(100)/99.

 plot,rad_l2,ne_l2/1.e8,psym=4,title='Ne-leg2'
 oplot,X,Ne0(ileg+1)* exp( -(1/lambda_N(ileg+1))*(1.-1./X) ) /1.e8
 xyouts,0.5*[1,1,1],1.-[.1,.125,.15],[string(Ne0(ileg+1)/1.e8),string(lambda_N(ileg+1)),string(r2n(ileg+1))],/normal

 plot,rad_l2,p_l2,psym=4,title='P-leg2'
 oplot,X,p0(ileg+1)* exp( -(1/lambda_p(ileg+1))*(1.-1./X) )
 xyouts,0.5*[1,1,1],(2./3)*1.-[.1,.125,.15],[string(P0(ileg+1)),string(lambda_p(ileg+1)),string(r2p(ileg+1))],/normal

 plot,rad_l2,tm_l2/1.e6,psym=4,title='Tm-leg2'
; oplot,X,(T0r2+gradT(ileg+1)*X)/1.e6
; xyouts,0.5*[1,1,1],(1./3)*1.-[.2,.225,.25],[string(T0r2/1.e6),string(gradT(ileg+1)/1.e6),string(r2t(ileg+1))],/normal

 !p.multi=0
fin:
goto,chau2
 wn=3

 window,wn,xs=400,ys=600
 !p.multi=[0,1,2]
 !p.charsize=1.5 
 
 x = min(s_l2) + (max(s_l2)-min(s_l2))*findgen(100)/99
  
 plot,s_l2,tm_l2/1.e6,psym=4,title='Tm(s)-leg2 linear'
 oplot,X,(T0s2+dTmds(ileg+1)*rsun*X)/1.e6
 xyouts,0.5*[1,1,1],(1.)*1.-[.25,.28,.31],[string(T0s2/1.e6),string(dTmds(ileg+1)/1.e6*rsun),string(r2Ts(ileg+1))],/normal

 plot,s_l2,tm_l2/1.e6,psym=4,title='Tm(s)-leg2 cuadr'
 oplot,X,(Acuadr2[0]*(X)^2 + Acuadr2[1]*(X) + Acuadr2[2])/1.e6
 xyouts,0.5*[1],(1./2)*1.-[.35],[string(r2Tcuadr(ileg+1))],/normal

 !p.multi=0

 wn=4
 window,wn,xs=400,ys=600
 !p.multi=[0,1,2]
 !p.charsize=1.5 
 plot,lat_v(ifirs_1:ilast_2,il),rad_v(ifirs_1:ilast_2,il),ystyle=1,ytitle='Rad',xtitle='Lat'
 plot,lon_v(ifirs_1:ilast_2,il),ytitle='Lon'
 !p.multi=0

chau2:

goto,nada22
 WN=8
 WINDOW,WN,XS=600,YS=400
!P.CHARSIZE=1.5
!P.SYMSIZE=1
 PLOT,[RAD_L1,RAD_L2],[TM_L1,TM_L2]/1.E6,YR=[0.5,2.],XTITLE='R [RSUN]',YTITLE='TE [MK]',TITLE='TEMPERATURA',PSYM=4,TH=3,CHARSIZE=1.5,/NODATA,FONT=0,XTHICK=THICK,YTHICK=THICK,YSTYLE=1
LOADCT,12
 OPLOT,[RAD_L1],[TM_L1]/1.E6,PSYM=4,TH=3,COLOR=110,SYMSIZE=1.5
 OPLOT, RAD_L1 ,(TM0_TS(ILEG)+GRADT_TS(ILEG)*RAD_L1)/1.E6,TH=5
 OPLOT,[RAD_L2],[TM_L2]/1.E6,PSYM=4,TH=3,COLOR=206,SYMSIZE=1.5
 OPLOT, RAD_L2 ,(TM0_TS(ILEG+1)+GRADT_TS(ILEG+1)*RAD_L2)/1.E6,TH=3
 XYOUTS,0.75*[1,1.1],1.-[.6,.6],['R!U2!N =',STRMID(STRING(FTS_TS(ILEG  )),4,5)],COLOR=[110,110],FONT=0,/NORMAL
 XYOUTS,0.75*[1,1.1],1.-[.7,.7],['R!U2!N =',STRMID(STRING(FTS_TS(ILEG+1)),4,5)],COLOR=[206,206],FONT=0,/NORMAL

LOADCT,0
nada22:
;grafica:

goto,nada
 !p.multi=0

 ps1,'./newfigs/aTm.eps',0

DEVICE,/TIMES, FONT_INDEX=4
DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1

THICK=5
; WN=8
; WINDOW,WN,XS=600,YS=400
!P.CHARSIZE=2.5
!P.SYMSIZE=2
 PLOT,[RAD_L1,RAD_L2],[TM_L1,TM_L2]/1.E6,YR=[0.5,2.],XTITLE='r [Rsun]',YTITLE='Te [MK]',TITLE='DEMT Temperature',PSYM=4,TH=5,CHARSIZE=2.5,/NODATA,FONT=0,XTHICK=THICK,YTHICK=THICK,YSTYLE=1
LOADCT,12
 OPLOT,[RAD_L1],[TM_L1]/1.E6,PSYM=4,TH=5,COLOR=110,SYMSIZE=1.5
 OPLOT, RAD_L1 ,(TM0_TS(ILEG)+GRADT_TS(ILEG)*RAD_L1)/1.E6,TH=5
 OPLOT,[RAD_L2],[TM_L2]/1.E6,PSYM=4,TH=5,COLOR=206,SYMSIZE=1.5
 OPLOT, RAD_L2 ,(TM0_TS(ILEG+1)+GRADT_TS(ILEG+1)*RAD_L2)/1.E6,TH=5

;SUPERPONER LINEAS DE ERROR
; oplot,rad_l1,[eplegT(ileg)+(Tm0_ts(ileg)+gradT_ts(ileg)*rad_l1)]/1.e6,th=3,color=10,linestyle=3
; oplot,rad_l1,[(Tm0_ts(ileg)+gradT_ts(ileg)*rad_l1)-eplegT(ileg)]/1.e6,th=3,color=10,linestyle=3
; oplot,rad_l2,[eplegT(ileg+1)+(Tm0_ts(ileg+1)+gradT_ts(ileg+1)*rad_l2)]/1.e6,th=3,color=10,linestyle=3
; oplot,rad_l2,[(Tm0_ts(ileg+1)+gradT_ts(ileg+1)*rad_l2)-eplegT(ileg+1)]/1.e6,th=3,color=10,linestyle=3

 xyouts,0.75*[1,1.1],1.-[.6,.6],['F = ',STRMID(string(FTs_ts(ileg  )),4,5)],color=[110,0],font=0,/normal
 xyouts,0.75*[1,1.1],1.-[.7,.7],['F = ',STRMID(string(FTs_ts(ileg+1)),4,5)],color=[206,0],font=0,/normal

; XYOUTS,0.75*[1,1.1],1.-[.6,.6],['R!U2!N =',STRMID(STRING(FTS_TS(ILEG  )),4,5)],COLOR=[110,0],FONT=0,/NORMAL
; XYOUTS,0.75*[1,1.1],1.-[.7,.7],['R!U2!N =',STRMID(STRING(FTS_TS(ILEG+1)),4,5)],COLOR=[206,0],FONT=0,/NORMAL

LOADCT,0
;PLOT,RAD_V(IFIRS_1:ILAST_2,IL),(TM_V(IFIRS_1:ILAST_2,IL)/1.E6)>0,YSTYLE=1,XTITLE='R [RSUN]',YTITLE='TE [MK]',PSYM=4,TH=2,CHARSIZE=2,FONT=-1
 PS2

 !P.MULTI=0
PS1,'./newfigs/aNe.eps',0
DEVICE,/TIMES, FONT_INDEX=4
DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1
THICK=5
; WN=8                                                                                                                                          
; WINDOW,WN,XS=600,YS=400                                                                                                                       
!P.CHARSIZE=2.5
!P.SYMSIZE=2
 PLOT,[RAD_L1,RAD_L2],[NE_L1,NE_L2]/1.E8,YR=[0.1,2.],XTITLE='r [Rsun]',YTITLE='Ne [10!U8!N cm!U-3!N]',TITLE='DEMT Electron density',PSYM=4,TH=5,CHARSIZE=2.5,/NODATA,FONT=0,XTHICK=THICK,YTHICK=THICK,YSTYLE=1
LOADCT,12
 OPLOT,[RAD_L1],[NE_L1]/1.E8,PSYM=4,TH=5,COLOR=110,SYMSIZE=1.5
 OPLOT, RAD_L1 ,NE0(ILEG)*EXP((-1./LAMBDA_N(ILEG))*(1.-1./RAD_L1))/1.E8,TH=5
 OPLOT,[RAD_L2],[NE_L2]/1.E8,PSYM=4,TH=5,COLOR=206,SYMSIZE=1.5
 OPLOT, RAD_L2 ,NE0(ILEG+1)*EXP((-1./LAMBDA_N(ILEG+1))*(1.-1./RAD_L2))/1.E8,TH=5
 XYOUTS,0.75*[1,1.1],1.-[.2,.2],['r!U2!N =',STRMID(STRING(R2N(ILEG  )),4,5)],COLOR=[110,0],FONT=0,/NORMAL
 XYOUTS,0.75*[1,1.1],1.-[.3,.3],['r!U2!N =',STRMID(STRING(R2N(ILEG+1)),4,5)],COLOR=[206,0],FONT=0,/NORMAL
 LOADCT,0
 PS2

 PS1,'./newfigs/aB.eps',0

DEVICE,/TIMES, FONT_INDEX=4
DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1
THICK=5
; WN=7
; WINDOW,WN,XS=600,YS=400
!P.CHARSIZE=2.5
!P.SYMSIZE=2
 PLOT,[R_L1_E,R_L2_E],[B_L1_E,B_L2_E],YR=[MIN([B_L1_E,B_L2_E]),MAX([B_L1_E,B_L2_E])],XTITLE='r [Rsun]',YTITLE='B [Gauss]',TITLE='Magnetic field',PSYM=4,TH=5,CHARSIZE=2.5,/NODATA,FONT=0,XTHICK=THICK,YTHICK=THICK
LOADCT,12 
 OPLOT,[R_L1_E],[B_L1_E],PSYM=4,TH=5,COLOR=110,SYMSIZE=1.5
 OPLOT,[R_L2_E],[B_L2_E],PSYM=4,TH=5,COLOR=206,SYMSIZE=1.5
LOADCT,0
;PLOT,RAD_V(IFIRS_1:ILAST_2,IL),B_V(IFIRS_1:ILAST_2,IL),YSTYLE=1,XTITLE='R [RSUN]',YTITLE='B [GAUSS]',PSYM=4,TH=2,CHARSIZE=2,FONT=-1
 PS2


 PS1,'./newfigs/aR.eps',0

DEVICE,/TIMES, FONT_INDEX=4
DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1
THICK=5
; WN=9
; WINDOW,WN,XS=600,YS=400
!P.CHARSIZE=2.5
!P.SYMSIZE=2
PLOT,LAT_V(IFIRS_1:ILAST_2,IL),RAD_V(IFIRS_1:ILAST_2,IL),YTITLE='r [Rsun]',XTITLE='Latitude [Deg]',TITLE='Loop shape',PSYM=4,TH=5,CHARSIZE=2.5, YSTYLE=1,/NODATA,FONT=0,XTHICK=THICK,YTHICK=THICK

;PLOTS,[LAT_V(IFIRS_1:ILAST_1,IL),LAT_V(IFIRS_2:ILAST_2,IL)],[LON_V(IFIRS_1:ILAST_1,IL),LON_V(IFIRS_2:ILAST_2,IL)],[RAD_V(IFIRS_1:ILAST_1,IL),RAD_V(IFIRS_2:ILAST_2,IL)],/T3D
;PLOT_3DBOX,[LAT_V(IFIRS_1:ILAST_1,IL),LAT_V(IFIRS_2:ILAST_2,IL)],[LON_V(IFIRS_1:ILAST_1,IL),LON_V(IFIRS_2:ILAST_2,IL)],[RAD_V(IFIRS_1:ILAST_1,IL),RAD_V(IFIRS_2:ILAST_2,IL)], $
;   GRIDSTYLE=2, $
;   TITLE='FORMA DEL ARCO', $
;   XTITLE='LATITUD [DEG]', YTITLE='LONGITUD [DEG]', $
;   ZTITLE='R [RSUN]', $
;   PSYM=6, CHARSIZE=5, COLOR=206

LOADCT,12

 OPLOT,LAT_V(IFIRS_1:ILAST_1,IL),RAD_V(IFIRS_1:ILAST_1,IL),PSYM=4,TH=5,COLOR=110,SYMSIZE=1.5
 OPLOT,LAT_V(IFIRS_2:ILAST_2,IL),RAD_V(IFIRS_2:ILAST_2,IL),PSYM=4,TH=5,COLOR=206,SYMSIZE=1.5
LOADCT,0
;PLOT,[LAT_L1,LAT_L2],[S_L1,S_L2],YSTYLE=1,YTITLE='S [RSUN]',XTITLE='LAT [DEG]',TITLE='LOOP (R VS LAT)',PSYM=4,TH=5,CHARSIZE=2.5,/NODATA
 PS2

;STOP

 PS1,'./newfigs/aEr.eps',0

DEVICE,/TIMES, FONT_INDEX=4
DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1
thick=5
; wn=6
; window,wn,xs=600,ys=400
!p.charsize=2.5
!p.symsize=2
 plot,[r_l1_e,r_l2_e],1.e5*[Er_l1_e,Er_l2_e],yr=[1.e5*min([Er_l1_e,Er_l2_e]),1.e5*max([Er_l1_e,Er_l2_e])],ytitle='Er [10!U-5!N erg cm!U-3!N s!U-1!N]',xtitle='r [Rsun]',title='Radiative power',Psym=4,th=5,charsize=2.5,/nodata,font=0,xthick=thick,ythick=thick
loadct,12
 oplot,[r_l1_e],1.e5*[Er_l1_e],Psym=4,th=5,color=110,symsize=1.5
 oplot,[r_l2_e],1.e5*[Er_l2_e],Psym=4,th=5,color=206,symsize=1.5
 oplot,r_l1_e,1.e5*Er_l1_e_fit,th=5
 oplot,r_l2_e,1.e5*Er_l2_e_fit,th=5
 xyouts,0.75*[1,1.1],1.-[.2,.2],['r!U2!N =',strmid(string(r2Er(ileg  )),4,5)],color=[110,0],font=0,/normal
 xyouts,0.75*[1,1.1],1.-[.3,.3],['r!U2!N =',strmid(string(r2Er(ileg+1)),4,5)],color=[206,0],font=0,/normal
loadct,0
 ps2

nada:
 stop
;--------------------TERMINA---------------------------------------

goto,skip_otros_graficos
 
stop
  
  plot,s_l1,Fc_l1,psym=4,title='Fc-leg1'
  
 wn=2
 ;window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 
 plot,rad_l1,Er_l1,psym=4,title='radiative loss-leg1'

 plot,rad_l1,Ec_l1,psym=4,title='conductive loss-leg1'
 
 ;plot,rad_l1,Eh_l1,psym=4,title='heating rate-leg1'
 plot,s_l1,Eh_l1,psym=4,title='heating rate-leg1'
oplot,x,Eh01*exp(x/Sh(ileg))
xyouts,0.5*[1,1,1],(1./3)*1.-[.1,.15,.20],[string(Eh01),string(sH(ileg)),string(r2sh(ileg))],/normal
stop

  plot,s_l2,Fc_l2,psym=4,title='Fc-leg2'
 
wn=2

 ;window,wn,xs=400,ys=800
 !p.multi=[0,1,3]
 !p.charsize=2
 !p.charthick=1
 
 plot,rad_l2,Er_l2,psym=4,title='radiative loss-leg2'

 plot,rad_l2,Ec_l2,psym=4,title='conductive loss-leg2'
 
 ;plot,rad_l2,Eh_l2,psym=4,title='heating rate-leg2'
  plot,s_l2,Eh_l2,psym=4,title='heating rate-leg2'
 oplot,x,Eh02*exp(x/Sh(ileg+1))
 xyouts,0.5*[1,1,1],(1./3)*1.-[.1,.15,.20],[string(Eh02),string(sH(ileg+1)),string(r2sh(ileg+1))],/normal
ps2
stop
skip_otros_graficos:

skiptestloopclosed:
;...........................................


  skipfitloop:
     opclstat(ileg)   =      opcls(il)
     opclstat(ileg+1) =      opcls(il)
  loop_length(ileg)   =      loopL(il) 
  loop_length(ileg+1) =      loopL(il)
        strad(ileg)   =      str_v(il)
        stlat(ileg)   = (90-stth_v(il)/!dtor)
        stlon(ileg)   =     stph_v(il)/!dtor  
        strad(ileg+1) =      str_v(il)
        stlat(ileg+1) = (90-stth_v(il)/!dtor)
        stlon(ileg+1) =     stph_v(il)/!dtor
  skipnextloop:
  indexloop(ileg)  = il
  indexloop(ileg+1)= il  
 ;if gradT(ileg)*gradT(ileg+1) lt 0. then begin
 ;indexloop(ileg)   = -678.   
 ;indexloop(ileg+1) = -678.
 ;endif 

  ileg = ileg+2

endelse
endfor


return
end

  pro switch_legs,rad_l1,rad_l2,lat_l1,lat_l2,lon_l1,lon_l2,Ne_l1,Ne_l2,Tm_l1,Tm_l2,WT_l1,WT_l2,scoreR_l1,scoreR_l2,il,switching
  common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v   

  str = str_v(il)  &  stth = stth_v(il)  &  stph = stph_v(il)
  xst = str * sin(stth) * cos(stph) 
  yst = str * sin(stth) * sin(stph) 
  zst = str * cos(stth)

  fr1 = abs(rad_l1-radstart) & i1 = (where(fr1 eq min(fr1)))(0)
  fr2 = abs(rad_l2-radstart) & i2 = (where(fr2 eq min(fr2)))(0)

  r1 = rad_l1(i1)  &  th1 = (90.-lat_l1(i1))*!dtor  &  ph1 = lon_l1(i1)*!dtor
  r2 = rad_l2(i2)  &  th2 = (90.-lat_l2(i2))*!dtor  &  ph2 = lon_l2(i2)*!dtor

  x1 = r1 * sin(th1) * cos(ph1)  &  y1 = r1 * sin(th1) * sin(ph1)  &  z1 = r1 * cos(th1)
  x2 = r2 * sin(th2) * cos(ph2)  &  y2 = r2 * sin(th2) * sin(ph2)  &  z2 = r2 * cos(th2)

  d1 = sqrt((x1-xst)^2+(y1-yst)^2+(z1-zst)^2)
  d2 = sqrt((x2-xst)^2+(y2-yst)^2+(z2-zst)^2)

  goto,skiptestswitch
  print,'-----------------------------------------------------------'
  print,'line #:',il
  print,'-----------------------------------------------------------'
  print,'                   r (Rsun)        th (deg)        ph (deg)'
  print,'-----------------------------------------------------------'
  print,'Line  start:',str/1.d,stth/!dtor,stph/!dtor
  print,'Leg-1 start:',r1/1.d,th1/!dtor/1.d,ph1/!dtor/1.d
  print,'Leg-2 start:',r2/1.d,th2/!dtor/1.d,ph2/!dtor/1.d
  print,'Dist. Leg-1:',d1/1.d
  print,'Dist. Leg-2:',d2/1.d
  if d1 le d2 then switching='no'
  if d1 gt d2 then switching='yes'
  print,'Switch legs?: '+switching
  print,'-----------------------------------------------------------'
  ;if switching eq 'YES' then stop ; Descomentar para ver todo switch
  if d1 gt .5 then STOP           ; Descomentar para ver un switch
                                  ; para el cual la Leg1 empezaba
                                  ; a más de 1/2 Rsun del start point.
  skiptestswitch:

  ; SOLO si d1>d2 INTERCAMBIAR las piernas, sino dejarlas como están.
  ; Así, al salir de esta rutina leg1 será SIEMPRE la pierna del START POINT.
  if d1 gt d2 then begin

  rad_tm = rad_l1
  lat_tm = lat_l1
  lon_tm = lon_l1
   Ne_tm =  Ne_l1  
   Tm_tm =  Tm_l1
   WT_tm =  WT_l1
   scoreR_tm = scoreR_l1

  rad_l1 = rad_l2
  lat_l1 = lat_l2
  lon_l1 = lon_l2
   Ne_l1 =  Ne_l2  
   Tm_l1 =  Tm_l2
   WT_l1 =  WT_l2
   scoreR_l1 = scoreR_l2

  rad_l2 = rad_tm
  lat_l2 = lat_tm
  lon_l2 = lon_tm
   Ne_l2 =  Ne_tm  
   Tm_l2 =  Tm_tm
   WT_l2 =  WT_tm
   scoreR_l2 = scoreR_tm
   switching ='yes' ;FEDE
  
  endif

  return
  end

pro fithslinear,rr,yy,Rmin,Rmax,epsN,A,r2
p=where(rr ge Rmin and rr le Rmax)
;ep=0.01
rr=rr(p)
yy=yy(p)
xx= 1/rr
zz= alog(yy)
A = [0.,0.]
fit_ts,xx,zz,epsN,a1,b1,F
Alin=[b1,a1]
A[0] = exp( a1 + b1)
A[1] = a1
fit_ts =  A[0] * exp( -A[1]*(1.-1./rr) )

;Alin = linfit(xx,zz)
;A[0] = exp( Alin[0]+ Alin[1])
;A[1] = Alin[1]
;fit =  A[0] * exp( -A[1]*(1.-1./rr) ) 

meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit_ts)^2 )
r2    = 1.-SSerr/SStot 

return
end

pro fitr0,rr,yy,A,r2   ;<---
A = linfit(rr,yy)
fit =  A[0]+A[1]*rr 
meanyy = mean(yy)
SStot  = total( (yy-meanyy)^2 )
SSerr  = total( (yy-fit    )^2 )
r2     = 1.-SSerr/SStot 
return 
end

pro fiths,rr,yy,Rmin,Rmax,A,r2
p=where(rr ge Rmin and rr le Rmax)
rr=rr(p)
yy=yy(p)
nr=n_elements(rr)
ia=0
ib=fix(float(nr)/2.)
ra=rr(ia)
rb=rr(ib)
ya=yy(ia)
yb=yy(ib)
A1 = alog(ya/yb)/(1./ra-1./rb) ;< Preguntar                                                                                                                                                                                                  
A0 = yb / exp(-A1*(1.-1./rb))
A=[A0,A1]
ww=yy*0.+1. ;1./yy ;ww=1 entonces no peso                                                                                                                                                                                                 

fit = curvefit (rr,yy,ww,A,Sigma,function_name='function_hs')
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot
return
end


pro fithsEr,rr,yy,A,r2
xx= 1/rr
zz= alog(yy)
A = [0.,0.]
Alin = linfit(xx,zz)
A[0] = exp( Alin[0]+ Alin[1])
A[1] = Alin[1]
fit =  A[0] * exp( -A[1]*(1.-1./rr) ) 
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot 
return
end

pro fitEr,rr,yy,A,r2  ;<--- ajusto exponencial común
ep=0.01
zz= alog(yy)
A = [0.,0.]
;fit_ts,rr,zz,ep,a1,b1,F
;Alin=[b1,a1]
;A[0] = exp(b1)
;A[1] = a1
Alin = linfit(rr,zz)
A[0] = exp( Alin[0])
A[1] = Alin[1]
fit =  A[0] * exp( A[1]*(rr) ) 
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot 
return
end


pro fitTemp,rr,yy,Rmin,Rmax,A,r2
p=where(rr ge Rmin and rr le Rmax)
rr=rr(p)
yy=yy(p)
A = linfit(rr,yy)
fit =  A[0]+A[1]*rr 
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot 
return 
end


pro fit_ts,x,y,eps,a1,b1,F
                                                  
n=n_elements(x)
nn=n*(n-1)*0.5

xx1=x#replicate(1,n)
yy1=y#replicate(1,n)

xx=xx1
yy=yy1

yb=fltarr(n,n)-100.
xb=fltarr(n,n)-100.
ts=fltarr(n,n)-100.
bb=fltarr(n)-100.

for j=0,n-1 do begin
   for i=0,n-1 do begin
      if j gt i then begin
      xb(j,i)=xx(j,i)-x(i)
      yb(j,i)=yy(j,i)-y(i)
      ts(j,i)=yb(j,i)/xb(j,i)
;if xb(j,i) eq 0 then stop
   endif else begin
         if j lt i then ts(j,i)=0
      endelse
   endfor
endfor

for j=0,n-1 do begin
   for i=0,n-1 do begin
      if j gt i then begin
         ts(j,i)=ts(j,i)
      endif else begin
         if j le i then ts(j,i)=0
      endelse
   endfor
endfor
ts1=ts[where(ts ne 0)]

infin = !values.f_infinity
ok = where(ts1 ne infin)
ts1 = ts1(ok)

a1=median(ts1)

for i=0,n-1 do begin
   bb(i)=y(i)-a1*x(i)
endfor
b1=median(bb)

yfit = b1 + a1*x

;Calidad de ajuste                                  
                
nfit = n_elements(yfit)
;eps = ep * median(yfit)
   yru = yfit + eps
   yrd = yfit - eps

 Yin = fltarr(n)-100.
for i=0,n-1 do begin
   if y(i) ge yrd(i) and y(i) le yru(i) then  Yin(i) = 1. 
   if y(i) lt yrd(i) or  y(i) gt yru(i) then  Yin(i) = 0.
endfor
nok=where(Yin eq 1.)

F=float(n_elements(Nok))/float(nfit)

return 
end

pro fitEh,ss,yy,smin,smax,A,r2

A= [-555.,-555]
r2 = -555.
p=where(ss ge smin and ss le smax and yy gt 0.)

if n_elements(p) ge 5 and 2*n_elements(p) gt n_elements(yy) then begin
ss=ss(p)
yy=yy(p)
zz = alog(yy) 
Alin = linfit(ss,zz)
A[0] = exp (Alin[0])
A[1] = 1./Alin[1]
fit =  A[0] * exp(ss/A[1]) 
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot 
endif

return
end


pro footpoint_map,box=box,footlat_c,footlon_c,opclstat_c,filelabel=filelabel,indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH

if not keyword_set(box) then box=[0.,-90.,360.,+90.]

!P.CHARTHICK=6
!p.charsize=2.5

; if rotacion eq 'CR2081' then trace_openclose,rotacion+'_90X180blines_r_',7,1.0,lonvA,latvA,/gng

; colors
coltb = 12
green=20
blue=100-12
dblue=100
red=200-36
dred=200

coltb=39
; Create custom made symbol (psym=8) for scatter plots
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=5.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL

iopen    = where(opclstat_c eq 0.)
ilarge   = where(opclstat_c eq 1.)
ismall   = where(opclstat_c eq 2.)

ps1,'./newfigs/'+filelabel+'_footpoint-map.eps',0
  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1

thick=3
;Device, SET_FONT = 'Bookman-LightItalic'
;Device, /BKMAN, /LIGHT
;thick=3

  plot,footlon_c,footlat_c,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
;     title='Localizacion Fisica de Loops en la Base',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0
      title='Physical location of the footpoints',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0

loadct,coltb
 if iopen (0) ne -1 then oplot,footlon_c(iopen ),footlat_c(iopen ),color=200  ,th=2,psym=8
 if ilarge(0) ne -1 then oplot,footlon_c(ilarge),footlat_c(ilarge),color=245   ,th=2,psym=8
 if ismall(0) ne -1 then oplot,footlon_c(ismall),footlat_c(ismall),color=28  ,th=2,psym=8

loadct,0

!p.multi = 0
!P.CHARTHICK=0

;oplot,lonvA,latvA,th=3,psym=8

ps2
return
end

pro trace_openclose,string,ir,height,gng=gng,mdi=mdi,lonvA,latvA
common mapopcl,mapoc
if keyword_set(gng) then load_mapoc,string,height,/gng
if keyword_set(mdi) then load_mapoc,string,height,/mdi
map2d=reform(mapoc(ir,*,*))
map2d=rotate(map2d,4)
Nlon=(size(map2d))(1)
Nlat=(size(map2d))(2)
dt = 2.
lon =   0. + dt/2. + dt * findgen(Nlon)
lat = -90. + dt/2. + dt * findgen(Nlat)
flag=0
for ilon=0,nlon-1 do begin
for ilat=1,nlat-1 do begin
if map2d(ilon,ilat) ne map2d(ilon,ilat-1) then begin
 lonv = lon(ilon)
 latv = mean([lat(ilat-1),lat(ilat)])
 if flag eq 0 then begin
 lonvA = [lonv]
 latvA = [latv]
 flag=1
 endif else begin
 lonvA = [lonvA,lonv]
 latvA = [latvA,latv]
 endelse
endif
endfor
endfor
return
end

pro fitcuadrtemp,rr,yy,Rmin,Rmax,A,r2,T0,gradT
p=where(rr ge Rmin and rr le Rmax)
rr=rr(p)
yy=yy(p)
A2 = T0
A1 = gradT
A0 = 0.0
A=[A0,A1,A2]
ww=yy*0.+1. ;1./yy
fit=curvefit(rr,yy,ww,A,Sigma,function_name='function_cuadr')
meanyy= mean(yy)
SStot = total( (yy-meanyy)^2 )
SSerr = total( (yy-fit   )^2 )
r2    = 1.-SSerr/SStot
return
end

PRO function_cuadr, X, A, F, pder   
   F = A[0]*X^2 + A[1]*X + A[2]
   IF N_PARAMS() GE 4 then pder=[ [X^2], [X], [replicate(1.0, N_ELEMENTS(X))]]
END

pro filter,i
common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s

common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base

common stat_filter,Nlegs_c,Nemean_c,Tmmean_c,WTmean_c,Nestddev_c,Tmstddev_c,WTstddev_c,loop_length_c,betamean_c,betaapex_c,Bmean_c,Br0_c
common stat_filter2,opclstat_c,lambda_N_c,lambda_p_c,Ne0_c,p0_c,Tefit_c,gradT_c,r2N_c,r2P_c,r2T_c,indexloop_c,leg_status_c,Tm0_c,Tm0s_c,Tm0_ts_c,gradT_ts_c,FTr_ts_c,Tm0s_ts_c,dTmds_ts_c,FTs_ts_c,Tm0s5_ts_c,dTmds5_ts_c,FTs5_ts_c
common stat_filter3,Eh_c,sH_c,r2sH_c,Phir_c,Fcb_c,Ner0_c,Ner1_c,Ner2_c,Ner3_c,TmR1_c,NR1_c,Erlin_F_c,Erlin_d_c,Erlin_o_c,Erlin_r0_c,phir_lin_c
common stat_filter4,r2Tcuadr_c,Acuadr_a_c, s_r0_a_c,dTmds_c,r2Ts_c,Netech_c,Ne0_s_c,lambda_N_s_c
common stat_filter5,strad_c,stlat_c,stlon_c,footrad_c,footlat_c,footlon_c,Rp_rad_c,Rp_lat_c,Rp_lon_c
common stat_filter6,r2Er_c,Phirfit_c,eplegT_c,deltaS_c,Smaxxx_c,Sminnn_c,B_base_c,Fcb5_c

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_loop,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTr15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common label,T_label,Er_label

      Nlegs_c =       Nlegs(i)
     Nemean_c =      Nemean(i)
     Tmmean_c =      Tmmean(i)
     WTmean_c =      WTmean(i)
   Nestddev_c =    Nestddev(i)
   Tmstddev_c =    Tmstddev(i)
   WTstddev_c =    Wtstddev(i)
loop_length_c = loop_length(i)
   betamean_c =    betamean(i)
   betaapex_c =    betaapex(i)
      Bmean_c =       Bmean(i)
        Br0_c =         Br0(i)

     B_base_c =      B_base(i)

   opclstat_c =    opclstat(i)
   lambda_N_c =    lambda_N(i)
   lambda_p_c =    lambda_p(i)
        Ne0_c =         Ne0(i)
         p0_c =          p0(i)  
      Tefit_c =       Tefit(i)
      gradT_c =       gradT(i)
        r2N_c =         r2N(i)
        r2P_c =         r2P(i)
        r2T_c =         r2T(i)
  indexloop_c =   indexloop(i)
 leg_status_c =  leg_status(i)
        Tm0_c =         Tm0(i)
       Tm0s_c =        Tm0s(i)

         Eh_c =          Eh(i)
         sH_c =          sH(i)
       r2sH_c =        r2sH(i)
       Phir_c =        Phir(i)
        Fcb_c =         Fcb(i)

      Ne0_s_c =       Ne0_s(i) 
 lambda_N_s_c =  lambda_N_s(i)
     Netech_c =      Netech(i)

       TmR1_c =        TmR1(i)
        NR1_c =         NR1(i)
     Tm0_ts_c =      Tm0_ts(i)
   gradT_ts_c =    gradT_ts(i)
     FTr_ts_c =      FTr_ts(i)
    Tm0s_ts_c =     Tm0s_ts(i)
   dTmds_ts_c =    dTmds_ts(i)
     FTs_ts_c =      FTs_ts(i)

;===============================

   Tm0s5_ts_c =  Tm0s5_ts(i)
  dTmds5_ts_c = dTmds5_ts(i)
    FTs5_ts_c =   FTs5_ts(i)

      Fcb5_c =  Fcb5(i)

   Erlin_F_c =  Erlin_F(i)
   Erlin_d_c =  Erlin_d(i)
   Erlin_o_c =  Erlin_o(i)
  Erlin_r0_c = Erlin_r0(i)
  phir_lin_c = phir_lin(i)

;================================

     eplegT_c =      eplegT(i)
     deltaS_c =      deltaS(i)
     Smaxxx_c =      Smaxxx(i)
     Sminnn_c =      Sminnn(i) 

   r2Tcuadr_c =    r2Tcuadr(i)
   Acuadr_a_c =    Acuadr_a(i)
     s_r0_a_c =      s_r0_a(i)
      dTmds_c =       dTmds(i)
       r2Ts_c =        r2Ts(i)

    footlat_c =     footlat(i)
    footlon_c =     footlon(i)
    footrad_c =     footrad(i)
      strad_c =       strad(i)
      stlat_c =       stlat(i)
      stlon_c =       stlon(i)

     Rp_rad_c =      Rp_rad(i)
     Rp_lat_c =      Rp_lat(i)
     Rp_lon_c =      Rp_lon(i)

        r2Er_c =       r2Er(i)
     Phirfit_c =    Phirfit(i)
        Ner0_c =       Ner0(i)

        Ner1_c =       Ner1(i)
        Ner2_c =       Ner2(i)
        Ner3_c =       Ner3(i)

        r0=1.025
        Ner0_c = Ne0_c * exp((-1./lambda_N_c)*(1.-1./r0)) 

        r1=1.035
        Ner1_c = Ne0_c * exp((-1./lambda_N_c)*(1.-1./r1)) 

        r2=1.075
        Ner2_c = Ne0_c * exp((-1./lambda_N_c)*(1.-1./r2)) 

        r3=1.115
        Ner3_c = Ne0_c * exp((-1./lambda_N_c)*(1.-1./r3)) 


return
end

pro histoplot,datar,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename

ps1,'./newfigs/'+filename+'.eps',0
  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1
  DEVICE,/TIMES, FONT_INDEX=4

fr = histogram(datar,min=min,max=max,nbins=nbins,locations=vbinr) & fr = fr / total(fr)
maxy=max(fr)
;maxy=0.3

 plot,vbinr,fr,psym=10,charsize=2.5,xtitle=xtit,ytitle=ytit,title=tit,yr=[0,maxy],xr=[min,max],xstyle=1,/nodata,thick=3,charthick=2.25,Font=0

loadct,12

verde= 20
azul=100
rojo=200
oplot,vbinr,fr,psym=10,th=10,color=rojo
loadct,0

;======= Rebin full corona
;oplot,[0.1,0.1],[0,1] 
;oplot,[0.4,0.4],[0,1]
;oplot,[0.7,0.7],[0,1] ;small
;oplot,[4. ,4. ],[0,1] ;small
;oplot,[0.65,0.65],[0,1]
;oplot,[1.1,1.1],[0,1]
;oplot,[2.5,2.5],[0,1]
;=======

;=========== Low full
;oplot,[0.1,0.1],[0,1]
;oplot,[0.3,0.3],[0,1]
;oplot,[0.45,0.45],[0,1]
;oplot,[0.7,0.7],[0,1]
;oplot,[2.5,2.5],[0,1]
;===========

;=========== Low full
;oplot,[0.1,0.1],[0,1]
;oplot,[0.3,0.3],[0,1]
;oplot,[0.4,0.4],[0,1]
;oplot,[0.6,0.6],[0,1]
;oplot,[2.5,2.5],[0,1]
;===========

;oplot,[0.1,0.1],[0,1]
;oplot,[0.37,0.37],[0,1]
;oplot,[0.524,0.524],[0,1]
;oplot,[0.762,0.762],[0,1]
;oplot,[1.13,1.13],[0,1]
;oplot,[5.,5.],[0,1]

;oplot,[0.00,0.00],[0,1]
;oplot,[0.44,0.44],[0,1]
;oplot,[0.56,0.56],[0,1]
;oplot,[0.68,0.68],[0,1]
;oplot,[0.87,0.87],[0,1]
;oplot,[2.00,2.00],[0,1]

avgr        = median(datar)
stdevr      =  stdev(datar);/avgr

Nr = N_elements(datar)

xyouts,.9-[.17,.06],0.8*[1,1],['!8m!3','!9s!3'],/normal,charsize=2,charthick=2,Font=0

loadct,12
negro=0
xyouts,.9-[.2,.10],0.7*[1,1],[strmid(string(avgr),4,5),strmid(string(stdevr),4,5)],/normal,color=[negro,negro],charsize=2,charthick=2,Font=0
loadct,0

xyouts,.95-[.20],0.35*[1],['N ='+strmid(string(Nr),6,8)],/normal,charsize=2,charthick=2,Font=0

ps2

return
end


pro pegatina

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base

common stat_filter,Nlegs_c,Nemean_c,Tmmean_c,WTmean_c,Nestddev_c,Tmstddev_c,WTstddev_c,loop_length_c,betamean_c,betaapex_c,Bmean_c,Br0_c
common stat_filter2,opclstat_c,lambda_N_c,lambda_p_c,Ne0_c,p0_c,Tefit_c,gradT_c,r2N_c,r2P_c,r2T_c,indexloop_c,leg_status_c,Tm0_c,Tm0s_c,Tm0_ts_c,gradT_ts_c,FTr_ts_c,Tm0s_ts_c,dTmds_ts_c,FTs_ts_c,Tm0s5_ts_c,dTmds5_ts_c,FTs5_ts_c  
common stat_filter3,Eh_c,sH_c,r2sH_c,Phir_c,Fcb_c,Ner0_c,Ner1_c,Ner2_c,Ner3_c,TmR1_c,NR1_c,Erlin_F_c,Erlin_d_c,Erlin_o_c,Erlin_r0_c,phir_lin_c
common stat_filter4,r2Tcuadr_c,Acuadr_a_c, s_r0_a_c,dTmds_c,r2Ts_c,Netech_c,Ne0_s_c,lambda_N_s_c
common stat_filter5,strad_c,stlat_c,stlon_c,footrad_c,footlat_c,footlon_c,Rp_rad_c,Rp_lat_c,Rp_lon_c
common stat_filter6,r2Er_c,Phirfit_c,eplegT_c,deltaS_c,Smaxxx_c,Sminnn_c,B_base_c,Fcb5_c

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_loop,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTs15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common label,T_label,Er_label

  rsun = 6.955e10    ; cm

;i = where(opclstat eq 2)
 i = where(opclstat gt 0.)

filter,i

ISO_c = (abs(dTmds_ts_c)*rsun*abs(Smaxxx_c - Sminnn_c))/(2*eplegT_c)

;openw,2,'Long_phi.txt'
;printf,2,format='(''Longitud'',''  Heating'')'

 Nlegs = n_elements(i)*1L
Nloops = Nlegs/2 

     Phih_loop = fltarr(Nloops)
       Long_cm = fltarr(Nloops)
        Phih_L = fltarr(Nloops)

     Phir_loop = fltarr(Nloops)
     rad1_loop = fltarr(Nloops)
     rad2_loop = fltarr(Nloops)
     lat1_loop = fltarr(Nloops)
     lat2_loop = fltarr(Nloops)
     lon1_loop = fltarr(Nloops)
     lon2_loop = fltarr(Nloops)
        L_loop = fltarr(Nloops)
 opclstat_loop = fltarr(Nloops)
    r2Er1_loop = fltarr(Nloops)
    r2Er2_loop = fltarr(Nloops)
   Tmmean_loop = fltarr(Nloops)
   Nemean_loop = fltarr(Nloops)

      Ne0_s_loop = fltarr(Nloops) 
 lambda_N_s_loop = fltarr(Nloops)
     Netech_loop = fltarr(Nloops)

 lambda_N_loop = fltarr(Nloops)
     r2N1_loop = fltarr(Nloops)
     r2N2_loop = fltarr(Nloops)

      Fcb_loop = fltarr(Nloops)
   dTmds1_loop = fltarr(Nloops)
   dTmds2_loop = fltarr(Nloops)
r2Tcuadr1_loop = fltarr(Nloops)
r2Tcuadr2_loop = fltarr(Nloops)
     r2T1_loop = fltarr(Nloops)
     r2T2_loop = fltarr(Nloops)
    Ner01_loop = fltarr(Nloops)
    Ner02_loop = fltarr(Nloops)
    Ner0m_loop = fltarr(Nloops)

    Ner11_loop = fltarr(Nloops)
    Ner12_loop = fltarr(Nloops)
    Ner21_loop = fltarr(Nloops)
    Ner22_loop = fltarr(Nloops)
    Ner31_loop = fltarr(Nloops)
    Ner32_loop = fltarr(Nloops)

  B_basem_loop = fltarr(Nloops)

  Rp_rad1_loop = fltarr(Nloops)
  Rp_rad2_loop = fltarr(Nloops)
  Rp_lat1_loop = fltarr(Nloops)
  Rp_lat2_loop = fltarr(Nloops)
  Rp_lon1_loop = fltarr(Nloops)
  Rp_lon2_loop = fltarr(Nloops)

  Tm01_ts_loop = fltarr(Nloops)
gradT1_ts_loop = fltarr(Nloops)
  FTr1_ts_loop = fltarr(Nloops)
 Tm0s1_ts_loop = fltarr(Nloops)
dTmds1_ts_loop = fltarr(Nloops)
  FTs1_ts_loop = fltarr(Nloops)

  Tm02_ts_loop = fltarr(Nloops)
gradT2_ts_loop = fltarr(Nloops)
  FTr2_ts_loop = fltarr(Nloops)
 Tm0s2_ts_loop = fltarr(Nloops)
dTmds2_ts_loop = fltarr(Nloops)
  FTs2_ts_loop = fltarr(Nloops)

;==============================

 Tm0s15_ts_loop = fltarr(Nloops)
dTmds15_ts_loop = fltarr(Nloops)
  FTs15_ts_loop = fltarr(Nloops)
 Tm0s25_ts_loop = fltarr(Nloops)
dTmds25_ts_loop = fltarr(Nloops)
  FTs25_ts_loop = fltarr(Nloops)

      Fcb5_loop = fltarr(Nloops)

  Erlin1_F_loop = fltarr(Nloops)
  Erlin1_d_loop = fltarr(Nloops)
  Erlin1_o_loop = fltarr(Nloops)
 Erlin1_r0_loop = fltarr(Nloops)
 phir1_lin_loop = fltarr(Nloops)

  Erlin2_F_loop = fltarr(Nloops)
  Erlin2_d_loop = fltarr(Nloops)
  Erlin2_o_loop = fltarr(Nloops)
 Erlin2_r0_loop = fltarr(Nloops)
 phir2_lin_loop = fltarr(Nloops)

;==============================

  eplegT1_loop = fltarr(Nloops)
  eplegT2_loop = fltarr(Nloops)
  deltaS1_loop = fltarr(Nloops)
  deltaS2_loop = fltarr(Nloops)
 
  Smaxxx1_loop = fltarr(Nloops)
  Smaxxx2_loop = fltarr(Nloops)
  s_r0_a1_loop = fltarr(Nloops)
  s_r0_a2_loop = fltarr(Nloops)
  Sminnn1_loop = fltarr(Nloops)
  Sminnn2_loop = fltarr(Nloops)

    TmR11_loop = fltarr(Nloops)
    TmR12_loop = fltarr(Nloops)
     NR11_loop = fltarr(Nloops)
     NR12_loop = fltarr(Nloops)

      ISO_loop = fltarr(Nloops)

iloop = 0L
for ileg=0L,Nlegs-1,2 do begin
     Phir_loop(iloop) =        Phirfit_c(ileg) + Phirfit_c(ileg+1)
     rad1_loop(iloop) =        footrad_c(ileg)
     rad2_loop(iloop) =        footrad_c(ileg+1)
     lat1_loop(iloop) =        footlat_c(ileg)
     lat2_loop(iloop) =        footlat_c(ileg+1)
     lon1_loop(iloop) =        footlon_c(ileg)
     lon2_loop(iloop) =        footlon_c(ileg+1)
 opclstat_loop(iloop) =       opclstat_c(ileg)
        L_loop(iloop) =    loop_length_c(ileg)
    r2Er1_loop(iloop) =           r2Er_c(ileg)
    r2Er2_loop(iloop) =           r2Er_c(ileg+1)
   Tmmean_loop(iloop) = mean([  Tmmean_c(ileg),  Tmmean_c(ileg+1)])   
   Nemean_loop(iloop) = mean([  Nemean_c(ileg),  Nemean_c(ileg+1)])

 lambda_N_loop(iloop) = mean([lambda_N_c(ileg),lambda_N_c(ileg+1)])
     r2N1_loop(iloop) =            r2N_c(ileg)
     r2N2_loop(iloop) =            r2N_c(ileg+1)

     Ne0_s_loop(iloop) = mean([     Ne0_s_c(ileg),     Ne0_s_c(ileg+1)])
lambda_N_s_loop(iloop) = mean([lambda_N_s_c(ileg),lambda_N_s_c(ileg+1)])
    Netech_loop(iloop) = mean([    Netech_c(ileg),    Netech_c(ileg+1)])

      Fcb_loop(iloop) =            Fcb_c(ileg) +    Fcb_c(ileg+1)
   dTmds1_loop(iloop) =          dTmds_c(ileg)
   dTmds2_loop(iloop) =          dTmds_c(ileg+1)
r2Tcuadr1_loop(iloop) =       r2Tcuadr_c(ileg)
r2Tcuadr2_loop(iloop) =       r2Tcuadr_c(ileg+1)
     r2T1_loop(iloop) =            r2T_c(ileg)
     r2T2_loop(iloop) =            r2T_c(ileg+1)
    Ner01_loop(iloop) =           Ner0_c(ileg)
    Ner02_loop(iloop) =           Ner0_c(ileg+1)
    Ner0m_loop(iloop) = mean([    Ner0_c(ileg),    Ner0_c(ileg+1)])

    Ner11_loop(iloop) =         Ner1_c(ileg)
    Ner12_loop(iloop) =         Ner1_c(ileg+1)
    Ner21_loop(iloop) =         Ner2_c(ileg)
    Ner22_loop(iloop) =         Ner2_c(ileg+1)
    Ner31_loop(iloop) =         Ner3_c(ileg)
    Ner32_loop(iloop) =         Ner3_c(ileg+1)

  B_basem_loop(iloop) = mean([B_base_c(ileg),    B_base_c(ileg+1)])

  Rp_rad1_loop(iloop) =       Rp_rad_c(ileg)
  Rp_rad2_loop(iloop) =       Rp_rad_c(ileg+1)
  Rp_lat1_loop(iloop) =       Rp_lat_c(ileg)
  Rp_lat2_loop(iloop) =       Rp_lat_c(ileg+1)
  Rp_lon1_loop(iloop) =       Rp_lon_c(ileg)
  Rp_lon2_loop(iloop) =       Rp_lon_c(ileg+1)

         Tm01_ts_loop(iloop) =    Tm0_ts_c(ileg) 
       gradT1_ts_loop(iloop) =  gradT_ts_c(ileg)
         FTr1_ts_loop(iloop) =    FTr_ts_c(ileg)
        Tm0s1_ts_loop(iloop) =   Tm0s_ts_c(ileg) 
       dTmds1_ts_loop(iloop) =  dTmds_ts_c(ileg)
         FTs1_ts_loop(iloop) =    FTs_ts_c(ileg)

         Tm02_ts_loop(iloop) =    Tm0_ts_c(ileg+1) 
       gradT2_ts_loop(iloop) =  gradT_ts_c(ileg+1)
         FTr2_ts_loop(iloop) =    FTr_ts_c(ileg+1)
        Tm0s2_ts_loop(iloop) =   Tm0s_ts_c(ileg+1) 
       dTmds2_ts_loop(iloop) =  dTmds_ts_c(ileg+1)
         FTs2_ts_loop(iloop) =    FTs_ts_c(ileg+1)

;======================================================

        Tm0s15_ts_loop(iloop) =   Tm0s5_ts_c(ileg) 
       dTmds15_ts_loop(iloop) =  dTmds5_ts_c(ileg)
         FTs15_ts_loop(iloop) =    FTs5_ts_c(ileg)
        Tm0s25_ts_loop(iloop) =   Tm0s5_ts_c(ileg+1) 
       dTmds25_ts_loop(iloop) =  dTmds5_ts_c(ileg+1)
         FTs25_ts_loop(iloop) =    FTs5_ts_c(ileg+1)

             Fcb5_loop(iloop) = Fcb5_c(ileg) + Fcb5_c(ileg+1)

         Erlin1_F_loop(iloop) = Erlin_F_c(ileg)
         Erlin1_d_loop(iloop) = Erlin_d_c(ileg)
         Erlin1_o_loop(iloop) = Erlin_o_c(ileg)
        Erlin1_r0_loop(iloop) = Erlin_r0_c(ileg)
        phir1_lin_loop(iloop) = phir_lin_c(ileg)

         Erlin2_F_loop(iloop) = Erlin_F_c(ileg+1)
         Erlin2_d_loop(iloop) = Erlin_d_c(ileg+1)
         Erlin2_o_loop(iloop) = Erlin_o_c(ileg+1)
        Erlin2_r0_loop(iloop) = Erlin_r0_c(ileg+1)
        phir2_lin_loop(iloop) = phir_lin_c(ileg+1)

;====================================================

         eplegT1_loop(iloop) =      eplegT_c(ileg)
         eplegT2_loop(iloop) =      eplegT_c(ileg+1)
         deltaS1_loop(iloop) =      deltaS_c(ileg)
         deltaS2_loop(iloop) =      deltaS_c(ileg+1)

         Smaxxx1_loop(iloop) =    Smaxxx_c(ileg)
         Smaxxx2_loop(iloop) =    Smaxxx_c(ileg+1)
         s_r0_a1_loop(iloop) =    s_r0_a_c(ileg)
         s_r0_a2_loop(iloop) =    s_r0_a_c(ileg+1)
         Sminnn1_loop(iloop) =    Sminnn_c(ileg)
         Sminnn2_loop(iloop) =    Sminnn_c(ileg+1)

    TmR11_loop(iloop) =         TmR1_c(ileg)
    TmR12_loop(iloop) =         TmR1_c(ileg+1)
     NR11_loop(iloop) =          NR1_c(ileg)
     NR12_loop(iloop) =          NR1_c(ileg+1)

     Phih_loop(iloop) = Phir_loop(iloop) - Fcb_loop(iloop)

       Long_cm(iloop) = L_loop(iloop)*rsun
        Phih_L(iloop) = Phih_loop(iloop)/Long_cm(iloop) 

      ISO_loop(iloop) = (ISO_c(ileg) + ISO_c(ileg+1))*0.5

;printf,2, format='(e10.2,e10.2)',Long_cm(iloop),Phih_L(iloop)

iloop=iloop+1
endfor
;stop
;close,2

return
end

pro filter_loop,i

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_lopo,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTs15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common loops_filter,Phir_loop_c,rad1_loop_c,rad2_loop_c,lat1_loop_c,lat2_loop_c,lon1_loop_c,lon2_loop_c,L_loop_c,opclstat_loop_c,r2Er1_loop_c,r2Er2_loop_c,Tmmean_loop_c,Fcb_loop_c,dTmds1_loop_c,dTmds2_loop_c,r2Tcuadr1_loop_c,r2Tcuadr2_loop_c,r2T1_loop_c,r2T2_loop_c,Ner01_loop_c,Ner02_loop_c,Ner11_loop_c,Ner12_loop_c,Ner21_loop_c,Ner22_loop_c,Ner31_loop_c,Ner32_loop_c,Rp_rad1_loop_c,Rp_lat1_loop_c,Rp_lon1_loop_c,Rp_rad2_loop_c,Rp_lat2_loop_c,Rp_lon2_loop_c,TmR11_loop_c,TmR12_loop_c,NR11_loop_c,NR12_loop_c,Nemean_loop_c,Long_cm_c,Phih_L_c,Ner0m_loop_c,lambda_N_loop_c,r2N1_loop_c,r2N2_loop_c,Tm01_ts_loop_c,gradT1_ts_loop_c,FTr1_ts_loop_c,Tm0s1_ts_loop_c,dTmds1_ts_loop_c,FTs1_ts_loop_c,Tm02_ts_loop_c,gradT2_ts_loop_c,FTr2_ts_loop_c,Tm0s2_ts_loop_c,dTmds2_ts_loop_c,FTs2_ts_loop_c,eplegT1_loop_c,eplegT2_loop_c,deltaS1_loop_c,deltaS2_loop_c,Smaxxx1_loop_c,Smaxxx2_loop_c,Sminnn1_loop_c,Sminnn2_loop_c,ISO_loop_c,B_basem_loop_c,Netech_loop_c,Ne0_s_loop_c,lambda_N_s_loop_c,Tm0s15_ts_loop_c,dTmds15_ts_loop_c,FTs15_ts_loop_c,Tm0s25_ts_loop_c,dTmds25_ts_loop_c,FTs25_ts_loop_c,Erlin1_F_loop_c,Erlin1_d_loop_c,Erlin1_o_loop_c,Erlin1_r0_loop_c,phir1_lin_loop_c,Erlin2_F_loop_c,Erlin2_d_loop_c,Erlin2_o_loop_c,Erlin2_r0_loop_c,phir2_lin_loop_c,Fcb5_loop_c

common label,T_label,Er_label

     Phir_loop_c =     Phir_loop(i)
     rad1_loop_c =     rad1_loop(i)
     rad2_loop_c =     rad2_loop(i)
     lat1_loop_c =     lat1_loop(i)
     lat2_loop_c =     lat2_loop(i)
     lon1_loop_c =     lon1_loop(i)
     lon2_loop_c =     lon2_loop(i)
        L_loop_c =        L_loop(i)
 opclstat_loop_c = opclstat_loop(i)
    r2Er1_loop_c =    r2Er1_loop(i) 
    r2Er2_loop_c =    r2Er2_loop(i)
   Tmmean_loop_c =   Tmmean_loop(i)
   Nemean_loop_c =   Nemean_loop(i)

 lambda_N_loop_c = lambda_N_loop(i)
     r2N1_loop_c =     r2N1_loop(i)
     r2N2_loop_c =     r2N2_loop(i)

     Ne0_s_loop_c =      Ne0_s_loop(i)
lambda_N_s_loop_c = lambda_N_s_loop(i)
    Netech_loop_c =     Netech_loop(i) 

      Fcb_loop_c =      Fcb_loop(i)
   dTmds1_loop_c =   dTmds1_loop(i)
   dTmds2_loop_c =   dTmds2_loop(i)
r2Tcuadr1_loop_c =r2Tcuadr1_loop(i)
r2Tcuadr2_loop_c =r2Tcuadr2_loop(i)
     r2T1_loop_c =     r2T1_loop(i)
     r2T2_loop_c =     r2T2_loop(i)
    Ner01_loop_c =    Ner01_loop(i)
    Ner02_loop_c =    Ner02_loop(i)
    Ner0m_loop_c =    Ner0m_loop(i)

    Ner11_loop_c =    Ner11_loop(i)
    Ner12_loop_c =    Ner12_loop(i)
    Ner21_loop_c =    Ner21_loop(i)
    Ner22_loop_c =    Ner22_loop(i)
    Ner31_loop_c =    Ner31_loop(i)
    Ner32_loop_c =    Ner32_loop(i)

  B_basem_loop_c =  B_basem_loop(i)

   Rp_rad1_loop_c =   Rp_rad1_loop(i)
   Rp_rad2_loop_c =   Rp_rad2_loop(i)
   Rp_lat1_loop_c =   Rp_lat1_loop(i)
   Rp_lat2_loop_c =   Rp_lat2_loop(i)
   Rp_lon1_loop_c =   Rp_lon1_loop(i)
   Rp_lon2_loop_c =   Rp_lon2_loop(i)

   Tm01_ts_loop_c =   Tm01_ts_loop(i)
 gradT1_ts_loop_c = gradT1_ts_loop(i)
   FTr1_ts_loop_c =   FTr1_ts_loop(i)
  Tm0s1_ts_loop_c =  Tm0s1_ts_loop(i)
 dTmds1_ts_loop_c = dTmds1_ts_loop(i)
   FTs1_ts_loop_c =   FTs1_ts_loop(i)
   Tm02_ts_loop_c =   Tm02_ts_loop(i)
 gradT2_ts_loop_c = gradT2_ts_loop(i)
   FTr2_ts_loop_c =   FTr2_ts_loop(i)
  Tm0s2_ts_loop_c =  Tm0s2_ts_loop(i)
 dTmds2_ts_loop_c = dTmds2_ts_loop(i)
   FTs2_ts_loop_c =   FTs2_ts_loop(i)

;========================================

  Tm0s15_ts_loop_c =  Tm0s15_ts_loop(i)
 dTmds15_ts_loop_c = dTmds15_ts_loop(i)
   FTs15_ts_loop_c =   FTs15_ts_loop(i)
  Tm0s25_ts_loop_c =  Tm0s25_ts_loop(i)
 dTmds25_ts_loop_c = dTmds25_ts_loop(i)
   FTs25_ts_loop_c =   FTs25_ts_loop(i)

       Fcb5_loop_c = Fcb5_loop(i)

   Erlin1_F_loop_c =  Erlin1_F_loop(i)
   Erlin1_d_loop_c =  Erlin1_d_loop(i)
   Erlin1_o_loop_c =  Erlin1_o_loop(i)
  Erlin1_r0_loop_c = Erlin1_r0_loop(i)
  phir1_lin_loop_c = phir1_lin_loop(i)

   Erlin2_F_loop_c =  Erlin2_F_loop(i)
   Erlin2_d_loop_c =  Erlin2_d_loop(i)
   Erlin2_o_loop_c =  Erlin2_o_loop(i)
  Erlin2_r0_loop_c = Erlin2_r0_loop(i)
  phir2_lin_loop_c = phir2_lin_loop(i)

;========================================

   Smaxxx1_loop_c =   Smaxxx1_loop(i)
   Smaxxx2_loop_c =   Smaxxx2_loop(i)
;   s_r0_a1_loop_c =   s_r0_a1_loop(i)
;   s_r0_a2_loop_c =   s_r0_a2_loop(i)
   Sminnn1_loop_c =   Sminnn1_loop(i)
   Sminnn2_loop_c =   Sminnn2_loop(i)

   eplegT1_loop_c =   eplegT1_loop(i) 
   eplegT2_loop_c =   eplegT2_loop(i) 
   deltaS1_loop_c =   deltaS1_loop(i)
   deltaS2_loop_c =   deltaS2_loop(i)

     TmR11_loop_c =     TmR11_loop(i)
     TmR12_loop_c =     TmR12_loop(i)
      NR11_loop_c =      NR11_loop(i)
      NR12_loop_c =      NR12_loop(i)

        Long_cm_c =        Long_cm(i)
         Phih_L_c =         Phih_L(i)

       ISO_loop_c =       ISO_loop(i)

return
end

pro histoplot3,datar,dataf,datah,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename

ps1,'./newfigs/'+filename+'.eps',0
DEVICE,/TIMES, FONT_INDEX=4

fr = histogram(datar,min=min,max=max,nbins=nbins,locations=vbinr) & fr = fr / total(fr)
fh = histogram(datah,min=min,max=max,nbins=nbins,locations=vbinh) & fh = fh / total(fh)
ff = histogram(dataf,min=min,max=max,nbins=nbins,locations=vbinf) & ff = ff / total(ff)

maxy=max([fr,fh,ff])

 plot,vbinr,fr,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,yr=[0,maxy],xr=[min,max],xstyle=1,/nodata,thick=3,charthick=2,Font=0
loadct,12

verde= 20
azul=100
rojo=200
oplot,vbinr,fr,psym=10,th=5,color=azul
oplot,vbinh,fh,psym=10,th=5,color=rojo
oplot,vbinf,ff,psym=10,th=5,color=verde
loadct,0

avgr        = median(datar)
avgh        = median(datah)
avgf        = median(dataf)
stdevr      =  stdev(datar);/avgr
stdevh      =  stdev(datah);/avgh
stdevf      =  stdev(dataf);/avgf

Nr          = N_elements(datar)
Nh          = N_elements(datah)
Nf          = N_elements(dataf)

xyouts,.95-[.3,.2,.1],0.8*[1,1,1],['!8m!3','!9s!3','N'],/normal,Font=0

loadct,12
negro=0
xyouts,.9-[.35,.3,.2,.1],0.7*[1,1,1,1],['!9f!3!Dr!N',strmid(string(avgr),4,5),strmid(string(stdevr),4,5),strmid(string(Nr),5,8)],/normal,color=[azul,negro,negro,negro],charthick=2,Font=0
xyouts,.9-[.35,.3,.2,.1],0.6*[1,1,1,1],['!9f!3!Dc!N',strmid(string(avgf),4,5),strmid(string(stdevf),4,5),strmid(string(Nf),5,8)],/normal,color=[verde,negro,negro,negro],charthick=2,Font=0
xyouts,.9-[.35,.3,.2,.1],0.5*[1,1,1,1],['!9f!3!Dh!N',strmid(string(avgh),4,5),strmid(string(stdevh),4,5),strmid(string(Nh),5,8)],/normal,color=[rojo,negro,negro,negro],charthick=2,Font=0

loadct,0

ps2

return
end

pro histoplotNe3,datar,dataf,datah,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename

ps1,'./newfigs/'+filename+'.eps',0

  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1
  DEVICE,/TIMES, FONT_INDEX=4

fr = histogram(datar,min=min,max=max,nbins=nbins,locations=vbinr) & fr = fr / total(fr)
fh = histogram(datah,min=min,max=max,nbins=nbins,locations=vbinh) & fh = fh / total(fh)
ff = histogram(dataf,min=min,max=max,nbins=nbins,locations=vbinf) & ff = ff / total(ff)

maxy=max([fr,fh,ff])

 plot,vbinr,fr,psym=10,charsize=2.5,xtitle=xtit,ytitle=ytit,title=tit,yr=[0,maxy],xr=[min,max],xstyle=1,/nodata,thick=3,charthick=2.25,Font=0

loadct,12

verde= 20
azul=100
rojo=200
oplot,vbinr,fr,th=10,color=azul ,psym=10,linestyle=3
oplot,vbinh,fh,th=10,color=rojo ,psym=10,linestyle=0
oplot,vbinf,ff,th=10,color=verde,psym=10,linestyle=2
loadct,0

avgr        = median(datar)
avgh        = median(datah)
avgf        = median(dataf)
stdevr      =  stdev(datar);/avgr
stdevh      =  stdev(datah);/avgh
stdevf      =  stdev(dataf);/avgf


Nh = N_elements(datah)


if floor(avgr) eq 0. then begin
   avgrr = strmid(string(avgr),3,6) 
endif else begin
if floor(avgr) gt 0. then avgrr =  strmid(string(avgr),4,6)
endelse

if floor(avgh) eq 0. then begin
   avghh = strmid(string(avgh),3,6) 
endif else begin
if floor(avgh) gt 0. then avghh =  strmid(string(avgh),4,6)
endelse

if abs(floor(avgf)) eq 0. then begin
   avgff = strmid(string(avgf),2,6) 
endif else begin
if abs(floor(avgf)) gt 0. then avgff =  strmid(string(avgf),4,6)
endelse

if floor(stdevr) eq 0. then begin
   stdevrr = strmid(string(stdevr),3,6) 
endif else begin
if floor(stdevr) gt 0. then stdevrr =  strmid(string(stdevr),4,6)
endelse

if floor(stdevh) eq 0. then begin
   stdevhh = strmid(string(stdevh),3,6) 
endif else begin
if floor(stdevh) gt 0. then stdevhh =  strmid(string(stdevh),4,6)
endelse

if floor(stdevf) eq 0. then begin
   stdevff = strmid(string(stdevf),3,6) 
endif else begin
if floor(stdevf) gt 0. then stdevff =  strmid(string(stdevf),4,6)
endelse

xyouts,.9-[.105,.015],0.8*[1,1],['!8m!3','!9s!3'],/normal,charsize=2,charthick=2,Font=0

loadct,12
negro=0

;xyouts,.9-[.18,.13,.05],0.7*[1,1,1],['!9f!3!Dr!N',' 1.39',' 1.08'],/normal,color=[azul ,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.18,.13,.05],0.6*[1,1,1],['!9f!3!Dc!N','-0.02',' 1.26'],/normal,color=[verde,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.18,.13,.05],0.5*[1,1,1],['!9f!3!Dh!N',' 0.99',' 1.69'],/normal,color=[rojo ,negro,negro],charthick=3,charsize=2,Font=0

;xyouts,.9-[.18,.13,.05],0.7*[1,1,1],['!9f!3!Dr!N',avgrr,stdevrr],/normal,color=[azul ,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.18,.13,.05],0.6*[1,1,1],['!9f!3!Dc!N',avgff,stdevff],/normal,color=[verde,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.18,.13,.05],0.5*[1,1,1],['!9f!3!Dh!N',avghh,stdevhh],/normal,color=[rojo ,negro,negro],charthick=3,charsize=2,Font=0

;xyouts,.9-[.225,.18,.13,.05],0.7*[1   ,1,1,1],['.-.- ','!9f!3!Dr!N',' 1.40',' 0.81'],/normal,color=[azul ,azul ,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.225,.18,.13,.05],0.6*[1   ,1,1,1],['---- ','!9f!3!Dc!N',' 0.15',' 0.19'],/normal,color=[verde,verde,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.225,.18,.13,.05],0.5*[1.04,1,1,1],['__ '  ,'!9f!3!Dh!N',' 1.57',' 0.77'],/normal,color=[rojo ,rojo ,negro,negro],charthick=3,charsize=2,Font=0

xyouts,.9-[.225,.18,.13,.05],0.7*[1   ,1,1,1],['.-.- ','!9f!3!Dr!N',strmid(string(avgr),4,6),strmid(string(stdevr),4,6)],/normal,color=[azul ,azul ,negro,negro],charthick=3,charsize=2,Font=0
xyouts,.9-[.225,.18,.13,.05],0.6*[1   ,1,1,1],['---- ','!9f!3!Dc!N',strmid(string(avgf),3,6),strmid(string(stdevf),4,6)],/normal,color=[verde,verde,negro,negro],charthick=3,charsize=2,Font=0
xyouts,.9-[.225,.18,.13,.05],0.5*[1.04,1,1,1],['__ '  ,'!9f!3!Dh!N',strmid(string(avgh),4,6),strmid(string(stdevh),4,6)],/normal,color=[rojo ,rojo ,negro,negro],charthick=3,charsize=2,Font=0

;xyouts,.9-[.18,.13,.05],0.7*[1,1,1],['!9f!3!Dr!N',strmid(string(avgr),4,6),strmid(string(stdevr),4,6)],/normal,color=[azul ,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.18,.13,.05],0.6*[1,1,1],['!9f!3!Dc!N',strmid(string(avgf),1,7),strmid(string(stdevf),4,6)],/normal,color=[verde,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.18,.13,.05],0.5*[1,1,1],['!9f!3!Dh!N',strmid(string(avgh),4,6),strmid(string(stdevh),4,6)],/normal,color=[rojo ,negro,negro],charthick=3,charsize=2,Font=0

loadct,0

xyouts,.95-[.15],0.35*[1],['N ='+strmid(string(Nh),5,8)],/normal,charsize=2,charthick=1,Font=0

ps2

return
end

pro Rpoint_map,box=box,footlat_c,footlon_c,opclstat_c,filelabel=filelabel,indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH

if not keyword_set(box) then box=[0.,-90.,360.,+90.]

!P.CHARTHICK=6
!p.charsize=2.5

; if rotacion eq 'CR2081' then trace_openclose,rotacion+'_90X180blines_r_',7,1.0,lonvA,latvA,/gng

; colors
coltb = 12
green=20
blue=100-12
dblue=100
red=200-36
dred=200

coltb=39
; Create custom made symbol (psym=8) for scatter plots
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=5.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL

iopen    = where(opclstat_c eq 0.)
ilarge   = where(opclstat_c eq 1.)
ismall   = where(opclstat_c eq 2.)

ps1,'./newfigs/'+filelabel+'_Rpoint-map.eps',0
  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1

thick=3
;Device, SET_FONT = 'Bookman-LightItalic'
;Device, /BKMAN, /LIGHT
;thick=3

  plot,footlon_c,footlat_c,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
;     title='Localizacion Fisica de Loops a R=1.075',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0
      title='Physical location of loops at R=1.075',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0

loadct,coltb
 if iopen (0) ne -1 then oplot,footlon_c(iopen ),footlat_c(iopen ),color=200  ,th=2,psym=8
 if ilarge(0) ne -1 then oplot,footlon_c(ilarge),footlat_c(ilarge),color=245   ,th=2,psym=8
 if ismall(0) ne -1 then oplot,footlon_c(ismall),footlat_c(ismall),color=28  ,th=2,psym=8

loadct,0

!p.multi = 0
!P.CHARTHICK=0

;oplot,lonvA,latvA,th=3,psym=8

ps2
return
end


function findel,value,array,matrix=matrix,array=retarray                                                                                            
  on_error,2;Return to caller if an error occurs                                                                                                    
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

pro PlotVs,xx1,yy1,xr=xr,yr=yr,xtit=xtit,ytit=ytit,tit=tit,filename=filename

ps1,'./newfigs/'+filename+'.eps',0
DEVICE,/TIMES, FONT_INDEX=4

rminhs = min(xx1)
rmaxhs = max(xx1)
xfit = xx1
yfit = yy1
fitTemp,xfit,yfit,rminhs,rmaxhs,A,corr2

hh1  = 2.*findgen(100)/99

 plot,xx1,yy1,psym=3,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,yr=yr,xr=xr,xstyle=1,/nodata,thick=3,charthick=2,font=0

loadct,12
verde= 20
azul=100
rojo=200
oplot,xx1,yy1,psym=3,th=8,color=rojo
;oplot,hh1,(A[0]+A[1]*hh1),psym=10,color=azul
loadct,0

;xyouts,0.6*[1,1,1],1.-[.2,.35,.5],[string(A[0]),string(A[1]),string(corr2)],/normal,font=0

ps2

return
end


pro Longitudes

common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base

common stat_filter,Nlegs_c,Nemean_c,Tmmean_c,WTmean_c,Nestddev_c,Tmstddev_c,WTstddev_c,loop_length_c,betamean_c,betaapex_c,Bmean_c,Br0_c
common stat_filter2,opclstat_c,lambda_N_c,lambda_p_c,Ne0_c,p0_c,Tefit_c,gradT_c,r2N_c,r2P_c,r2T_c,indexloop_c,leg_status_c,Tm0_c,Tm0s_c,Tm0_ts_c,gradT_ts_c,FTr_ts_c,Tm0s_ts_c,dTmds_ts_c,FTs_ts_c,Tm0s5_ts_c,dTmds5_ts_c,FTs5_ts_c    
common stat_filter3,Eh_c,sH_c,r2sH_c,Phir_c,Fcb_c,Ner0_c,Ner1_c,Ner2_c,Ner3_c,TmR1_c,NR1_c,Erlin_F_c,Erlin_d_c,Erlin_o_c,Erlin_r0_c,phir_lin_c
common stat_filter4,r2Tcuadr_c,Acuadr_a_c, s_r0_a_c,dTmds_c,r2Ts_c,Netech_c,Ne0_s_c,lambda_N_s_c
common stat_filter5,strad_c,stlat_c,stlon_c,footrad_c,footlat_c,footlon_c,Rp_rad_c,Rp_lat_c,Rp_lon_c
common stat_filter6,r2Er_c,Phirfit_c,eplegT_c,deltaS_c,Smaxxx_c,Sminnn_c,B_base_c,Fcb5_c

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_loop,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTs15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common loops_filter,Phir_loop_c,rad1_loop_c,rad2_loop_c,lat1_loop_c,lat2_loop_c,lon1_loop_c,lon2_loop_c,L_loop_c,opclstat_loop_c,r2Er1_loop_c,r2Er2_loop_c,Tmmean_loop_c,Fcb_loop_c,dTmds1_loop_c,dTmds2_loop_c,r2Tcuadr1_loop_c,r2Tcuadr2_loop_c,r2T1_loop_c,r2T2_loop_c,Ner01_loop_c,Ner02_loop_c,Ner11_loop_c,Ner12_loop_c,Ner21_loop_c,Ner22_loop_c,Ner31_loop_c,Ner32_loop_c,Rp_rad1_loop_c,Rp_lat1_loop_c,Rp_lon1_loop_c,Rp_rad2_loop_c,Rp_lat2_loop_c,Rp_lon2_loop_c,TmR11_loop_c,TmR12_loop_c,NR11_loop_c,NR12_loop_c,Nemean_loop_c,Long_cm_c,Phih_L_c,Ner0m_loop_c,lambda_N_loop_c,r2N1_loop_c,r2N2_loop_c,Tm01_ts_loop_c,gradT1_ts_loop_c,FTr1_ts_loop_c,Tm0s1_ts_loop_c,dTmds1_ts_loop_c,FTs1_ts_loop_c,Tm02_ts_loop_c,gradT2_ts_loop_c,FTr2_ts_loop_c,Tm0s2_ts_loop_c,dTmds2_ts_loop_c,FTs2_ts_loop_c,eplegT1_loop_c,eplegT2_loop_c,deltaS1_loop_c,deltaS2_loop_c,Smaxxx1_loop_c,Smaxxx2_loop_c,Sminnn1_loop_c,Sminnn2_loop_c,ISO_loop_c,B_basem_loop_c,Netech_loop_c,Ne0_s_loop_c,lambda_N_s_loop_c,Tm0s15_ts_loop_c,dTmds15_ts_loop_c,FTs15_ts_loop_c,Tm0s25_ts_loop_c,dTmds25_ts_loop_c,FTs25_ts_loop_c,Erlin1_F_loop_c,Erlin1_d_loop_c,Erlin1_o_loop_c,Erlin1_r0_loop_c,phir1_lin_loop_c,Erlin2_F_loop_c,Erlin2_d_loop_c,Erlin2_o_loop_c,Erlin2_r0_loop_c,phir2_lin_loop_c,Fcb5_loop_c

common label,T_label,Er_label

Longitudes = 0.2*findgen(6)+0.2
NLongitudes = N_elements(Longitudes)

for t=0,Nlongitudes-1 do begin
Lmin = Longitudes(t)
Lmax = Longitudes(t) + 0.2

 i = where(Abs(lat1_loop) le 30. and Abs(lat2_loop) le 30. and  Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T $
           and L_loop ge Lmin and L_loop lt Lmax)
filter_loop,i

x = where(Fcb_loop_c/1.e5 gt 0)
xx = N_elements(x)
if xx eq -1 then goto,siguiente

;y = where(Fcb_loop_c/1.e5 lt 0)

down = Fcb_loop_c(x)
;up   = Fcb_loop_c(y)

Ndown = N_elements(down)
;Nup   = N_elements(up  )
Ntot  = N_elements(Fcb_loop_c)

fraccion = Ndown/Ntot

print,'low',Lmin,Lmax,Ndown,Ntot,fraccion

 i = where(Abs(lat1_loop) ge 30. and Abs(lat2_loop) ge 30. and  Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T $
           and L_loop ge Lmin and L_loop lt Lmax)
filter_loop,i

x = where(Fcb_loop_c/1.e5 gt 0)
xx = N_elements(x)
if xx eq -1 then goto,siguiente

y = where(Fcb_loop_c/1.e5 lt 0)
down = Fcb_loop_c(x)
up   = Fcb_loop_c(y)

Ndown = N_elements(down)
Nup   = N_elements(up  )
Ntot  = N_elements(Fcb_loop_c)

fraccion = Ndown/Ntot

print,'mid',Lmin,Lmax,Ndown,Nup,Ntot,fraccion

siguiente:
endfor

Longitudes2 = 0.5*findgen(6)+2.0
NLongitudes2 = N_elements(Longitudes2)

for t=0,Nlongitudes2-1 do begin
Lmin = Longitudes2(t)
Lmax = Longitudes2(t) + 0.5

 i = where(Abs(lat1_loop) le 30. and Abs(lat2_loop) le 30. and  Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T $
           and L_loop ge Lmin and L_loop lt Lmax)
filter_loop,i

x = where(Fcb_loop_c/1.e5 gt 0)
xx = N_elements(x)
if xx eq -1 then goto,siguiente2

;y = where(Fcb_loop_c/1.e5 lt 0)
down = Fcb_loop_c(x)
;up   = Fcb_loop_c(y)

Ndown = N_elements(down)
;Nup   = N_elements(up  )
Ntot  = N_elements(Fcb_loop_c)

fraccion = Ndown/Ntot

print,'low',Lmin,Lmax,Ndown,Ntot,fraccion

 i = where(Abs(lat1_loop) ge 30. and Abs(lat2_loop) ge 30. and  Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2Er1_loop ge r2crit_Er and r2Er2_loop ge r2crit_Er and r2T1_loop ge r2crit_T and r2T2_loop ge r2crit_T $
           and L_loop ge Lmin and L_loop lt Lmax)
filter_loop,i

x = where(Fcb_loop_c/1.e5 gt 0)
xx = N_elements(x)
if xx eq -1 then goto,siguiente2

;y = where(Fcb_loop_c/1.e5 lt 0)
down = Fcb_loop_c(x)
;up   = Fcb_loop_c(y)

Ndown = N_elements(down)
;Nup   = N_elements(up  )
Ntot  = N_elements(Fcb_loop_c)

fraccion = Ndown/Ntot

print,'mid',Lmin,Lmax,Ndown,Ntot,fraccion

siguiente2:
endfor
                                                                                                                        
return 
end


pro Rpointfull_map,box=box,footlat,footlon,opclstat,NR1,filelabel=filelabel,indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH

if not keyword_set(box) then box=[0.,-90.,360.,+90.]

!P.CHARTHICK=6
!p.charsize=2.5

; if rotacion eq 'CR2081' then trace_openclose,rotacion+'_90X180blines_r_',7,1.0,lonvA,latvA,/gng

; colors
coltb = 12
green=20
blue=100-12
dblue=100
red=200-36
dred=200

coltb=39
; Create custom made symbol (psym=8) for scatter plots
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=5.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL

iopen    = where(opclstat eq 0. )
ilarge   = where(opclstat eq 1. and NR1/1.e8 lt 1.4)
ismall   = where(opclstat eq 2. and NR1/1.e8 lt 1.4)
iact     = where(opclstat ne 0. and NR1/1.e8 gt 1.4)

ps1,'./newfigs/'+filelabel+'_Rpointfull-map.eps',0
  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1

thick=3
;Device, SET_FONT = 'Bookman-LightItalic'
;Device, /BKMAN, /LIGHT
;thick=3

  plot,footlon,footlat,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
       title='Physical location of loop at R=1.075',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0
loadct,coltb
 if iopen (0) ne -1 then oplot,footlon(iopen ),footlat(iopen ),color=70  ,th=2,psym=8
 if ilarge(0) ne -1 then oplot,footlon(ilarge),footlat(ilarge),color=245  ,th=2,psym=8
 if ismall(0) ne -1 then oplot,footlon(ismall),footlat(ismall),color=28   ,th=2,psym=8
 if iact  (0) ne -1 then oplot,footlon(iact  ),footlat(iact  ),color=200  ,th=2,psym=8
loadct,0

!p.multi = 0
!P.CHARTHICK=0

;oplot,lonvA,latvA,th=3,psym=8

ps2
return
end


pro footpointfull_map,box=box,footlat,footlon,opclstat,NR1,filelabel=filelabel,indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH

if not keyword_set(box) then box=[0.,-90.,360.,+90.]

!P.CHARTHICK=6
!p.charsize=2.5

; if rotacion eq 'CR2081' then trace_openclose,rotacion+'_90X180blines_r_',7,1.0,lonvA,latvA,/gng

; colors
coltb = 12
green=20
blue=100-12
dblue=100
red=200-36
dred=200

coltb=39
; Create custom made symbol (psym=8) for scatter plots
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=5.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL

iopen    = where(opclstat eq 0. )
ilarge   = where(opclstat eq 1. and NR1/1.e8 lt 1.4)
ismall   = where(opclstat eq 2. and NR1/1.e8 lt 1.4)
iact     = where(opclstat ne 0. and NR1/1.e8 gt 1.4)

ps1,'./newfigs/'+filelabel+'_footpointfull-map.eps',0
  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1

thick=3
;Device, SET_FONT = 'Bookman-LightItalic'
;Device, /BKMAN, /LIGHT
;thick=3

  plot,footlon,footlat,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
       title='Physical location of the footpoints',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0
loadct,coltb
 if iopen (0) ne -1 then oplot,footlon(iopen ),footlat(iopen ),color=70  ,th=2,psym=8
 if ilarge(0) ne -1 then oplot,footlon(ilarge),footlat(ilarge),color=245  ,th=2,psym=8
 if ismall(0) ne -1 then oplot,footlon(ismall),footlat(ismall),color=28   ,th=2,psym=8
 if iact  (0) ne -1 then oplot,footlon(iact  ),footlat(iact  ),color=200  ,th=2,psym=8
loadct,0

!p.multi = 0
!P.CHARTHICK=0

;oplot,lonvA,latvA,th=3,psym=8

ps2
return
end








pro Rpoint_updown,box=box,footlat_c,footlon_c,opclstat_c,Fcb_loop_c,filelabel=filelabel,indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH

if not keyword_set(box) then box=[0.,-90.,360.,+90.]

!P.CHARTHICK=6
!p.charsize=2.5

; if rotacion eq 'CR2081' then trace_openclose,rotacion+'_90X180blines_r_',7,1.0,lonvA,latvA,/gng

; colors
coltb = 12
green=20
blue=100-12
dblue=100
red=200-36
dred=200

coltb=39
; Create custom made symbol (psym=8) for scatter plots
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=5.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL

;iopen    = where(opclstat_c eq 0.)

ilarge_up   = where(opclstat_c eq 1. and Fcb_loop_c gt 0.)
ilarge_down = where(opclstat_c eq 1. and Fcb_loop_c lt 0.)
ismall_up   = where(opclstat_c eq 2. and Fcb_loop_c gt 0.)
ismall_down = where(opclstat_c eq 2. and Fcb_loop_c lt 0.)

ps1,'./newfigs/'+filelabel+'_Rpoint-updown.eps',0
  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1

thick=3
;Device, SET_FONT = 'Bookman-LightItalic'
;Device, /BKMAN, /LIGHT
;thick=3

  plot,footlon_c,footlat_c,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
      title='Localizacion Up-Down Loops a R=1.075',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0
;     title='Physical location of loops at R=1.075',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0

loadct,coltb
;if iopen (0) ne -1 then oplot,footlon_c(iopen ),footlat_c(iopen ),color=200  ,th=2,psym=8
 if ilarge_up  (0) ne -1 then oplot,footlon_c(ilarge_up  ),footlat_c(ilarge_up  ),color=65    ,th=2,psym=8
 if ilarge_down(0) ne -1 then oplot,footlon_c(ilarge_down),footlat_c(ilarge_down),color=253   ,th=2,psym=8
 if ismall_up  (0) ne -1 then oplot,footlon_c(ismall_up  ),footlat_c(ismall_up  ),color=95    ,th=2,psym=8
 if ismall_down(0) ne -1 then oplot,footlon_c(ismall_down),footlat_c(ismall_down),color=213   ,th=2,psym=8

loadct,0

!p.multi = 0
!P.CHARTHICK=0

;oplot,lonvA,latvA,th=3,psym=8

ps2
return
end


pro histoplot2,datar,dataf,min=min,max=max,nbins=nbins,xtit=xtit,ytit=ytit,tit=tit,filename=filename

ps1,'./newfigs/'+filename+'.eps',0
  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1
  DEVICE,/TIMES, FONT_INDEX=4

fr = histogram(datar,min=min,max=max,nbins=nbins,locations=vbinr) & fr = fr / total(fr)
ff = histogram(dataf,min=min,max=max,nbins=nbins,locations=vbinf) & ff = ff / total(ff)

maxy=max([fr,ff])
;maxy=0.1

 plot,vbinr,fr,psym=10,charsize=2,xtitle=xtit,ytitle=ytit,title=tit,yr=[0,maxy],xr=[min,max],xstyle=1,/nodata,thick=3,charthick=2,Font=0
loadct,12
;loadct,39

verde= 20
azul=100
rojo=200
oplot,vbinr,fr,psym=10,th=5,color=azul,linestyle=0
oplot,vbinf,ff,psym=10,th=5,color=rojo,linestyle=2
loadct,0

avgr        = median(datar)
avgf        = median(dataf)
stdevr      =  stdev(datar);/avgr
stdevf      =  stdev(dataf);/avgf

Nr          = N_elements(datar)
Nf          = N_elements(dataf)

loadct,12
negro=0

xyouts,.9-[.21,.12,.03],0.8*[1,1,1],['!8m!3','!9s!3','N'],/normal,charsize=2,charthick=2,Font=0

xyouts,.9-[.42,.36,.24,.14,.06],0.7*[1.03,1,1,1,1],['__' ,'T!Dm!N/T!DEBTEL!N',strmid(string(avgr),3,6),strmid(string(stdevr),4,5),strmid(string(Nr),5,8)],/normal,color=[azul,azul,negro,negro,negro],charthick=3,charsize=2,Font=0
xyouts,.9-[.42,.36,.24,.15,.06],0.6*[1   ,1,1,1,1],['---','N!D0!N/N!DEBTEL!N',strmid(string(avgf),3,6),strmid(string(stdevf),4,6),strmid(string(Nf),5,8)],/normal,color=[rojo,rojo,negro,negro,negro],charthick=3,charsize=2,Font=0

;xyouts,.9-[.42,.36,.24,.14,.06],0.7*[1.03,1,1,1,1],['__' ,'N!DCB!N/N!Dmean!N',strmid(string(avgr),3,6),strmid(string(stdevr),4,5),strmid(string(Nr),5,8)],/normal,color=[azul,azul,negro,negro,negro],charthick=3,charsize=2,Font=0
;xyouts,.9-[.42,.36,.24,.15,.06],0.6*[1   ,1,1,1,1],['---',     'L/!9l!3'     ,strmid(string(avgf),3,6),strmid(string(stdevf),4,6),strmid(string(Nf),5,8)],/normal,color=[rojo,rojo,negro,negro,negro],charthick=3,charsize=2,Font=0

loadct,0

ps2

return
end




                                                                                                                                                                                                                                                                                                                                                                                       
PRO Fluence_Plot,data1,lat,lon

common trace_sampled,rad_v,lat_v,lon_v,s_v,Ne_v,Tm_v,WT_v,Er_v,scoreR_v,midcell_v,Npts_v,str_v,stth_v,stph_v,radstart,enrad_v,enlon_v,enlat_v,npar,DEMc_v,lambda_v,L,Tmin,Tmax
common B_sampled,B_v,Br_v,Bth_v,Bph_v
common opclstatus,opcls,loopL,WTc  

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base

common stat_filter,Nlegs_c,Nemean_c,Tmmean_c,WTmean_c,Nestddev_c,Tmstddev_c,WTstddev_c,loop_length_c,betamean_c,betaapex_c,Bmean_c,Br0_c
common stat_filter2,opclstat_c,lambda_N_c,lambda_p_c,Ne0_c,p0_c,Tefit_c,gradT_c,r2N_c,r2P_c,r2T_c,indexloop_c,leg_status_c,Tm0_c,Tm0s_c,Tm0_ts_c,gradT_ts_c,FTr_ts_c,Tm0s_ts_c,dTmds_ts_c,FTs_ts_c,Tm0s5_ts_c,dTmds5_ts_c,FTs5_ts_c    
common stat_filter3,Eh_c,sH_c,r2sH_c,Phir_c,Fcb_c,Ner0_c,Ner1_c,Ner2_c,Ner3_c,TmR1_c,NR1_c,Erlin_F_c,Erlin_d_c,Erlin_o_c,Erlin_r0_c,phir_lin_c
common stat_filter4,r2Tcuadr_c,Acuadr_a_c, s_r0_a_c,dTmds_c,r2Ts_c,Netech_c,Ne0_s_c,lambda_N_s_c
common stat_filter5,strad_c,stlat_c,stlon_c,footrad_c,footlat_c,footlon_c,Rp_rad_c,Rp_lat_c,Rp_lon_c
common stat_filter6,r2Er_c,Phirfit_c,eplegT_c,deltaS_c,Smaxxx_c,Sminnn_c,B_base_c,Fcb5_c

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_loop,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTs15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common loops_filter,Phir_loop_c,rad1_loop_c,rad2_loop_c,lat1_loop_c,lat2_loop_c,lon1_loop_c,lon2_loop_c,L_loop_c,opclstat_loop_c,r2Er1_loop_c,r2Er2_loop_c,Tmmean_loop_c,Fcb_loop_c,dTmds1_loop_c,dTmds2_loop_c,r2Tcuadr1_loop_c,r2Tcuadr2_loop_c,r2T1_loop_c,r2T2_loop_c,Ner01_loop_c,Ner02_loop_c,Ner11_loop_c,Ner12_loop_c,Ner21_loop_c,Ner22_loop_c,Ner31_loop_c,Ner32_loop_c,Rp_rad1_loop_c,Rp_lat1_loop_c,Rp_lon1_loop_c,Rp_rad2_loop_c,Rp_lat2_loop_c,Rp_lon2_loop_c,TmR11_loop_c,TmR12_loop_c,NR11_loop_c,NR12_loop_c,Nemean_loop_c,Long_cm_c,Phih_L_c,Ner0m_loop_c,lambda_N_loop_c,r2N1_loop_c,r2N2_loop_c,Tm01_ts_loop_c,gradT1_ts_loop_c,FTr1_ts_loop_c,Tm0s1_ts_loop_c,dTmds1_ts_loop_c,FTs1_ts_loop_c,Tm02_ts_loop_c,gradT2_ts_loop_c,FTr2_ts_loop_c,Tm0s2_ts_loop_c,dTmds2_ts_loop_c,FTs2_ts_loop_c,eplegT1_loop_c,eplegT2_loop_c,deltaS1_loop_c,deltaS2_loop_c,Smaxxx1_loop_c,Smaxxx2_loop_c,Sminnn1_loop_c,Sminnn2_loop_c,ISO_loop_c,B_basem_loop_c,Netech_loop_c,Ne0_s_loop_c,lambda_N_s_loop_c,Tm0s15_ts_loop_c,dTmds15_ts_loop_c,FTs15_ts_loop_c,Tm0s25_ts_loop_c,dTmds25_ts_loop_c,FTs25_ts_loop_c,Erlin1_F_loop_c,Erlin1_d_loop_c,Erlin1_o_loop_c,Erlin1_r0_loop_c,phir1_lin_loop_c,Erlin2_F_loop_c,Erlin2_d_loop_c,Erlin2_o_loop_c,Erlin2_r0_loop_c,phir2_lin_loop_c,Fcb5_loop_c

common label,T_label,Er_label

                                ; Create some example data to plot.                                                                                                                                                                         
;    data = data1 ;Gaussian_Function([3,6], Width=21)
    data = 1.0 - data1
    x = lon ;Findgen(21) ;Scale_Vector(Findgen(21), -4, 4)
    y = lat ;Findgen(21) ;Scale_Vector(Findgen(21), -4, 4)  

                                ; Load program colors.                                                                                                                      
    LoadCT, 39 ;, /Brewer, /Reverse                                                                                                                                                                                                            ;cgLoadCT, 4, /Brewer, /Reverse                                                                                                                                                                                                                                          ; Create a position for the surface in
                                ; the display window.                                                                                                                                                                                       
    pos = [0.1, 0.1, 0.9, 0.75]
                                ; With surfaces, the charsize often
                                ; needs to be adjusted in PostScript.                                                                                                                                                                       
    thisCharSize = (!D.Name EQ 'PS') ? cgDefCharsize()*1.75 : cgDefCharsize()
                                ; Draw the fluence surface plot as a
                                ; shaded surface.                                                                                                                                                                                           
    cgSurf, data, x, y, RotX=80, RotZ=45, Position=pos, /Elevation, ZStyle=4, $
        /Shaded, ZTickformat='(A1)', XTitle='X Location (cm)', YTitle='Y Location (cm)', $
        Charsize=thisCharSize
                                ; Overlay the surface lines and add a
                                ; skirt.                                                                                                                                                                                                    
    cgSurf, data, x, y, RotX=80, RotZ=45, Position=pos, Skirt=Min(data), $
        XTickformat='(A1)', YTickformat='(A1)', ZStyle=4, /NoErase, Color='dark gray', $
            Charsize=thisCharSize
                                ; Add a color bar to the plot.                                                                                                                                                                              
    cgColorbar, Range=[0,1], Title='Normalized Fluence', TLocation='top', $
        Position=[0.86, 0.15, 0.91, 0.85], Charsize=thisCharSize*0.8
 END 




pro Phih_map,box=box,footlat_c,footlon_c,Phih_c,filelabel=filelabel,indexloop_c,rotacion,Eh=Eh,sH=sH,r2sH=r2sH

if not keyword_set(box) then box=[0.,-90.,360.,+90.]

!P.CHARTHICK=6
!p.charsize=2.5

; if rotacion eq 'CR2081' then trace_openclose,rotacion+'_90X180blines_r_',7,1.0,lonvA,latvA,/gng

; colors
coltb = 12
green=20
blue=100-12
dblue=100
red=200-36
dred=200

coltb=3;9
; Create custom made symbol (psym=8) for scatter plots
 N=25
 A = FINDGEN(N) * (!PI*2/float(N-1))
 f=3.
 USERSYM, COS(A)/f, SIN(A)/f,/FILL

i1 = where(Phih_c lt 1.1                    )
i2 = where(Phih_c lt 1.27 and Phih_c gt 1.1 )
i3 = where(Phih_c lt 1.45 and Phih_c gt 1.27)
i4 = where(Phih_c lt 1.7  and Phih_c gt 1.45)
i5 = where(                   Phih_c gt 1.7 )

ps1,'./newfigs/'+filelabel+'_Phih-map.eps',0
  DEVICE,/INCHES,YSIZE=6,XSIZE=12,SCALE_FACTOR=1

thick=3
;Device, SET_FONT = 'Bookman-LightItalic'
;Device, /BKMAN, /LIGHT
;thick=3

  plot,footlon_c,footlat_c,xr=[box[0],box[2]],yr=[box[1],box[3]],psym=8,$
      title='Mapa Heating',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0
;     title='Physical location of loops at R=1.075',xtitle='Lon [deg]',ytitle='Lat [deg]',xthick=thick,ythick=thick,/nodata,xstyle=1,ystyle=1,font=0

loadct,coltb
 if i1 (0) ne -1 then oplot,footlon_c(i1),footlat_c(i1),color=48  ,th=2,psym=8
 if i2 (0) ne -1 then oplot,footlon_c(i2),footlat_c(i2),color=96  ,th=2,psym=8
 if i3 (0) ne -1 then oplot,footlon_c(i3),footlat_c(i3),color=144 ,th=2,psym=8
 if i4 (0) ne -1 then oplot,footlon_c(i4),footlat_c(i4),color=152 ,th=2,psym=8
 if i5 (0) ne -1 then oplot,footlon_c(i5),footlat_c(i5),color=200 ,th=2,psym=8

loadct,0

!p.multi = 0
!P.CHARTHICK=0

;oplot,lonvA,latvA,th=3,psym=8

ps2
return
end



pro longphi

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base

common stat_filter,Nlegs_c,Nemean_c,Tmmean_c,WTmean_c,Nestddev_c,Tmstddev_c,WTstddev_c,loop_length_c,betamean_c,betaapex_c,Bmean_c,Br0_c
common stat_filter2,opclstat_c,lambda_N_c,lambda_p_c,Ne0_c,p0_c,Tefit_c,gradT_c,r2N_c,r2P_c,r2T_c,indexloop_c,leg_status_c,Tm0_c,Tm0s_c,Tm0_ts_c,gradT_ts_c,FTr_ts_c,Tm0s_ts_c,dTmds_ts_c,FTs_ts_c,Tm0s5_ts_c,dTmds5_ts_c,FTs5_ts_c  
common stat_filter3,Eh_c,sH_c,r2sH_c,Phir_c,Fcb_c,Ner0_c,Ner1_c,Ner2_c,Ner3_c,TmR1_c,NR1_c,Erlin_F_c,Erlin_d_c,Erlin_o_c,Erlin_r0_c,phir_lin_c
common stat_filter4,r2Tcuadr_c,Acuadr_a_c, s_r0_a_c,dTmds_c,r2Ts_c,Netech_c,Ne0_s_c,lambda_N_s_c
common stat_filter5,strad_c,stlat_c,stlon_c,footrad_c,footlat_c,footlon_c,Rp_rad_c,Rp_lat_c,Rp_lon_c
common stat_filter6,r2Er_c,Phirfit_c,eplegT_c,deltaS_c,Smaxxx_c,Sminnn_c,B_base_c,Fcb5_c

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_loop,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTs15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common loops_filter,Phir_loop_c,rad1_loop_c,rad2_loop_c,lat1_loop_c,lat2_loop_c,lon1_loop_c,lon2_loop_c,L_loop_c,opclstat_loop_c,r2Er1_loop_c,r2Er2_loop_c,Tmmean_loop_c,Fcb_loop_c,dTmds1_loop_c,dTmds2_loop_c,r2Tcuadr1_loop_c,r2Tcuadr2_loop_c,r2T1_loop_c,r2T2_loop_c,Ner01_loop_c,Ner02_loop_c,Ner11_loop_c,Ner12_loop_c,Ner21_loop_c,Ner22_loop_c,Ner31_loop_c,Ner32_loop_c,Rp_rad1_loop_c,Rp_lat1_loop_c,Rp_lon1_loop_c,Rp_rad2_loop_c,Rp_lat2_loop_c,Rp_lon2_loop_c,TmR11_loop_c,TmR12_loop_c,NR11_loop_c,NR12_loop_c,Nemean_loop_c,Long_cm_c,Phih_L_c,Ner0m_loop_c,lambda_N_loop_c,r2N1_loop_c,r2N2_loop_c,Tm01_ts_loop_c,gradT1_ts_loop_c,FTr1_ts_loop_c,Tm0s1_ts_loop_c,dTmds1_ts_loop_c,FTs1_ts_loop_c,Tm02_ts_loop_c,gradT2_ts_loop_c,FTr2_ts_loop_c,Tm0s2_ts_loop_c,dTmds2_ts_loop_c,FTs2_ts_loop_c,eplegT1_loop_c,eplegT2_loop_c,deltaS1_loop_c,deltaS2_loop_c,Smaxxx1_loop_c,Smaxxx2_loop_c,Sminnn1_loop_c,Sminnn2_loop_c,ISO_loop_c,B_basem_loop_c,Netech_loop_c,Ne0_s_loop_c,lambda_N_s_loop_c,Tm0s15_ts_loop_c,dTmds15_ts_loop_c,FTs15_ts_loop_c,Tm0s25_ts_loop_c,dTmds25_ts_loop_c,FTs25_ts_loop_c,Erlin1_F_loop_c,Erlin1_d_loop_c,Erlin1_o_loop_c,Erlin1_r0_loop_c,phir1_lin_loop_c,Erlin2_F_loop_c,Erlin2_d_loop_c,Erlin2_o_loop_c,Erlin2_r0_loop_c,phir2_lin_loop_c,Fcb5_loop_c

common label,T_label,Er_label

R2Crit_Er = 0.8
R2Crit_Ne = 0.75
R2Crit_T  = 0.5
FTscrit   = 0.70
NeCritA   = 1.4
NeCritQ   = 1.4
rsun      = 6.955e10

ff=1.


 i = where(Phir_loop gt 0. and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit $
           and    (abs(dTmds1_ts_loop)*rsun*abs(Smaxxx1_loop - Sminnn1_loop) gt ff*(2*eplegT1_loop)        $
           or      abs(dTmds2_ts_loop)*rsun*abs(Smaxxx2_loop - Sminnn2_loop) gt ff*(2*eplegT2_loop) )      $
           and ((dTmds1_ts_loop gt 0. and dTmds2_ts_loop gt 0.) or (dTmds1_ts_loop lt 0. and dTmds2_ts_loop lt 0.))  );$ 
;           and  Abs(lat1_loop) lt 30. and Abs(lat2_loop) lt 30. )     ;low
;          and  Abs(lat1_loop) ge 30. and Abs(lat2_loop) ge 30. ) ;mid


filter_loop,i
;stop
iin=where(Fcb_loop_c/1.e5 gt -1.1)
Phir_filt = Phir_loop_c(iin)
 Fcb_filt =  Fcb_loop_c(iin)
   L_filt =    L_loop_c(iin)
     L_cm = L_filt*rsun

Phih_filt = Phir_filt-Fcb_filt
 Phih_L   = Phih_filt/L_cm   

iinn=where(Phih_L gt 5/1.e8)
  L_cm1   =     L_cm(iinn)
  Phih_L1 =   Phih_L(iinn)

;iinn2=where(L_cm1 lt 0.9*1.e11)
iinn2=where(L_cm1 lt 1.25*1.e10)
  L_cm2   =     L_cm1(iinn2)
  Phih_L2 =   Phih_L1(iinn2)

;stop

writecol,'/data1/work/dem/Long_phi_2081-cortos-lt1-25e10.txt',L_cm2,Phih_L2   
suffix = '-2081-cortos-lt1-25e10'
;openw,2,'Long_phi_2081.txt'
;printf,2,format='(''Longitud'',''  Heating'')'
;printf,2, format='(e10.2,e10.2)',L_cm,Phih_L

nbins=80

  xx = L_cm2
mini = 0.
maxi = max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='L [cm]',ytit='Frequency Histogram',tit='Longitudes',filename='QS-L_cm'+suffix

  xx = Phih_L2
mini = 0.
maxi = max(xx)
histoplot,xx,min=mini,max=maxi,nbins=nbins,xtit='!9f!3/L [10!U5!N erg cm!U-3!N sec!U-1!N]',ytit='Frequency Histogram',tit='Heating rate',filename='QS-Phih_L'+suffix

goto,fin
Nloops = n_elements(L_filt)*1L
;stop
     Phih_loop = fltarr(Nloops)-666
       Long_cm = fltarr(Nloops)-666
        Phih_L = fltarr(Nloops)-666

printf,2,Nloops

iloop = 0L
for iloop = 0L,Nloops-1 do begin
;stop
     Phih_loop(iloop) = Phir_filt(iloop) - Fcb_filt(iloop)

       Long_cm(iloop) = L_filt(iloop)*rsun
        Phih_L(iloop) = Phih_loop(iloop)/Long_cm(iloop) 

printf,2, format='(e10.2,e10.2)',Long_cm(iloop),Phih_L(iloop)

;iloop=iloop+1
;print,iloop
;if iloop eq 20 then stop
if Long_cm(iloop) eq -666 then stop
if  Phih_L(iloop) eq -666 then stop
endfor
;stop
;close,2
fin:

return
end



pro label

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base

common stat_filter,Nlegs_c,Nemean_c,Tmmean_c,WTmean_c,Nestddev_c,Tmstddev_c,WTstddev_c,loop_length_c,betamean_c,betaapex_c,Bmean_c,Br0_c
common stat_filter2,opclstat_c,lambda_N_c,lambda_p_c,Ne0_c,p0_c,Tefit_c,gradT_c,r2N_c,r2P_c,r2T_c,indexloop_c,leg_status_c,Tm0_c,Tm0s_c,Tm0_ts_c,gradT_ts_c,FTr_ts_c,Tm0s_ts_c,dTmds_ts_c,FTs_ts_c,Tm0s5_ts_c,dTmds5_ts_c,FTs5_ts_c  
common stat_filter3,Eh_c,sH_c,r2sH_c,Phir_c,Fcb_c,Ner0_c,Ner1_c,Ner2_c,Ner3_c,TmR1_c,NR1_c,Erlin_F_c,Erlin_d_c,Erlin_o_c,Erlin_r0_c,phir_lin_c
common stat_filter4,r2Tcuadr_c,Acuadr_a_c, s_r0_a_c,dTmds_c,r2Ts_c,Netech_c,Ne0_s_c,lambda_N_s_c
common stat_filter5,strad_c,stlat_c,stlon_c,footrad_c,footlat_c,footlon_c,Rp_rad_c,Rp_lat_c,Rp_lon_c
common stat_filter6,r2Er_c,Phirfit_c,eplegT_c,deltaS_c,Smaxxx_c,Sminnn_c,B_base_c,Fcb5_c

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_loop,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTs15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common loops_filter,Phir_loop_c,rad1_loop_c,rad2_loop_c,lat1_loop_c,lat2_loop_c,lon1_loop_c,lon2_loop_c,L_loop_c,opclstat_loop_c,r2Er1_loop_c,r2Er2_loop_c,Tmmean_loop_c,Fcb_loop_c,dTmds1_loop_c,dTmds2_loop_c,r2Tcuadr1_loop_c,r2Tcuadr2_loop_c,r2T1_loop_c,r2T2_loop_c,Ner01_loop_c,Ner02_loop_c,Ner11_loop_c,Ner12_loop_c,Ner21_loop_c,Ner22_loop_c,Ner31_loop_c,Ner32_loop_c,Rp_rad1_loop_c,Rp_lat1_loop_c,Rp_lon1_loop_c,Rp_rad2_loop_c,Rp_lat2_loop_c,Rp_lon2_loop_c,TmR11_loop_c,TmR12_loop_c,NR11_loop_c,NR12_loop_c,Nemean_loop_c,Long_cm_c,Phih_L_c,Ner0m_loop_c,lambda_N_loop_c,r2N1_loop_c,r2N2_loop_c,Tm01_ts_loop_c,gradT1_ts_loop_c,FTr1_ts_loop_c,Tm0s1_ts_loop_c,dTmds1_ts_loop_c,FTs1_ts_loop_c,Tm02_ts_loop_c,gradT2_ts_loop_c,FTr2_ts_loop_c,Tm0s2_ts_loop_c,dTmds2_ts_loop_c,FTs2_ts_loop_c,eplegT1_loop_c,eplegT2_loop_c,deltaS1_loop_c,deltaS2_loop_c,Smaxxx1_loop_c,Smaxxx2_loop_c,Sminnn1_loop_c,Sminnn2_loop_c,ISO_loop_c,B_basem_loop_c,Netech_loop_c,Ne0_s_loop_c,lambda_N_s_loop_c,Tm0s15_ts_loop_c,dTmds15_ts_loop_c,FTs15_ts_loop_c,Tm0s25_ts_loop_c,dTmds25_ts_loop_c,FTs25_ts_loop_c,Erlin1_F_loop_c,Erlin1_d_loop_c,Erlin1_o_loop_c,Erlin1_r0_loop_c,phir1_lin_loop_c,Erlin2_F_loop_c,Erlin2_d_loop_c,Erlin2_o_loop_c,Erlin2_r0_loop_c,phir2_lin_loop_c,Fcb5_loop_c

common label,T_label,Er_label

 i = where(opclstat gt 0.)

 Nlegs = n_elements(i)*1L
Nloops = Nlegs/2 

stop
;Nloops = n_elements(Phir_loop)*1L

;Etiquetado de loops
;
;T_label
;Ajuste de temperatura: (se realizan dos ajustes de temperatura lineales, el priemero con todos los datos de la pierna (FTs*_ts_loop) y el segundo con los primeros puntos (FTs*5_ts_loop))
;     (0) si no supera ningún ajuste en una o ambas piernas. (FTs*_ts_loop < 0.7 y FTs*5_ts_loop < 0.7)
;     (1) si tiene éxito el ajuste con todos los datos en ambas piernas. (FTs*_ts_loop > 0.7)
;     (2) si fracasa el ajuste con todos los datos pero tiene éxito el ajuste con pocos datos en ambas piernas. (FTs*5_ts_loop > 0.7) 
;     (3) si en una pierna es (1) y en otra (2)
;
;Er_label
;Ajuste de pérdida radiativa: (se realizan dos ajustes, el primero exponencial con todos los datos de la pierna (r2Er*_loop) y el segundo lineal con los primeros cuatro puntos (Erlin*_F_loop))
;     (0) si no soporta ningún ajuste en una o ambas piernas. (r2Er*_loop < 0.75 y Erlin*_F_loop < 0.5)
;     (1) si tiene éxito el ajuste con todos los datos en ambas piernas. (r2Er*_loop > 0.75)
;     (2) si fracasa el ajuste con todos los datos pero tiene éxito el ajuste con cuatro datos en ambas piernas. (Erlin*_F_loop > 0.5)
;     (3) si en una pierna es (1) y en otra (2)

 T_label = fltarr(Nloops)-555. 
Er_label = fltarr(Nloops)-555.

stop

;Temperatura

for iloop = 0L,Nloops-1 do begin
if ((FTs1_ts_loop(iloop) lt 0.70 or  FTs2_ts_loop(iloop) lt 0.70) and (FTs15_ts_loop(iloop) lt 0.70 or  FTs25_ts_loop(iloop) lt 0.70)) then T_label(iloop) = 0
if  (FTs1_ts_loop(iloop) ge 0.70 and FTs2_ts_loop(iloop) ge 0.70)                                                                      then T_label(iloop) = 1
if  (FTs1_ts_loop(iloop) lt 0.70 and FTs2_ts_loop(iloop) lt 0.70  and  FTs15_ts_loop(iloop) ge 0.70 and FTs25_ts_loop(iloop) ge 0.70)  then T_label(iloop) = 2
if ((FTs1_ts_loop(iloop) ge 0.70 and FTs2_ts_loop(iloop) lt 0.70  and                                   FTs25_ts_loop(iloop) ge 0.70) $
 or (FTs1_ts_loop(iloop) lt 0.70 and FTs2_ts_loop(iloop) ge 0.70  and  FTs15_ts_loop(iloop) ge 0.70))                                  then T_label(iloop) = 3
endfor


;Pérdida radiativa

for iloop = 0L,Nloops-1 do begin
if ((r2Er1_loop(iloop) lt 0.75 or  r2Er2_loop(iloop) lt 0.75) and (Erlin1_F_loop(iloop) lt 0.50 or  Erlin2_F_loop(iloop) lt 0.50)) then Er_label(iloop) = 0
if  (r2Er1_loop(iloop) ge 0.75 and r2Er2_loop(iloop) ge 0.75)                                                                      then Er_label(iloop) = 1
if ((r2Er1_loop(iloop) lt 0.75 and r2Er2_loop(iloop) lt 0.75) and (Erlin1_F_loop(iloop) ge 0.50 and Erlin2_F_loop(iloop) ge 0.50)) then Er_label(iloop) = 2
if ((r2Er1_loop(iloop) ge 0.75 and r2Er2_loop(iloop) lt 0.75  and                                   Erlin2_F_loop(iloop) ge 0.50) $
 or (r2Er1_loop(iloop) lt 0.75 and r2Er2_loop(iloop) ge 0.75  and  Erlin1_F_loop(iloop) ge 0.50))                                  then Er_label(iloop) = 3
endfor

stop

return
end

pro select

common statistic_loops,Nlegs,Nemean,Tmmean,WTmean,Nestddev,Tmstddev,WTstddev,loop_length,betamean,betaapex,Bmean,Br0
common statistic_loops2,opclstat,lambda_N,lambda_p,Ne0,p0,Tefit,gradT,r2N,r2P,r2T,indexloop,leg_status,Tm0,Tm0s  
common statistic_loops3,Eh,sH,r2sH,Phir,Fcb,Ner0,Ner1,Ner2,Ner3,TmR1,NR1,Fcb5
common statistic_loops4,r2Tcuadr,Acuadr_a, s_r0_a,dTmds,r2Ts,Tmbase,Netech,Ne0_s,lambda_N_s
common statistic_loops5,r2Er,Phirfit,Tm0_ts,gradT_ts,FTr_ts,Tm0s_ts,dTmds_ts,FTs_ts,eplegT,deltaS,Smaxxx,Sminnn,Tm0s5_ts,dTmds5_ts,FTs5_ts,Erlin_F,Erlin_d,Erlin_o,Erlin_r0,phir_lin
common starttrace,strad,stlat,stlon,footrad,footlat,footlon,Rp_rad,Rp_lat,Rp_lon,B_base

common stat_filter,Nlegs_c,Nemean_c,Tmmean_c,WTmean_c,Nestddev_c,Tmstddev_c,WTstddev_c,loop_length_c,betamean_c,betaapex_c,Bmean_c,Br0_c
common stat_filter2,opclstat_c,lambda_N_c,lambda_p_c,Ne0_c,p0_c,Tefit_c,gradT_c,r2N_c,r2P_c,r2T_c,indexloop_c,leg_status_c,Tm0_c,Tm0s_c,Tm0_ts_c,gradT_ts_c,FTr_ts_c,Tm0s_ts_c,dTmds_ts_c,FTs_ts_c,Tm0s5_ts_c,dTmds5_ts_c,FTs5_ts_c  
common stat_filter3,Eh_c,sH_c,r2sH_c,Phir_c,Fcb_c,Ner0_c,Ner1_c,Ner2_c,Ner3_c,TmR1_c,NR1_c,Erlin_F_c,Erlin_d_c,Erlin_o_c,Erlin_r0_c,phir_lin_c
common stat_filter4,r2Tcuadr_c,Acuadr_a_c, s_r0_a_c,dTmds_c,r2Ts_c,Netech_c,Ne0_s_c,lambda_N_s_c
common stat_filter5,strad_c,stlat_c,stlon_c,footrad_c,footlat_c,footlon_c,Rp_rad_c,Rp_lat_c,Rp_lon_c
common stat_filter6,r2Er_c,Phirfit_c,eplegT_c,deltaS_c,Smaxxx_c,Sminnn_c,B_base_c,Fcb5_c

common loops,Phir_loop,rad1_loop,rad2_loop,lat1_loop,lat2_loop,lon1_loop,lon2_loop,L_loop,opclstat_loop,r2Er1_loop,r2Er2_loop,Tmmean_loop,Fcb_loop,dTmds1_loop,dTmds2_loop,r2Tcuadr1_loop,r2Tcuadr2_loop,r2T1_loop,r2T2_loop,Ner01_loop,Ner02_loop,Ner11_loop,Ner12_loop,Ner21_loop,Ner22_loop,Ner31_loop,Ner32_loop,Rp_rad1_loop,Rp_lat1_loop,Rp_lon1_loop,Rp_rad2_loop,Rp_lat2_loop,Rp_lon2_loop,TmR11_loop,TmR12_loop,NR11_loop,NR12_loop,Nemean_loop,Long_cm,Phih_L,Ner0m_loop,lambda_N_loop,r2N1_loop,r2N2_loop,Tm01_ts_loop,gradT1_ts_loop,FTr1_ts_loop,Tm0s1_ts_loop,dTmds1_ts_loop,FTs1_ts_loop,Tm02_ts_loop,gradT2_ts_loop,FTr2_ts_loop,Tm0s2_ts_loop,dTmds2_ts_loop,FTs2_ts_loop,eplegT1_loop,eplegT2_loop,deltaS1_loop,deltaS2_loop,Smaxxx1_loop,Smaxxx2_loop,s_r0_a1_loop,s_r0_a2_loop,Sminnn1_loop,Sminnn2_loop,ISO_loop,B_basem_loop,Netech_loop,Ne0_s_loop,lambda_N_s_loop,Tm0s15_ts_loop,dTmds15_ts_loop,FTs15_ts_loop,Tm0s25_ts_loop,dTmds25_ts_loop,FTs25_ts_loop,Erlin1_F_loop,Erlin1_d_loop,Erlin1_o_loop,Erlin1_r0_loop,phir1_lin_loop,Erlin2_F_loop,Erlin2_d_loop,Erlin2_o_loop,Erlin2_r0_loop,phir2_lin_loop,Fcb5_loop

common loops_filter,Phir_loop_c,rad1_loop_c,rad2_loop_c,lat1_loop_c,lat2_loop_c,lon1_loop_c,lon2_loop_c,L_loop_c,opclstat_loop_c,r2Er1_loop_c,r2Er2_loop_c,Tmmean_loop_c,Fcb_loop_c,dTmds1_loop_c,dTmds2_loop_c,r2Tcuadr1_loop_c,r2Tcuadr2_loop_c,r2T1_loop_c,r2T2_loop_c,Ner01_loop_c,Ner02_loop_c,Ner11_loop_c,Ner12_loop_c,Ner21_loop_c,Ner22_loop_c,Ner31_loop_c,Ner32_loop_c,Rp_rad1_loop_c,Rp_lat1_loop_c,Rp_lon1_loop_c,Rp_rad2_loop_c,Rp_lat2_loop_c,Rp_lon2_loop_c,TmR11_loop_c,TmR12_loop_c,NR11_loop_c,NR12_loop_c,Nemean_loop_c,Long_cm_c,Phih_L_c,Ner0m_loop_c,lambda_N_loop_c,r2N1_loop_c,r2N2_loop_c,Tm01_ts_loop_c,gradT1_ts_loop_c,FTr1_ts_loop_c,Tm0s1_ts_loop_c,dTmds1_ts_loop_c,FTs1_ts_loop_c,Tm02_ts_loop_c,gradT2_ts_loop_c,FTr2_ts_loop_c,Tm0s2_ts_loop_c,dTmds2_ts_loop_c,FTs2_ts_loop_c,eplegT1_loop_c,eplegT2_loop_c,deltaS1_loop_c,deltaS2_loop_c,Smaxxx1_loop_c,Smaxxx2_loop_c,Sminnn1_loop_c,Sminnn2_loop_c,ISO_loop_c,B_basem_loop_c,Netech_loop_c,Ne0_s_loop_c,lambda_N_s_loop_c,Tm0s15_ts_loop_c,dTmds15_ts_loop_c,FTs15_ts_loop_c,Tm0s25_ts_loop_c,dTmds25_ts_loop_c,FTs25_ts_loop_c,Erlin1_F_loop_c,Erlin1_d_loop_c,Erlin1_o_loop_c,Erlin1_r0_loop_c,phir1_lin_loop_c,Erlin2_F_loop_c,Erlin2_d_loop_c,Erlin2_o_loop_c,Erlin2_r0_loop_c,phir2_lin_loop_c,Fcb5_loop_c

common label,T_label,Er_label

R2Crit_Ne = 0.75
FTscrit   = 0.70
FTs5crit  = 0.90
NeCritQ   = 1.4

print,n_elements(phir_loop)
 select = where(Abs(lat1_loop) le 30. and Abs(lat2_loop) le 30. and Phir_loop gt 0. $
           and NR11_loop/1.e8 le NeCritQ and NR12_loop/1.e8 le NeCritQ $
           and r2N1_loop ge r2crit_Ne and r2N2_loop ge r2crit_Ne $
           and FTs1_ts_loop ge FTscrit and FTs2_ts_loop ge FTscrit)

          ;and FTs15_ts_loop  ge FTs5crit  and FTs25_ts_loop  ge FTs5crit)

filter_loop,select

stop

print, 'Hacer análisis de AIAwarm ahora'
stop

p1select = p1w
p2select = p2w

stop 
file = 'traceLDEM_CRCR2099_AIA4-hot_reg0.75_safety0.5_radstart-1.035-1.215Rs_unifgrid_v2.heating.sampled.v2.CECI.dat'

  statloop,file,rloopmin=1.05,p1select=p1select,p2select=p2select,/AIAhot,/linear;,/Tfitlow;,/fitcuadr
  pegatina
print,n_elements(phir_loop)

stop
  filter_loop,select

print, 'Hacer análisis de AIAhot ahora'

stop

return
end

