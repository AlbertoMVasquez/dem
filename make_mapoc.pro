;Hecho por Fede Nuevo 2019.

pro hacer_make_mapoc
  file_input='github_dem/dem/traceLDEM_CR2223_ldem_adapt-gong_realization_0_radstart-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.105
  file_suffix='CR2223_90X180_fdips_realization_0'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  
  file_input='github_dem/dem/traceLDEM_CR2223_ldem_adapt-gong_realization_1_radstart-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.105
  file_suffix='CR2223_90X180_fdips_realization_1'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  
  file_input='traceLDEM_CR2223_ldem__radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.025+ 0.04 *findgen(6)
  file_suffix='CR2223_90X180_fdips'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

stop
  file_input='traceLDEM_CR2082_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.025+ 0.04 *findgen(6)
  file_suffix='CR2082_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/mhd
stop
  file_input= 'traceLDEM_CR2208_hollow_demt-data_field-awsom_6alt_radstart-1.025-1.225Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.025+ 0.04 *findgen(6)
  file_suffix='CR2208_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/mhd
  stop


  file_input='traceLDEM_CR2219_demt_pfss_gong_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

;testeo mapocs CR2219
  file_input='traceLDEM_CR2219_demt_pfss_gong_mrmqs_0713_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_campo_awsom_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_hmi_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_gong_0702_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_0709_R0_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  
  file_input='traceLDEM_CR2219_demt_pfss_gong_0709_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_0709_R1_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
;mapocs CR2223

  file_input='traceLDEM_CR2223_demt_pfss_gong_mrmqs_1030_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2223_demt_pfss_hmi_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2223_demt_pfss_1112_R1_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2223_demt_pfss_1023_R10_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_0702_R0_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2223_awsom__radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng


;OBS:este es gong mrnqs con zero point corrected
  file_input='traceLDEM_CR2219_demt_pfss_gong_mrnqs_0713_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  ;cambiando rssmax a 2.15 en lugar de 2.5
  file_input='traceLDEM_CR2219_demt_pfss_gong_mrnqs_0713_rmax215_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  ;daily shifteado zpc
  file_input='traceLDEM_CR2219_demt_pfss_gong_mrzqs_shifted_0716_rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng


;magnetograma diario, shifteado, zpc gong,a distintas alturas para
;rssmax en el fdips
  file_input='traceLDEM_CR2219_demt_pfss_gong_mrzqs_shifted_0716_rmax225_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_gong_mrzqs_shifted_0716_rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng


  file_input='traceLDEM_CR2219_demt_pfss_gong_mrzqs_shifted_0716_22hs__rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng ;file_output='CR2219_90X180blines_r_1.065_open-close-map_mrzqs_0716_22hs_shifted_rmax25.dat'
  
  file_input='traceLDEM_CR2219_demt_pfss_adaptgong_R1_0716_16hs_rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng 

  file_input='traceLDEM_CR2219_demt_pfss_adaptgong_R1_0716_16hs_rmax275_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_gong_mrzqs_shifted_0716_rmax275_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_gong_mrzqs_shifted_0716_22hs_rmax275_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  
  file_input='traceLDEM_CR2219_demt_pfss_hmi_0716_rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  
  file_input='traceLDEM_CR2219_demt_pfss_hmi_0716_rmax275_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_adaptgong_R1_0716_22hs_rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2219_demt_pfss_adaptgong_R1_0716_22hs_rmax275_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
;Para AGU con nuevas corridas awsom
  file_input='traceLDEM_CR2219_awsom_cocen_radstart-1.025Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.025
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2219_90X180blines_r_1.025_open-close-map_awsom_cocent.dat'
  
  file_input='traceLDEM_CR2219_awsom_cocen_radstart-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.105
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2219_90X180blines_r_1.105_open-close-map_awsom_cocent.dat'

  file_input='traceLDEM_CR2219_awsom_cocen_radstart-5.985Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=5.985
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng,/HCS
  file_output='CR2219_90X180blines_r_5.985_open-close-map_awsom_cocent.dat'  

;CR2223  
  file_input='traceLDEM_CR2223_demt_pfss_gong_mrzqs_shifted_0511_rmax225_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng


  file_input='traceLDEM_CR2223_demt_pfss_gong_mrzqs_shifted_0511_rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng


  file_input='traceLDEM_CR2223_demt_pfss_gong_mrzqs_shifted_0511_rmax275_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng


  file_input='traceLDEM_CR2223_demt_pfss_adapt-gong_0511_16hs_rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  
  file_input='traceLDEM_CR2223_demt_pfss_adapt-gong_0511_16hs_rmax275_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2219_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2223_demt_pfss_hmi_shifted_0511_12hs_rmax25_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='traceLDEM_CR2223_demt_pfss_hmi_shifted_0511_12hs_rmax275_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  

  
  file_input='traceLDEM_CR2223_demt_pfss_gong_mrnqs_1030_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=1.065
  file_suffix='CR2223_90X180blines_r_'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  

  file_input='github_dem/dem/traceLDEM_CR2223_ldem_adapt-gong_realization_0_3alturas_radstart-1.025-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.025,1.065,1.105]
  file_suffix='CR2223_90X180_fdips_realization_0_3alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='github_dem/dem/traceLDEM_CR2223_ldem_adapt-gong_realization_1_3alturas_radstart-1.025-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.025,1.065,1.105]
  file_suffix='CR2223_90X180_fdips_realization_1_3alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

  file_input='github_dem/dem/traceLDEM_CR2223_ldem_gong_3alturas_radstart-1.025-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.025,1.065,1.105]
  file_suffix='CR2223_90X180_fdips__3alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

;mapocs CR2223 a dos alturas demt + B awsom -> para charla AAA2020
file_input='traceLDEM_CR2223_demt_radstart-1.025Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.025]
  file_suffix='CR2223_90X180_fdips__3alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  
  file_input='traceLDEM_CR2223_demt_radstart-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.105]
  file_suffix='CR2223_90X180_fdips__3alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng

file_output='CR2223_90X180blines_r_1.065_open-close-map_hmi_shift_0511_12hs_rmax275.dat'
file_output='CR2223_90X180blines_r_1.025_open-close-map_awsom.dat'
file_output='CR2223_90X180blines_r_1.105_open-close-map_awsom.dat'
;mapoc a 6Rs.
 file_input='traceLDEM_CR2223_demt_radstart-5.995Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
 alturas=[5.995]
  file_suffix='CR2223_90X180_fdips__3alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2223_90X180blines_r_5.995_open-close-map_awsom.dat'

  file_input='traceLDEM_CR2223_awsom__radstart-5.995Rs_unifgrid_v2.heating.sampled.v2.DIEGO_2degx2deg.dat.sav'
;  'traceLDEM_CR2223_awsom__radstart-5.995Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
 alturas=[5.995]
  file_suffix='CR2223_90X180_fdips__3alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng,/HCS
  file_output='CR2223_90X180blines_r_5.995_open-close-map_awsom.dat'

;mapocs de AWSOM con magnetogramas adapt-gong COCENTRADOS -CR2219 CR2223
  file_input='traceLDEM_CR2219_awsom_cocen_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.065]
  file_suffix='CR2219_90X180_asd_3alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2219_90X180blines_r_1.065_open-close-map_awsom_cocent.dat'


  file_input='traceLDEM_CR2223_awsom_cocen_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.065]
  file_suffix='CR2223_90X180_asd_alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2223_90X180blines_r_1.065_open-close-map_awsom_cocent.dat'

;mapocs de AWSOM con magnetogramas GONG cocentrados 
  file_input='traceLDEM_CR2219_awsom_cocen_GONG_radstart-1.025Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.025]
  file_suffix='CR2219_90X180_asd_alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2219_90X180blines_r_1.025_open-close-map_awsom_GONG_cocent.dat'

  file_input='traceLDEM_CR2219_awsom_cocen_GONG_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.065]
  file_suffix='CR2219_90X180_asd_alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2219_90X180blines_r_1.065_open-close-map_awsom_GONG_cocent.dat'

  file_input='traceLDEM_CR2219_awsom_cocen_GONG_radstart-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.105]
  file_suffix='CR2219_90X180_asd_alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2219_90X180blines_r_1.105_open-close-map_awsom_GONG_cocent.dat'
  

  file_input='traceLDEM_CR2223_awsom_cocen_GONG_radstart-1.025Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.025]
  file_suffix='CR2223_90X180_asd_alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2223_90X180blines_r_1.025_open-close-map_awsom_GONG_cocent.dat'

  file_input='traceLDEM_CR2223_awsom_cocen_GONG_radstart-1.065Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.065]
  file_suffix='CR2223_90X180_asd_alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2223_90X180blines_r_1.065_open-close-map_awsom_GONG_cocent.dat'

  file_input='traceLDEM_CR2223_awsom_cocen_GONG_radstart-1.105Rs_unifgrid_v2.heating.sampled.v2.DIEGO.dat.sav'
  alturas=[1.105]
  file_suffix='CR2223_90X180_asd_alturas'
  MAKE_MAPOC,file_input,file_suffix,alturas,/gng
  file_output='CR2223_90X180blines_r_1.105_open-close-map_awsom_GONG_cocent.dat'



  
  
  return
end




pro MAKE_MAPOC,file_input,file_suffix,alturas,mdi=mdi,gng=gng,mhd=mhd,dir_input=dir_input,dir_output=dir_output,HCS=HCS
;/HCS si usamos mapa de Br, hace el "mapoc" de la HCR.
  if not keyword_set(dir_input) then dir_input='/data1/work/dem/'
  if not keyword_set(dir_output) then dir_output='/data1/work/MHD/'

  print,'calcula mapa open-close en:',alturas

  print,'leyendo trazado: ',file_input
  restore,dir_input+file_input

  if keyword_set(mdi) then postsuffix='_MDI.dat'
  if keyword_set(gng) then postsuffix='_GNG.dat'
  if keyword_set(mhd) then postsuffix='_MHD.dat'

  nr=n_elements(alturas)
  for ir=0L,nr-1 do begin
     altura=alturas(ir)
     stringheight=strmid(string(altura),6,5)
     file_output=file_suffix+stringheight+'_open-close-map'+postsuffix
     print,'-----> O/C: '+file_output
     index=where(str_v eq altura)
     stth = stth_v(index)
     stph = stph_v(index) 
     
     nlat=90
     nlon=180
     dlat=2.
     dlon=2.
     lat = -90. + dlat /2. + dlat * findgen(nlat)
     lon =   0. + dlon /2. + dlon * findgen(nlon)

     stlon = round(    stph/!dtor)*1.
     stlat = round(90.-stth/!dtor)*1.
     N = n_elements(stlon)

     mapoc = fltarr(1,nlat,nlon)

     if not keyword_set(HCS) then begin
        for i=0L,N-1 do begin
           ilat = where(lat eq stlat(i))
           ilon = where(lon eq stlon(i))       
           mapoc(0,ilat,ilon) = opcls(i)
        endfor  
;quedo: 0=abierto o indeterminado
;       1=cerrado
;cambiar al final a:  (open = 10) y (closed = 0.1)
        iopen   = where ( mapoc eq 0 )
        iclosed = where ( mapoc eq 1 or mapoc eq 2)
     endif else begin ;entra aca si estoy calculando la HCS

        for i=0L,N-1 do begin
;           if min(abs(lat-stlat(i))) ne 0. then stop
           ;if min(abs(lon-stlon(i))) ne 0. then stop ;goto,skip
;           if min(abs(lon-stlon(i))) ne 0. then stop ;goto,skip
           ;ilat = where(lat eq stlat(i))
           ;ilon = where(lon eq stlon(i))
           ilon = (where(min(abs(lon-stlon(i))) eq (abs(lon-stlon(i)))) )(0)
           ilat = (where(min(abs(lat-stlat(i))) eq (abs(lat-stlat(i)))) )(0) 
           if n_elements(ilat) ne 1 or n_elements(ilon) ne 1 then stop

           posicion_r = (where(abs(rad_v(*,i)-altura) le 1.e-3))(0)
           mapoc(0,ilat,ilon) = Br_v(posicion_r,i) ;500
           skip:
        endfor

        iopen   = where ( mapoc ge 0 )
        iclosed = where ( mapoc le 0.000000001 )
;        stop
        test=fltarr(1,90,180)
        for i=0,179 do begin
           test(0,*,i)=lat
        endfor
     endelse

     mapoc(iopen)   = 10
     mapoc(iclosed) = 0.1

     if keyword_set(HCS) then begin
        mapoc(where(test le -50))=0.1
        mapoc(where(test ge 50))=10        
     endif
     
     stop
     openw,1,dir_output+file_output
     writeu,1,mapoc
     close,1
  endfor

  return
end
