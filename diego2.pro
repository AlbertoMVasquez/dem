%
suffix = '-decon'

r2crit_T = 0.95

; piernas de loops chicos con pie en el rango de latitud [-30,+30]
i = where(gradT ne -555. AND opclstat eq 2. and r2N gt r2crit_T and abs(footlat) le 30.)
filter,i
footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel='low-lat-small'+suffix,indexloop_c,rotacion
histoplot,lambda_N_c     ,min=0,max=0.2,nbins=100,xtit='!4k!3!DN!N [R!DSUN!N]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_lambdaN'+suffix
histoplot,     Ne0_c/1.e8,min=0,max=5.0,nbins=100,xtit='T!De0!N [MK]',ytit='Frequency Histogram',tit='Low latitude closed small loops '+suffix,filename='low-lat-small_histo_Ne0'+suffix

; piernas de loops chicos con pie en el rango de abs(latitud) (30,90]
i = where(gradT ne -555. AND opclstat eq 2. and r2N gt r2crit_T and abs(footlat) gt 30.)
filter,i
footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel='mid-lat-small'+suffix,indexloop_c,rotacion
histoplot,lambda_N_c     ,min=0,max=0.2,nbins=100,xtit='!4k!3!DN!N [R!DSUN!N]',ytit='Frequency Histogram',tit='Mid latitude closed small loops '+suffix,filename='mid-lat-small_histo_lambdaN'+suffix
histoplot,     Ne0_c/1.e8,min=0,max=5.0,nbins=100,xtit='T!De0!N [MK]',ytit='Frequency Histogram',tit='Mid latitude closed small loops '+suffix,filename='mid-lat-small_histo_Ne0'+suffix

; piernas de loops grandes
i = where(gradT ne -555. AND opclstat eq 1. and r2N ge r2crit_T)
filter,i
footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel='large'+suffix,indexloop_c,rotacion
histoplot,lambda_N_c     ,min=0,max=0.2,nbins=100,xtit='!4k!3!DN!N [R!DSUN!N]',ytit='Frequency Histogram',tit='Closed large loops '+suffix,filename='large_histo_lambdaN'+suffix
histoplot,     Ne0_c/1.e8,min=0,max=5.0,nbins=100,xtit='T!De0!N [MK]',ytit='Frequency Histogram',tit='Closed Large loops '+suffix,filename='large_histo_Ne0'+suffix

; piernas de loops grandes de la O/C boundary sur
i = where(gradT ne -555. AND opclstat eq 1. and r2N ge r2crit_T and footlat lt -40.)
filter,i
footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel='large-OCS'+suffix,indexloop_c,rotacion
histoplot,lambda_N_c     ,min=0,max=0.2,nbins=100,xtit='!4k!3!DN!N [R!DSUN!N]',ytit='Frequency Histogram',tit='Closed large loops O/C South '+suffix,filename='large-OCS_histo_lambdaN'+suffix
histoplot,     Ne0_c/1.e8,min=0,max=5.0,nbins=100,xtit='T!De0!N [MK]',ytit='Frequency Histogram',tit='Closed large loops O/C South '+suffix,filename='large-OCS_histo_Ne0'+suffix

; piernas de loops grandes de la O/C boundary norte
i = where(gradT ne -555. AND opclstat eq 1. and r2N ge r2crit_T and footlat gt +40.)
filter,i
footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel='large-OCN'+suffix,indexloop_c,rotacion
histoplot,lambda_N_c     ,min=0,max=0.2,nbins=100,xtit='!4k!3!DN!N [R!DSUN!N]',ytit='Frequency Histogram',tit='Closed large loops OC/South '+suffix,filename='large-OCN_histo_lambdaN'+suffix
histoplot,     Ne0_c/1.e8,min=0,max=5.0,nbins=100,xtit='T!De0!N [MK]',ytit='Frequency Histogram',tit='Closed large loops O/C South '+suffix,filename='large-OCN_histo_Ne0'+suffix

; piernas de lineas abiertas del polo sur 
i = where(gradT ne -555. AND opclstat eq 0. and r2N ge r2crit_T and footlat lt -50.)
filter,i
footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel='open-south'+suffix,indexloop_c,rotacion
histoplot,lambda_N_c     ,min=0,max=0.2,nbins=100,xtit='!4k!3!DN!N [R!DSUN!N]',ytit='Frequency Histogram',tit='Open south loops '+suffix,filename='open-south_histo_lambdaN'+suffix
histoplot,     Ne0_c/1.e8,min=0,max=5.0,nbins=100,xtit='T!De0!N [MK]',ytit='Frequency Histogram',tit='Open south loops '+suffix,filename='open-south_histo_Ne0'+suffix

; piernas de lineas abiertas del polo norte 
i = where(gradT ne -555. AND opclstat eq 0. and r2N ge r2crit_T and footlat gt +50.)
filter,i
footpoint_map,footlat_c,footlon_c,opclstat_c,filelabel='open-north'+suffix,indexloop_c,rotacion
histoplot,lambda_N_c     ,min=0,max=0.2,nbins=100,xtit='!4k!3!DN!N [R!DSUN!N]',ytit='Frequency Histogram',tit='Open north loops '+suffix,filename='open-north_histo_lambdaN'+suffix
histoplot,     Ne0_c/1.e8,min=0,max=5.0,nbins=100,xtit='T!De0!N [MK]',ytit='Frequency Histogram',tit='Open north loops '+suffix,filename='open-north_histo_Ne0'+suffix

