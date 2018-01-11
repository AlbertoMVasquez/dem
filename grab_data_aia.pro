pro grab_data_aia,date1,date2,cadence,basedir

; cadence: integer, cadence in [hr] 

; Examples:
;
; Use of grab data to get a full tomographic period:
; grab_data_aia,'2011-01-01','2011-01-02',1,'/data1/tomography/DATA/aia/Jan-2011/'
;
; Direct use of the SSW routines to get high-cadence data for a short 6 hr period:
; B171 = vso_search(date='2009-03-18T15:30 - 2009-03-18T16:30',inst='EUVI',sample=100L,source='STEREO_B',wave='171 Angstrom')
; A195 = vso_get(A195,/force,out_dir='/media/disk/data1/tomography/DATA/euvi/2009.03/hicad195_1day/')

;cadence = cadence * 3600L

;STOP
; grab_data_aia,'2017-01-10','2017-02-06',10,'/data1/work/Minimum_2017_diego/' 
;server = 'sao'

 dir094=basedir+'094/'
 dir131=basedir+'131/'
 dir171=basedir+'171/'
 dir193=basedir+'193/'
 dir211=basedir+'211/'
 dir335=basedir+'335/'
 dir304=basedir+'304/'

;goto,hicad

 cadence094 = cadence*3600L
 cadence131 = cadence*3600L
 cadence171 = cadence*3600L
 cadence193 = cadence*3600L
 cadence211 = cadence*3600L
 cadence335 = cadence*3600L
 cadence304 = cadence*3600L

 pix = 4096

 a171=vso_search(date1,date2, inst='AIA',sample=cadence171,wave='171 Angstrom',pixels=pix,level=lev1)
 a193=vso_search(date1,date2, inst='AIA',sample=cadence193,wave='193 Angstrom',pixels=pix,level=lev1)
 a211=vso_search(date1,date2, inst='AIA',sample=cadence211,wave='211 Angstrom',pixels=pix,level=lev1)
 a335=vso_search(date1,date2, inst='AIA',sample=cadence335,wave='335 Angstrom',pixels=pix,level=lev1)
 a131=vso_search(date1,date2, inst='AIA',sample=cadence131,wave='131 Angstrom',pixels=pix,level=lev1)
 a094=vso_search(date1,date2, inst='AIA',sample=cadence094,wave='094 Angstrom',pixels=pix,level=lev1)
 a304=vso_search(date1,date2, inst='AIA',sample=cadence304,wave='304 Angstrom',pixels=pix,level=lev1)

stop

 a171=vso_get(a171,/force,out_dir=dir171,/rice,site=server)   
 a193=vso_get(a193,/force,out_dir=dir193,/rice,site=server)   
 a211=vso_get(a211,/force,out_dir=dir211,/rice,site=server)   
 a335=vso_get(a335,/force,out_dir=dir335,/rice,site=server)   
 a131=vso_get(a131,/force,out_dir=dir131,/rice,site=server)   
 a094=vso_get(a094,/force,out_dir=dir094,/rice,site=server)   
;a304=vso_get(a304,/force,out_dir=dir304,/rice,site=server)

return
end
