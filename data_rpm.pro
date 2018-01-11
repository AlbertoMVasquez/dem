; This code is the generalization of eit_rpm to include data from
; multiple spacecraft: EIT/SOHO, EUVI-A and EUVI-B / STEREO, XRT/HINODE
; 05.29.2008 amv.

; data_rpm,'/data1','/data1/tomography/DATA/euvi/2007.01_4perday/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/dnsec,/despk,/normalize

; data_rpm,'/data1','/data1/tomography/DATA/euvi/2007.02.25/test/',index,data,filenames,nfiles,1,/setbinfactor,/read,/euvi,/dnsec  ,/despk
; data_rpm,'/data1','/data1/tomography/DATA/euvi/2007.02.25/',index,data,filenames,nfiles,1,/setbinfactor,/read,/euvi,/photons,/despk

; data_rpm,'/data1','/media/disk/data1/tomography/DATA/euvi/2008.04/X/',index,data,filenames,nfiles,4,/setbinfactor,/read,/euvi,/dnsec,/despk

; data_rpm,'/','/media/disk/data1/tomography/DATA/euvi/2007.02.25/',index,data,filenames,nfiles,1,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,rescale
; data_rpm,'/','/media/disk/data1/tomography/DATA/euvi/2007.02.25/',index,data,filenames,nfiles,1,/setbinfactor,/read,/euvi,/despk,/photons

; data_rpm,'/','/data1/tomography/DATA/euvi/2007.02.25/','/data1/tomography/DATA/euvi/2007.02.25/',index,data,filenames,nfiles,1,/setbinfactor,/read,/euvi,/photons,/norot

; data_rpm,'/data1','/data1/tomography/DATA/euvi/2009.03/alldata/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale
; data_rpm,'/data1','/data1/tomography/DATA/euvi/2009.03/alldata/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/photons

; timebinning,'/data1','/data1/tomography/DATA/euvi/2009.03/alldata/dnsec/',4,/euvi

; makemovie,'/data1','/media/disk/data1/tomography/DATA/euvi/2009.03/alldata/dnsec/b4/',/euvi,/save,/correctscale,/time,/dnsec

; data_rpm,'/data1','/data1/tomography/DATA/euvi/2009.03/X/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale

; data_rpm,'/data1','/data1/tomography/DATA/euvi/2008.04/Y/',index,data,filenames,nfiles,4,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale

; data_rpm,'/','/data1/tomography/DATA/euvi/CR2077/','/data1/tomography/DATA/euvi/CR2077/',index,data,filenames,nfiles,1,/setbinfactor,/read,/euvi,/dnsec,/normalize,/rescale,/despk,/rot

; data_rpm,'/','/data1/tomography/DATA/eit/CR2077/','/data1/tomography/DATA/eit/CR2077/',index,data,filenames,nfiles,2,/setbinfactor,/read,/eit,/normalize,/rescale,/despk,/rot

;-------------------------------------------------------------------------------------
pro analysis_cycle

data_rpm,'/','/data1/tomography/DATA/euvi/CR2081/B171/test/','/data1/tomography/DATA/euvi/CR2081/B171/test/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot;,/decon

return

data_rpm,'/','/data1/tomography/DATA/euvi/CR2082/B171/test/','/data1/tomography/DATA/euvi/CR2082/B171/test/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot;,/decon
return
data_rpm,'/','/data1/tomography/DATA/euvi/CR2082/B195/S4N5/','/data1/tomography/DATA/euvi/CR2082/B195/S4N5/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot;,/decon
data_rpm,'/','/data1/tomography/DATA/euvi/CR2082/B284/S4N5/','/data1/tomography/DATA/euvi/CR2082/B284/S4N5/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot;,/decon

goto,fin
data_rpm,'/','/data1/tomography/DATA/euvi/CR2082/B304/','/data1/tomography/DATA/euvi/CR2082/B304/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot ,/decon

goto,fin

data_rpm,'/','/data1/tomography/DATA/euvi/CR2106/Extra-A/A171/X/','/data1/tomography/DATA/euvi/CR2106/Extra-A/A171/X/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2074/B195/','/data1/tomography/DATA/euvi/CR2074/B195/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2074/B284/','/data1/tomography/DATA/euvi/CR2074/B284/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2074/B304/','/data1/tomography/DATA/euvi/CR2074/B304/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

goto,fin

data_rpm,'/','/data1/tomography/DATA/euvi/CR2074/B171/','/data1/tomography/DATA/euvi/CR2074/B171/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/data1/tomography/DATA/euvi/CR2056/B171/','/data1/tomography/DATA/euvi/CR2056/B171/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2056/B195/','/data1/tomography/DATA/euvi/CR2056/B195/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2056/B284/','/data1/tomography/DATA/euvi/CR2056/B284/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2056/B304/','/data1/tomography/DATA/euvi/CR2056/B304/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

goto,fin

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2078/B171/','/Storage1TB/tomography/DATA/euvi/CR2078/B171/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2078/B195/','/Storage1TB/tomography/DATA/euvi/CR2078/B195/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2078/B284/','/Storage1TB/tomography/DATA/euvi/CR2078/B284/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

goto,fin

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2055/B171/','/Storage1TB/tomography/DATA/euvi/CR2055/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2055/B195/','/Storage1TB/tomography/DATA/euvi/CR2055/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2055/B284/','/Storage1TB/tomography/DATA/euvi/CR2055/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2055/B304/','/Storage1TB/tomography/DATA/euvi/CR2055/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2055/A171/','/Storage1TB/tomography/DATA/euvi/CR2055/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2055/A195/','/Storage1TB/tomography/DATA/euvi/CR2055/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2055/A284/','/Storage1TB/tomography/DATA/euvi/CR2055/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2055/A304/','/Storage1TB/tomography/DATA/euvi/CR2055/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

goto,fin

data_rpm,'/','/data2/CR2095/B171/','/Storage1TB/tomography/DATA/euvi/CR2095/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data2/CR2095/B195/','/Storage1TB/tomography/DATA/euvi/CR2095/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data2/CR2095/B284/','/Storage1TB/tomography/DATA/euvi/CR2095/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data2/CR2095/B304/','/Storage1TB/tomography/DATA/euvi/CR2095/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data2/CR2095/A171/','/Storage1TB/tomography/DATA/euvi/CR2095/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data2/CR2095/A195/','/Storage1TB/tomography/DATA/euvi/CR2095/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data2/CR2095/A284/','/Storage1TB/tomography/DATA/euvi/CR2095/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data2/CR2095/A304/','/Storage1TB/tomography/DATA/euvi/CR2095/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/data1/tomography/DATA/euvi/CR2071/B304/','/Storage1TB/tomography/DATA/euvi/CR2071/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2071/A171/','/Storage1TB/tomography/DATA/euvi/CR2071/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2071/A195/','/Storage1TB/tomography/DATA/euvi/CR2071/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2071/A284/','/Storage1TB/tomography/DATA/euvi/CR2071/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/data1/tomography/DATA/euvi/CR2071/A304/','/Storage1TB/tomography/DATA/euvi/CR2071/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

fin:
return
end



;-------------------------------------------------------------------------------------
pro analysis_cycle_cr2077_interdata

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Intermediate_data/B284/','/Storage1TB/tomography/DATA/euvi/CR2077/Intermediate_data/B284/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_extra

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/B171/','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/B195/','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/B284/','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/B304/','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot


data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2068/B171/','/Storage1TB/tomography/DATA/euvi/CR2068/B171/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2068/B195/','/Storage1TB/tomography/DATA/euvi/CR2068/B195/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2068/B284/','/Storage1TB/tomography/DATA/euvi/CR2068/B284/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2068/B304/','/Storage1TB/tomography/DATA/euvi/CR2068/B304/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_cr2077

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/B171/','/Storage1TB/tomography/DATA/euvi/CR2077/Despiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/B195/','/Storage1TB/tomography/DATA/euvi/CR2077/Despiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/B284/','/Storage1TB/tomography/DATA/euvi/CR2077/Despiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/B171/','/Storage1TB/tomography/DATA/euvi/CR2077/Undespiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/B195/','/Storage1TB/tomography/DATA/euvi/CR2077/Undespiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/B284/','/Storage1TB/tomography/DATA/euvi/CR2077/Undespiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/A171/','/Storage1TB/tomography/DATA/euvi/CR2077/Despiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/A195/','/Storage1TB/tomography/DATA/euvi/CR2077/Despiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/A284/','/Storage1TB/tomography/DATA/euvi/CR2077/Despiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/A171/','/Storage1TB/tomography/DATA/euvi/CR2077/Undespiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/A195/','/Storage1TB/tomography/DATA/euvi/CR2077/Undespiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/A284/','/Storage1TB/tomography/DATA/euvi/CR2077/Undespiked/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/dnsec,/normalize,/rescale,/norot

goto,fin

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/A304/','/Storage1TB/tomography/DATA/euvi/CR2077/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/B304/','/Storage1TB/tomography/DATA/euvi/CR2077/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

fin:
return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_cr2077_extra_data

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/B284/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/photon,/normalize,/rescale,/norot

return

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/B171/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/B195/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/B284/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/B304/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/A171/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/A195/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/A284/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/A304/','/Storage1TB/tomography/DATA/euvi/CR2077/High_cadence_sample/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/B171/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/B195/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/B284/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/A171/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/A195/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/A284/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

return

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/B304/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/A304/','/Storage1TB/tomography/DATA/euvi/CR2077/Complement/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

end

;-----------------------------------------------------------------------
pro analysis_cycle_euvi_2107

 data_rpm,'/','/data1/tomography/DATA/euvi/CR2107/B171/','/data1/tomography/DATA/euvi/CR2107/B171/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
 data_rpm,'/','/data1/tomography/DATA/euvi/CR2107/B195/','/data1/tomography/DATA/euvi/CR2107/B195/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
 data_rpm,'/','/data1/tomography/DATA/euvi/CR2107/B284/','/data1/tomography/DATA/euvi/CR2107/B284/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
;data_rpm,'/','/data1/tomography/DATA/euvi/CR2107/B304/','/data1/tomography/DATA/euvi/CR2107/B304/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

return
end

;-----------------------------------------------------------------------
pro analysis_cycle_euvi_2106
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/B171/','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/B195/','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/B284/','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/B304/','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

return

data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2106/A171/','/Storage1TB/tomography/DATA/euvi/CR2106/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2106/A195/','/Storage1TB/tomography/DATA/euvi/CR2106/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2106/A284/','/Storage1TB/tomography/DATA/euvi/CR2106/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2106/A304/','/Storage1TB/tomography/DATA/euvi/CR2106/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

fin:
return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_eit_cr2100

;data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2100/A171/','/Storage1TB/tomography/DATA/euvi/CR2100/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot
 data_rpm,'/','/Storage1TB/tomography/DATA/euvi/CR2100/A195/','/Storage1TB/tomography/DATA/euvi/CR2100/',index,data,filenames,nfiles,2,/setbinfactor,/read,/euvi,/despk,/dnsec,/normalize,/rescale,/norot

return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_eit_cr2029

data_rpm,'/','/data1/tomography/DATA/eit/CR2029/171/','data1/tomography/DATA/eit/CR2029/171/',index,data,filenames,nfiles,1,/setbinfactor,/read,/eit,/norot,/normalize, /despk
data_rpm,'/','/data1/tomography/DATA/eit/CR2029/195/','data1/tomography/DATA/eit/CR2029/195/',index,data,filenames,nfiles,1,/setbinfactor,/read,/eit,/norot,/normalize, /despk
data_rpm,'/','/data1/tomography/DATA/eit/CR2029/284/','data1/tomography/DATA/eit/CR2029/284/',index,data,filenames,nfiles,1,/setbinfactor,/read,/eit,/norot,/normalize, /despk

return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_eit_cr1988

data_rpm,'/','/data1/tomography/DATA/eit/CR1988/284/test/','data1/tomography/DATA/eit/CR1988/284/test/',index,data,filenames,nfiles,1,/setbinfactor,/read,/eit,/norot,/normalize, /despk

data_rpm,'/','/data1/tomography/DATA/eit/CR1988/171/','data1/tomography/DATA/eit/CR1988/171/',index,data,filenames,nfiles,1,/setbinfactor,/read,/eit,/norot,/normalize, /despk
data_rpm,'/','/data1/tomography/DATA/eit/CR1988/195/','data1/tomography/DATA/eit/CR1988/195/',index,data,filenames,nfiles,1,/setbinfactor,/read,/eit,/norot,/normalize, /despk
data_rpm,'/','/data1/tomography/DATA/eit/CR1988/284/','data1/tomography/DATA/eit/CR1988/284/',index,data,filenames,nfiles,1,/setbinfactor,/read,/eit,/norot,/normalize, /despk

return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_eit_WSM
data_rpm,'/','/data1/tomography/DATA/eit/WSM/','/Storage1TB/tomography/DATA/eit/WSM/',index,data,filenames,nfiles,2,/setbinfactor,/read,/eit,/norot,/normalize,/despk
return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_eit_cr1911
data_rpm,'/','/Storage1TB/tomography/DATA/eit/CR1911/','/Storage1TB/tomography/DATA/eit/CR1911/',index,data,filenames,nfiles,2,$
/setbinfactor,/read,/eit,/norot,/normalize,/despk
return
end

;-------------------------------------------------------------------------------------
pro analysis_cycle_eit_test
data_rpm,'/','/data1/tomography/DATA/eit/test/','/data1/tomography/DATA/eit/test/',index,data,filenames,nfiles,1,/setbinfactor,/read,/eit,/norot,/despk,/normalize
return
end

;-------------------------------------------------------------------------------------
pro timebinning_cycle

 timebinning,'','/data1/tomography/DATA/eit/CR1919/171/','list.171.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1919/195/','list.195.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1919/284/','list.284.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1919/304/','list.304.proc.selected',4,/eit

 timebinning,'','/data1/tomography/DATA/eit/CR1915/171/','list.171.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1915/195/','list.195.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1915/284/','list.284.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1915/304/','list.304.proc.selected',4,/eit

 timebinning,'','/data1/tomography/DATA/eit/CR1914/171/','list.171.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1914/195/','list.195.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1914/284/','list.284.proc.selected',4,/eit
 timebinning,'','/data1/tomography/DATA/eit/CR1914/304/','list.304.proc.selected',4,/eit

return

 timebinning,'','/data1/tomography/DATA/euvi/CR2081/A171n/','list.A171.proc.nodecon.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/A195n/','list.A195.proc.nodecon.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/A284n/','list.A284.proc.nodecon.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/A304n/','list.A304.proc.nodecon.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/A171n/','list.A171.proc.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/A195n/','list.A195.proc.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/A284n/','list.A284.proc.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/A304n/','list.A304.proc.selected',4,/euvi

return

 timebinning,'','/data1/tomography/DATA/euvi/CR2081/B171/','list.B171.proc.nodecon.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.proc.nodecon.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/B284/','list.B284.proc.nodecon.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/B171/','list.B171.proc.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.proc.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/B284/','list.B284.proc.selected',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2081/B304/','list.B304.proc.selected',4,/euvi

return

 timebinning,'','/data1/tomography/DATA/euvi/CR2081/B195/','list.B195.proc.selected.2ndbin',4,/euvi

 timebinning,'','/data1/tomography/DATA/euvi/CR2082/B171/','listB171.nodecon',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2082/B195/','listB195.nodecon',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2082/B284/','listB284.nodecon',4,/euvi

return

 timebinning,'','/data1/tomography/DATA/euvi/CR2074/B195/','list.B195',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2074/B284/','list.B284',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2074/B304/','list.B304',4,/euvi

return

 timebinning,'','/data1/tomography/DATA/euvi/CR2074/B171/','list.B171',4,/euvi

 timebinning,'','/data1/tomography/DATA/euvi/CR2056/B171/','list.B171',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2056/B195/','list.B195',4,/euvi
 timebinning,'','/data1/tomography/DATA/euvi/CR2056/B284/','list.B284.selected',4,/euvi


return

 timebinning,'','/data1/tomography/DATA/euvi/CR2056/B304/','list.B304.selected',4,/euvi

return

timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2068/B171/','list.B171.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2068/B195/','list.B195.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2068/B284/','list.B284.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2068/B304/','list.B304.selected',4,/euvi

return

timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/B171/','list.B171',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/B195/','list.B195',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/B284/','list.B284',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2077/Extra.3.days/B304/','list.B304',4,/euvi

return

timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/B171/','list.171.B.extra.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/B195/','list.195.B.extra.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/B284/','list.284.B.extra.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2106/Extra-B/B304/','list.304.B.extra.selected',4,/euvi

return

timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2106/A171/','list.171.A.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2106/A195/','list.195.A.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2106/A284/','list.284.A.selected',4,/euvi
timebinning,'','/Storage1TB/tomography/DATA/euvi/CR2106/A304/','list.304.A.selected',4,/euvi

return
end

;-------------------------------------------------------------------------------------
pro makemovie_cycle

;makemovie, '/Storage1TB/tomography/DATA/euvi/CR2072-2073/time-binned/','/Storage1TB/tomography/DATA/euvi/CR2072-2073/time-binned/',/euvi,/save,/correctscale,/time,/dnsec
;makemovie, '/Storage1TB/tomography/DATA/euvi/CR2068/','/Storage1TB/tomography/DATA/euvi/CR2068/',/euvi,/save,/correctscale,/time,/dnsec
 makemovie, '/Storage1TB/tomography/DATA/euvi/CR2077/b4/','/Storage1TB/tomography/DATA/euvi/CR2077/b4/',/euvi,/save,/correctscale,/time,/dnsec

return
end

;-------------------------------------------------------------------------------------
pro data_rpm,basedir,data_dir,out_dir,index,data,filenames, nfiles, bf ,$
             setbinfactor=setbinfactor,read=read,$
             eit=eit,euvi=euvi,xrt=xrt,medianfilter=medianfilter,$
             PHOTONS=PHOTONS,DNSEC=DNSEC,despk=despk,$
             NORMALIZE=NORMALIZE,RESCALE=RESCALE,rot=rot,norot=norot,decon=decon

common parameters,binfactor,normalization,rescaling,rotating,deconvolution

; first look into given DIR and make a list of the yet UNPROCESSED DATA files.
if keyword_set(read) and keyword_set(eit) then $
reading, basedir, data_dir, filenames, nfiles, /eit

if keyword_set(read) and keyword_set(euvi) and keyword_set(DNSEC)   then $
reading, basedir, data_dir, filenames, nfiles, /euvi,/DNSEC
if keyword_set(read) and keyword_set(euvi) and keyword_set(PHOTONS) then $
reading, basedir, data_dir, filenames, nfiles, /euvi,/PHOTONS

if keyword_set(read) and keyword_set(xrt) then $
reading, basedir, data_dir, filenames, nfiles, /xrt

medfilt=0
if keyword_set(medianfilter) then medfilt=1
dspk=0
if keyword_set(despk)        then dspk=1

if keyword_set(eit)  then binfactor=2
if keyword_set(euvi) then binfactor=4
if keyword_set(setbinfactor) then binfactor=bf

print,'BINFACTOR:',binfactor
print,'MEDFILT:  ',medfilt

deconvolution=0
normalization=0
rescaling    =0
rotating     =-1
if keyword_set(decon) then deconvolution = 1
if keyword_set(norot) then rotating = 0
if keyword_set(  rot) then rotating = 1
if rotating eq -1 then stop
; now select from that list the desired ones, calibrate them,
; generate a file 'list_of_files.out' with ALL files and their status,
; and generate new FITSFILEs with the CALIBRATED IMAGE 
if keyword_set(eit) then begin
  if keyword_set(normalize) then normalization=1 
  process_data_eit,  base_dir, data_dir, filenames, nfiles,medfilt,dspk
endif

if keyword_set(euvi) then begin
  if keyword_set(normalize) then normalization=1
  if keyword_set(rescale)   then rescaling    =1
  if keyword_set(PHOTONS) then $
  process_data_euvi, base_dir, data_dir, out_dir, filenames, nfiles,medfilt,dspk,/PHOTONS
  if keyword_set(DNSEC)   then $
  process_data_euvi, base_dir, data_dir, out_dir, filenames, nfiles,medfilt,dspk,/DNSEC
endif

if keyword_set(xrt) then $
process_data_xrt,  base_dir, data_dir, filenames, nfiles,medfilt

return
end

;-----------------------------------------------------------------------------------
pro reading, basedir, data_dir, filenames, nfiles,eit=eit,euvi=euvi,xrt=xrt,photons=photons,dnsec=dnsec

;basedir='/data1'

!path = '+'+basedir+'/dem/idl:'+!path
!path=expand_path(!path)
;use_chianti,basedir+'/dem/dbase/'

filenames=findfile(data_dir)


if n_elements(filenames) eq 0 then begin
 print,'NO FILES TO READ!'
 stop
endif

; ELIMINATE FILES THAT ARE NOT EIT or EUVI or XRT FILES
; OR THAT HAVE BEEN ALREADY PROCESSED:

if keyword_set(eit) then begin
d=intarr(n_elements(filenames))
u = where(strmid(filenames,0,3) eq 'efz' and strlen(filenames) eq 18)
if u(0) eq -1 then begin
print,'THERE ARE NO UNPROCESSED EIT FILES TO PROCESS HERE!'
stop
endif
ufiles=filenames(u)
; do not count again unprocessed files for which there is already a processed file.
d = intarr(n_elements(ufiles))+1
p = where(strlen(filenames) eq 26 and strmid(filenames,20,3) NE '.b.')
if p(0) ne -1 then begin
   pfiles=filenames(p)
   for i=0,n_elements(pfiles)-1 do begin
     string=strmid(pfiles(i),0,18)
     j=(where(ufiles eq string))(0)
     if j ne -1 then d(j)=0
   endfor
endif
dd=where(d eq 1)
if dd(0) ne -1 then ufiles=ufiles(dd)
if dd(0) eq -1 then stop ; !!!
endif

if keyword_set(euvi) then begin
d=intarr(n_elements(filenames))
u = where(strmid(filenames,18,2) eq 'eu' and strlen(filenames) eq 25)
if u(0) eq -1 then begin
print,'THERE ARE NO UNPROCESSED EUVI FILES TO PROCESS HERE!'
stop
endif
ufiles=filenames(u)
; do not count again unprocessed files for which there is already a processed file.
; unless PHOTON images are requested.
processedlenght=79
d = intarr(n_elements(ufiles))+1
p = where(strmid(filenames,18,2) eq 'eu' and strlen(filenames) eq processedlenght)
if p(0) ne -1 then begin
   pfiles=filenames(p)
   for i=0,n_elements(pfiles)-1 do begin
     string=strmid(pfiles(i),0,21)
     j=(where(strmid(ufiles,0,21) eq string))(0)
     if j ne -1 then d(j)=0
   endfor
endif
dd=where(d eq 1)
if dd(0) ne -1 then ufiles=ufiles(dd)
if dd(0) eq -1 then stop ; !!!
useall:
endif

if keyword_set(xrt) then begin
d=intarr(n_elements(filenames))
u = where(strmid(filenames,0,3) eq 'XRT' and strlen(filenames) eq 25)
if u(0) eq -1 then begin
print,'THERE ARE NO UNPROCESSED XRT FILES TO PROCESS HERE!'

endif
u = where(strmid(filenames,0,3) eq 'XRT' and strlen(filenames) eq 25)
if u(0) eq -1 then begin
print,'THERE ARE NO UNPROCESSED FILES HERE!!!'

endif
ufiles=filenames(u)
; do not count again unprocessed files for which there is already a processed file.
d = intarr(n_elements(ufiles))+1
p = where(strmid(filenames,21,2) eq 'fw' and strlen(filenames) eq 30)
if p(0) ne -1 then begin
   pfiles=filenames(p)
   for i=0,n_elements(pfiles)-1 do begin
     string=strmid(pfiles(i),0,20)
     j=(where(strmid(ufiles,0,20) eq string))(0)
     if j ne -1 then d(j)=0
   endfor
endif
dd=where(d eq 1)
if dd(0) ne -1 then ufiles=ufiles(dd)
if dd(0) eq -1 then stop ; !!!
endif

here:
; now make 'filenames' the list of UN-processed EIT/EUVI/XRT files.
filenames=ufiles
nfiles=n_elements(filenames)


return
end

;-----------------------------------------------------------------------------------
pro process_data_eit, base_dir, data_dir,filenames, nfiles,medfilt,dspk

common parameters,binfactor,normalization,rescaling,rotating,deconvolution

; set graph stuff
goto,skipgraphs
device, retain     = 2          
device, true_color = 24
device, decomposed = 0
window,1,ys=512,xs=512
skipgraphs:

print,'processing files...'

openw,1,data_dir+'list_of_files.out',/append
printf,1,'Number of files:',nfiles
printf,1,'Filename                   Status/Reason'
print,   'Number of files:',nfiles
print,   'Filename                   Status/Reason'

openw,22,data_dir+'snow.txt',/append

for i=0,nfiles-1 do begin
print,filenames(i)
read_eit, data_dir+filenames(i), index, data
flag=0 ; to accept file flag must be zero

;goto,skip_flag_assignement
if (size(index.object))(0) eq 1 then begin
 flag=10  ; it is a "multiple file"
 goto,skip_flag_assignement
endif

if index.OBJECT    ne 'full FOV' then flag=1
;if index.FILTER   ne 'Clear'    then flag=2
;if index.WAVELNTH  eq 304        then flag=3
if index.NAXIS1 ne index.NAXIS2  then flag=4
if (flag eq 0) AND ((index.NAXIS1 ne 512) AND (index.NAXIS1 ne 1024)) then flag=5
skip_flag_assignement:

; Missing blocks
if flag eq 0 AND (index.nmissb)(0) ne 0 then begin
   pmb=where(data eq 0)
   npmb  = float(n_elements(pmb))
   ndata = float(index.NAXIS1) * float(index.NAXIS2)
   if npmb/ndata ge 0.75 then flag=6
endif

if flag eq 0 then begin
;-----------------------------------------
 if dspk eq 0 then eit_prep, data_dir+filenames(i) , hdr , image 
 if dspk eq 1 then eit_prep, data_dir+filenames(i) , hdr , image , /cosmic 
;-----------------------------------------

;compare with correctly oriented RAW image:
rawimage=data
if abs(index.sc_roll) gt 170. then rawimage=reverse(reverse(data,1),2)   
;if index.sc_roll ne 0. AND index.sc_roll ne 180. then STOP

;Set missingblocks to "hugenegnum".
hugenegnum=-1.e10
pmb=where(rawimage eq 0)
if pmb(0) ne -1 then image(pmb)=hugenegnum

if medfilt eq 1 then begin
  stop
  image=median(image,3)
endif

;print,hdr.sc_roll
;Rotate image: North up.
if rotating eq 1 then begin
 ANGLE=-hdr.sc_roll
 rimage=rot(image,ANGLE,1,hdr.crpix1-1,hdr.crpix2-1,/pivot,missing=hugenegnum)
 hdr.sc_roll=0.
 image=rimage
 rotstring='rotat'
endif
if rotating eq 0 then rotstring='norot'
;print,hdr.sc_roll

if hdr.filter eq 'Clear' then filterstring='clear'
if hdr.filter eq 'Al +1' then filterstring='al1'
if hdr.filter eq 'Al +2' then filterstring='al2'
if hdr.filter ne 'Clear' AND hdr.filter ne 'Al +1' AND hdr.filter ne 'Al +2' then begin
     print, '---> Unrecognized filter!'
     STOP
endif
filterstring='normto-clear'

newfilename=strmid(filenames(i),0,18)+'.'+strmid(string(fix(index.WAVELNTH)),5,3)+'.'+filterstring+'.'+rotstring

;Correct for EIT's LEB software incorrect treatment of 4x4 binned images:
if index.NAXIS1 eq 512 then image=image/4.

if index.NAXIS1 eq 1024 AND binfactor gt 1 then begin
binned_image=rebin(image,index.NAXIS1/binfactor,index.NAXIS1/binfactor)
image=binned_image
hdr.SOLAR_R=hdr.SOLAR_R/float(binfactor)
; C' = 1 + (C-1) * (N/B-1)/(N-1).
hdr.CRPIX1 = 1 + (hdr.CRPIX1 -1) * (hdr.NAXIS1/binfactor - 1) / (hdr.NAXIS1-1)
hdr.CRPIX2 = 1 + (hdr.CRPIX2 -1) * (hdr.NAXIS2/binfactor - 1) / (hdr.NAXIS2-1)
hdr.NAXIS1=hdr.NAXIS1/binfactor
hdr.NAXIS2=hdr.NAXIS2/binfactor
hdr.CDELT1=hdr.CDELT1*float(binfactor)
hdr.CDELT2=hdr.CDELT2*float(binfactor)
endif

if hdr.NAXIS1 ge 1000 then newfilename=newfilename+'.'+strmid(hdr.naxis1,4,4)
if hdr.NAXIS1 lt 1000 then newfilename=newfilename+'.'+strmid(hdr.naxis1,5,3)

if medfilt eq 1 then newfilename=newfilename+'.MEDFILT'
if dspk    eq 1 then newfilename=newfilename+'.DESPIKE'

;Numbers below threshold are missing block or missing block stpread,
;make them -999.
threshold=hugenegnum/1.e2
p=where(image lt threshold)
if p(0) ne -1 then image(p)=-999.

goto,skipsnow
snow_detect,image,out,estado
printf,22,filenames(i),string(out),'  ',estado
print,filenames(i),string(out),'  ',estado
if estado eq 'reject' then begin
 flag=7
 goto,snow
endif
skipsnow:

if normalization eq 0 then begin
 newfilename=newfilename+'.NoNormal'
endif
if normalization eq 1 then begin
 divide_by_ck0,image,hdr,newimage,/eit
 image=newimage
 newfilename=newfilename+'.Norm-Ck0'
endif

printf,1,filenames(i),'  accepted, WV:'+string(index.WAVELNTH),index.NAXIS1,index.CRPIX1,index.CRPIX2,' ',index.OBJECT,' ',index.FILTER
print   ,filenames(i),'  accepted, WV:'+string(index.WAVELNTH),index.NAXIS1,index.CRPIX1,index.CRPIX2,' ',index.OBJECT,' ',index.FILTER

newfilename=newfilename+'.fts'
hdr.filename=newfilename

; make sure ALL non positive pixels are -999.
p=where(image le 0.)
if p(0) ne -1 then image(p)=-999.

goto,skipgraphs2
monitor_image=image
monitor_image=rebin(image,512,512)>.01<2000.
monitor_image(0,0)=2000.
monitor_image(0,0)=0.01
monitor_image=monitor_image
eit_colors,index.wavelnth
tvscl,alog10(monitor_image);,fix((index.wavelnth+20)/100)-1
skipgraphs2:

filename=data_dir+newfilename
MWRITEFITS, hdr,image, outfile=filename 

goto,skipgraphs3
record_gif,data_dir,newfilename+'.gif'
skipgraphs3:

endif else begin
 if flag eq  1 then reason='  NOT full-FOV'
 if flag eq  2 then reason='  NOT Clear filter'
 if flag eq  3 then reason='  It`s a 304'
 if flag eq  4 then reason='  NOT square'
 if flag eq  5 then reason='  Square, but NOT 1024^2 or 512^2'
 if flag eq  6 then reason='  Square, 512^2 or 1024^2, but 75% or more missing blocks'
 if flag eq 10 then reason='  It is a multiple file'
 snow:
 if flag eq 7 then reason='  SNOW with RATIO='+string(out)

printf,1,filenames(i),reason
print   ,filenames(i),reason
endelse

endfor

close,1
close,22
print,'done processing files!'

return
end

;-----------------------------------------------------------------------------------
pro process_data_euvi, base_dir, data_dir, out_dir, filenames, nfiles, medfilt,dspk,PHOTONS=PHOTONS,DNSEC=DNSEC,DECON=DECON

common parameters,binfactor,normalization,rescaling,rotating,deconvolution

print,'processing files...'

if keyword_set(PHOTONS) and dspk eq 1 then begin
 openw, 1,data_dir+'list_of_files_PHOTONS.DESPIKE.out',/append
 openw,22,data_dir+'snow_PHOTONS.txt'         ,/append
endif

if keyword_set(DNSEC) and dspk eq 1 then begin
 openw, 1,data_dir+'list_of_files_DNSEC.DESPIKE.out',/append
 openw,22,data_dir+'snow_DNSEC.txt'         ,/append
endif

if keyword_set(PHOTONS) and medfilt eq 1 then begin
 openw, 1,data_dir+'list_of_files_PHOTONS.MEDFILT.out',/append
 openw,22,data_dir+'snow_PHOTONS.txt'         ,/append
endif

if keyword_set(DNSEC) and medfilt eq 1 then begin
 openw, 1,data_dir+'list_of_files_DNSEC.MEDFILT.out',/append
 openw,22,data_dir+'snow_DNSEC.txt'         ,/append
endif

if keyword_set(DNSEC) and dspk eq 0 and medfilt eq 0 then begin
 openw, 1,data_dir+'list_of_files_DNSEC.out',/append
 openw,22,data_dir+'snow_DNSEC.txt'         ,/append
endif

if keyword_set(PHOTONS) and dspk eq 0 and medfilt eq 0 then begin
 openw, 1,data_dir+'list_of_files_PHOTONS.out',/append
 openw,22,data_dir+'snow_PHOTONS.txt'         ,/append
endif

printf,1,'Number of files:',nfiles
printf,1,'Filename                   Status/Reason'

flag_psf = 0

for i=0,nfiles-1 do begin
;if i eq 3 then stop
if keyword_set(DNSEC)   then begin
; This one is to get an IMAGE  where each pixel is in [DN/DEC].
; REMOVE /NORMAL_OFF if scale up to OPEN is desired here (only approximately, see my notes)
 if dspk eq 0 then begin
   secchi_prep,data_dir+filenames(i),hdr,image,/DN2P_OFF,/CAILMG_OFF,/NORMAL_OFF
 endif
 if dspk eq 1 then begin
   secchi_prep,data_dir+filenames(i),hdr,image,/DN2P_OFF,/CALIMG_OFF,/EXPTIME_OFF,/NORMAL_OFF
   tnv=8
   if hdr.wavelnth eq 284 then tnv=4
   ;STOP ; UNCOMMENT NEXT TWO LINES!
   for ii=1,3 do image=despike_gen(image,tn=tnv,/low3)
   image=euvi_correction(image,hdr,/DN2P_OFF,/CALIMG_OFF,/NORMAL_OFF);exposure normalize here
 endif
endif

if keyword_set(PHOTONS) then begin
; This one is to get an IMAGE2 where each pixel is in [PHOTONS]
 if dspk eq 0 then begin
   secchi_prep,data_dir+filenames(i),hdr,image,/EXPTIME_OFF,/CALIMG_OFF,/NORMAL_OFF
 endif
 if dspk eq 1 then begin
   secchi_prep,data_dir+filenames(i),hdr,image,/DN2P_OFF,/CALIMG_OFF,/NORMAL_OFF,/EXPTIME_OFF
   tnv=8
   if hdr.wavelnth eq 284 then tnv=4
   ;STOP ; UNCOMMENT NEXT TWO LINES!
   for ii=1,3 do image=despike_gen(image,tn=tnv,/low3)
   image=euvi_correction(image,hdr,/EXPTIME_OFF,/CALIMG_OFF,/NORMAL_OFF);change to PHOTONS here
 endif
endif

flag=0 ; to accept file flag must be zero

if (size(hdr.object))(0) eq 1 then begin
flag=10  ; it is a "multiple file" ; this used to work for EIT, will it work with EUVI?
STOP ; SEE IF IT WORKED!
;goto,skip_flag_assignement2
endif
if (hdr.filter ne 'OPEN') AND (hdr.filter ne 'S1') then flag=3
;goto,skip_flag_assignement2
if hdr.NAXIS1 ne hdr.NAXIS2 then flag=4
if (flag eq 0) AND (hdr.NAXIS1 ne 2048) then flag=5
skip_flag_assignement2:

; Create a fake missing block to test things.
; image(800:900,800:900)=hdr.blank
; hdr.nmissing=1   

; Missing blocks
imissing = where(image eq hdr.blank or finite(image) eq 0)
if flag eq 0 AND (hdr.nmissing ne 0 OR imissing(0) ne -1) then begin
   ;pmb1=where(image eq hdr.blank)
   ;pmb2=where(finite(image) eq 0)
   npmb  = float(n_elements(imissing))
   ndata = float(hdr.NAXIS1) * float(hdr.NAXIS2)
   if npmb/ndata ge 0.75 then flag=6
endif

if flag eq 0 then begin

stop
; image     = image - hdr.bzero
; hdr.bzero = 0.

if deconvolution eq 1 then begin
if flag_psf eq 0 then begin
 flag_psf = 1
 if hdr.obsrvtry eq 'STEREO_B' then begin
    if hdr.wavelnth eq 171 then psf_file = 'psf171B'
    if hdr.wavelnth eq 195 then psf_file = 'psf195B'
    if hdr.wavelnth eq 284 then psf_file = 'psf284B'
    if hdr.wavelnth eq 304 then psf_file = 'psf304B'
 endif
 if hdr.obsrvtry eq 'STEREO_A' then begin
    if hdr.wavelnth eq 171 then psf_file = 'psf171A'
    if hdr.wavelnth eq 195 then psf_file = 'psf195A'
    if hdr.wavelnth eq 284 then psf_file = 'psf284A'
    if hdr.wavelnth eq 304 then psf_file = 'psf304A'
 endif
 mreadfits,'/data1/work/psfs/'+psf_file+'.fts',psf_header,psf
 print,'leyó'
endif
image = euvi_decon( image , psf )
endif

 ;Set missingblocks to "hugenegnum".
 hugenegnum=-1.e20
 if imissing(0) ne -1 then image(imissing)=hugenegnum
 if medfilt eq 1 then image=median(image,3)
 ;Rotate image: North up. Note that we correct the disk center from
 ;FITS to IDL convention.
 if rotating eq 1 then begin
 ANGLE     = -hdr.crota
 rimage    = rot(image,ANGLE,1,hdr.crpix1-1,hdr.crpix2-1,/pivot,missing=hugenegnum)
 hdr.crota = 0.
 image     = rimage
 rotstring='rotat'
 endif
if rotating eq 0 then rotstring='norot'

newfilename=strmid(filenames(i),0,22)+hdr.filter+'.'+strmid(string(fix(hdr.WAVELNTH)),5,3)+'.'+rotstring

if deconvolution eq 1 then newfilename = newfilename+'.decon'

if hdr.NAXIS1 eq 2048 then begin
binned_image=rebin(image,hdr.NAXIS1/binfactor,hdr.NAXIS2/binfactor)
image=binned_image
; C' = 1 + (C-1) * (N/B-1)/(N-1).
hdr.CRPIX1 = 1 + (hdr.CRPIX1 -1) * (hdr.NAXIS1/binfactor - 1) / (hdr.NAXIS1-1)
hdr.CRPIX2 = 1 + (hdr.CRPIX2 -1) * (hdr.NAXIS2/binfactor - 1) / (hdr.NAXIS2-1)
hdr.CRPIX1A= 1 + (hdr.CRPIX1A-1) * (hdr.NAXIS1/binfactor - 1) / (hdr.NAXIS1-1)
hdr.CRPIX2A= 1 + (hdr.CRPIX2A-1) * (hdr.NAXIS2/binfactor - 1) / (hdr.NAXIS2-1)
hdr.NAXIS1 = hdr.NAXIS1 /binfactor
hdr.NAXIS2 = hdr.NAXIS2 /binfactor
hdr.CDELT1 = hdr.CDELT1 *binfactor
hdr.CDELT2 = hdr.CDELT2 *binfactor
hdr.CDELT1A= hdr.CDELT1A*binfactor
hdr.CDELT2A= hdr.CDELT2A*binfactor
if hdr.NAXIS1 ge 1000 then newfilename=newfilename+'.'+strmid(hdr.naxis1,4,4)
if hdr.NAXIS1 lt 1000 then newfilename=newfilename+'.'+strmid(hdr.naxis1,5,3)
endif

if keyword_set(DNSEC)   then newfilename=newfilename+'.DNSEC'
if keyword_set(PHOTONS) then newfilename=newfilename+'.PHOTO'

if medfilt eq 1 then newfilename=newfilename+'.MEDFILT'
if dspk    eq 1 then newfilename=newfilename+'.DESPIKE-tn'+strmid(string(tnv),7,1)

;--------------------------------------------------------------------
; Make missing data -999.
; Numbers below threshold are missing block or missing block spread
; after rotation or binning, make them -999.
threshold=hugenegnum/1.e5
p=where(image lt threshold)
if p(0) ne -1 then image(p)=-999.
;--------------------------------------------------------------------

if normalization eq 0 then begin
 newfilename=newfilename+'.NoNormal'
endif
if normalization eq 1 then begin
 divide_by_ck0,image,hdr,newimage,/euvi
 image=newimage
 newfilename=newfilename+'.Norm-Ck0'
endif

if rescaling eq 1 then begin
 rescale,image,hdr,newimage,/euvi
 image=newimage
 newfilename=newfilename+'.rs'
endif

newfilename=newfilename+'.fts'
hdr.filename=newfilename

printf,1,newfilename,hdr.CRPIX1,hdr.CRPIX2,' COMP:',hdr.COMPRSSN
print   ,newfilename,hdr.CRPIX1,hdr.CRPIX2,' COMP:',hdr.COMPRSSN

goto,graficos
 if i eq 0 then begin
  device, retain     = 2
  device, true_color = 24
  device, decomposed = 0
  window,0,ys=256*2,xs=256*4
 endif
 monitor_image=image
 monitor_image=rebin(image,256,256)
 pmb=where(monitor_image eq -999.)
 plate=monitor_image
 if hdr.WAVELNTH eq 171 then pos0=0
 if hdr.WAVELNTH eq 195 then pos0=1
 if hdr.WAVELNTH eq 284 then pos0=2
 if hdr.WAVELNTH eq 304 then pos0=3
 if hdr.obsrvtry eq 'STEREO_B' then pos0=pos0+4
 if keyword_set(PHOTONS) then bot=.1
 if keyword_set(DNSEC)   then bot=.01
 top=1.*max(plate)
 plate=plate>bot<top
 plate(0,0)=top
 SECCHI_COLORS, 'EUVI', hdr.wavelnth, R, G, B,/load
 tvscl,alog10(plate),pos0
graficos:

;-----------------------------------------------------------
; This section was originally requesteded by Rich, 
; later on cancelled by Paul's request for decon purposes.
; Make sure ALL negative pixels are -999.
; p=where(image lt 0.)
; if p(0) ne -1 then image(p)=-999.
;-----------------------------------------------------------

filename=out_dir+newfilename

MWRITEFITS, hdr,image, outfile=filename;'.crap'


endif else begin                ; if flag is non zero do this
;if flag eq  1 then reason='  NOT full-FOV'
;if flag eq  2 then reason='  NOT Clear filter'
 if flag eq  3 then reason='  Filter not (OPEN,S1)'
 if flag eq  4 then reason='  NOT square'
 if flag eq  5 then reason='  Square, but NOT 2048^2'
 if flag eq  6 then reason='  Square, 2048^2, but 75% or more missing blocks'
;if flag eq 10 then reason='  It is a multiple file'
 snow2:
;if flag eq 7 then reason='  SNOW with RATIO='+string(out)

printf,1,filenames(i),reason
print   ,filenames(i),reason
endelse

endfor ; next image

close,1
close,22
print,'done processing files!'

return
end

;-----------------------------------------------------------------------------------
pro process_data_xrt, base_dir, data_dir, filenames, nfiles, medfilt

; set graph stuff
device, retain     = 2
device, true_color = 24
device, decomposed = 0
window,0,ys=256,xs=768
;window,0,ys=512,xs=512

print,'processing files...'

openw,1,data_dir+'list_of_files.out',/append
printf,1,'Number of files:',nfiles
printf,1,'Filename                   Status/Reason'

openw,22,data_dir+'snow.txt',/append

for i=0,nfiles-1 do begin

read_xrt,data_dir+filenames(i),index,data
xrt_prep,index,data,hdr,image

flag=0 ; to accept file flag must be zero
;read_eit, data_dir+filenames(i), index, data
;goto,skip_flag_assignement
;if (size(hdr.object))(0) eq 1 then begin
;flag=10  ; it is a "multiple file" ; this used to work for EIT, will it work with EUVI?
;goto,skip_flag_assignement2
;endif
;if index.OBJECT   ne 'full FOV' then flag=1
;if index.FILTER   ne 'Clear'    then flag=2
;if hdr.WAVELNTH eq 304      then flag=3
if hdr.NAXIS1 ne hdr.NAXIS2 then flag=4
if (flag eq 0) AND (hdr.NAXIS1 ne 2048) then flag=5
skip_flag_assignement2:

; Create a fake missing block to test things.
; image(800:900,800:900)=hdr.blank
; hdr.nmissing=1   

; Missing blocks
;if flag eq 0 AND hdr.nmissing ne 0 then begin
;   pmb1=where(image eq hdr.blank)
;   pmb2=where(finite(image) eq 0)
;   npmb  = float(n_elements(pmb))
;   ndata = float(hdr.NAXIS1) * float(hdr.NAXIS2)
;   if npmb/ndata ge 0.75 then flag=6
;endif

if flag eq 0 then begin
;Set missingblocks to "hugenegnum".
;hugenegnum=-1.e10
;if hdr.nmissing ne 0 then begin
;if pmb1(0) ne -1 then image(pmb1)=hugenegnum
;if pmb2(0) ne -1 then image(pmb2)=hugenegnum
;endif

if medfilt eq 1 then image=median(image,3)

;Rotate image: North up.
;ANGLE=-hdr.crota
;rimage=rot(image,ANGLE,1,hdr.crpix1,hdr.crpix2,/pivot,missing=hugenegnum)
;hdr.crota=0.
;image=rimage

if hdr.NAXIS1 eq 2048 then begin
binned_image=rebin(image,512,512)
image=binned_image
hdr.CRPIX1 =hdr.CRPIX1 /4.
hdr.CRPIX2 =hdr.CRPIX2 /4.
;hdr.CRPIX1A=hdr.CRPIX1A/4.
;hdr.CRPIX2A=hdr.CRPIX2A/4.
hdr.NAXIS1=hdr.NAXIS1/4
hdr.NAXIS2=hdr.NAXIS2/4
hdr.CDELT1=hdr.CDELT1*4.
hdr.CDELT2=hdr.CDELT2*4.
;hdr.CDELT1A=hdr.CDELT1A*4.
;hdr.CDELT2A=hdr.CDELT2A*4.
endif

if hdr.EC_FW1 eq 0 then flt1str='0'
if hdr.EC_FW1 eq 1 then flt1str='1'
if hdr.EC_FW1 eq 2 then flt1str='2'
if hdr.EC_FW1 eq 3 then flt1str='3'
if hdr.EC_FW1 eq 4 then flt1str='4'
if hdr.EC_FW1 eq 5 then flt1str='5'

if hdr.EC_FW2 eq 0 then flt2str='0'
if hdr.EC_FW2 eq 1 then flt2str='1'
if hdr.EC_FW2 eq 2 then flt2str='2'
if hdr.EC_FW2 eq 3 then flt2str='3'
if hdr.EC_FW2 eq 4 then flt2str='4'
if hdr.EC_FW2 eq 5 then flt2str='5'

newfilename=strmid(filenames(i),0,21)+'fw'+flt1str+flt2str+'.fits'
;hdr.filename=newfilename

;Numbers below threshold are missing block or missing block stpread,
;make them -999.
;threshold=hugenegnum/1.e2
;p=where(image lt threshold)
;if p(0) ne -1 then image(p)=-999.

goto,skip_snow_detection
snow_detect,image,out,estado
printf,22,filenames(i),string(out),'  ',estado
if estado eq 'reject' then begin
 flag=7
 goto,snow2
endif
skip_snow_detection:

;divide_by_ck0,image,hdr,newimage,/euvi
;image=newimage

;printf,1,filenames(i),'  accepted, WV:'+string(hdr.WAVELNTH),hdr.NAXIS1,hdr.CRPIX1,hdr.CRPIX2,' ',hdr.OBJECT,' ',hdr.FILTER
;print   ,filenames(i),'  accepted, WV:'+string(hdr.WAVELNTH),hdr.NAXIS1,hdr.CRPIX1,hdr.CRPIX2,' ',hdr.OBJECT,' ',hdr.FILTER

monitor_image=rebin(image,128,128)
;monitor_image=rebin(image,512,512)
pmb=where(monitor_image eq -999.)
monitor_image=monitor_image>1.

eit_colors,171
if hdr.EC_FW1 ne 0 and hdr.EC_FW1 ne 0 then STOP ;!!
                        pos0=0
if hdr.EC_FW1 eq 0 then pos0=6
tvscl,alog10(monitor_image),hdr.EC_FW1+hdr.EC_FW2+pos0
;Stop
filename=data_dir+newfilename
MWRITEFITS, hdr,image, outfile=filename 


endif else begin ; if flaf is non zero do this
;if flag eq  1 then reason='  NOT full-FOV'
;if flag eq  2 then reason='  NOT Clear filter'
 if flag eq  3 then reason='  It`s a 304'
 if flag eq  4 then reason='  NOT square'
 if flag eq  5 then reason='  Square, but NOT 2048^2'
 if flag eq  6 then reason='  Square, 2048^2, but 75% or more missing blocks'
;if flag eq 10 then reason='  It is a multiple file'
 snow2:
;if flag eq 7 then reason='  SNOW with RATIO='+string(out)


printf,1,filenames(i),reason
print   ,filenames(i),reason
endelse

endfor ; next image

close,1
close,22
print,'done processing files!'

return
end

;-------------------------------------------------------------------------------------
pro timebinning, basedir, data_dir, filename, ntb,eit=eit,euvi=euvi,xrt=xrt

dT = 3600.*24./ntb ; time bin size in secs.

basedir='/usr/local/ssw/packages/chianti/'
!path = '+'+basedir+'idl:'+!path
!path=expand_path(!path)
;use_chianti,basedir+'dbase/'

;filenames=findfile(data_dir)

nf=0
x=''
openr,1,data_dir+filename
readf,1,nf
filenames=strarr(nf)
for i=0,nf-1 do begin
readf,1,x
filenames(i)=x
endfor
close,1

help,filenames

if n_elements(filenames) eq 0 then begin
 print,'NO FILES TO READ!!!'
 stop
endif

goto,skipthis

; ELIMINATE FILES THAT ARE NOT EIT (OR EUVI) FILES
d=intarr(n_elements(filenames))
if keyword_set(eit) then begin
for i=0,n_elements(filenames)-1 do begin
  if (strmid(filenames(i),0,3) eq 'efz') and (strlen(filenames(i)) eq 26) then d(i)=1
endfor
p=where(d eq 1)
if p(0) eq -1 then begin
print,'THERE ARE NO PRE-PROCESSED EIT FILES HERE!!!'
stop
endif
endif
if keyword_set(euvi) then begin
for i=0,n_elements(filenames)-1 do begin
  if (strmid(filenames(i),18,2) eq 'eu') then d(i)=1;and (strlen(filenames(i)) eq 47) then d(i)=1
endfor
p=where(d eq 1)
if p(0) eq -1 then begin
print,'THERE ARE NO PRE-PROCESSED EUVI FILES HERE!!!'
stop
endif
endif

filenames=filenames(p)
skipthis:
help,filenames
stop
if keyword_set(eit) then begin
p171A=where(strmid(filenames,19,3) eq '171')
p195A=where(strmid(filenames,19,3) eq '195')
p284A=where(strmid(filenames,19,3) eq '284')
if p171A(0) ne -1 then filenames171A=filenames(p171A)
if p195A(0) ne -1 then filenames195A=filenames(p195A)
if p284A(0) ne -1 then filenames284A=filenames(p284A)
endif
if keyword_set(euvi) then begin
p171A=where(strmid(filenames,25,3) eq '171' AND strmid(filenames,20,1) eq 'A')
p195A=where(strmid(filenames,25,3) eq '195' AND strmid(filenames,20,1) eq 'A')
p284A=where(strmid(filenames,25,3) eq '284' AND strmid(filenames,20,1) eq 'A')
p304A=where(strmid(filenames,25,3) eq '304' AND strmid(filenames,20,1) eq 'A')
p171B=where(strmid(filenames,25,3) eq '171' AND strmid(filenames,20,1) eq 'B')
p195B=where(strmid(filenames,25,3) eq '195' AND strmid(filenames,20,1) eq 'B')
p284B=where(strmid(filenames,25,3) eq '284' AND strmid(filenames,20,1) eq 'B')
p304B=where(strmid(filenames,25,3) eq '304' AND strmid(filenames,20,1) eq 'B')
if p171A(0) ne -1 then filenames171A=filenames(p171A)
if p195A(0) ne -1 then filenames195A=filenames(p195A)
if p284A(0) ne -1 then filenames284A=filenames(p284A)
if p304A(0) ne -1 then filenames304A=filenames(p304A)
if p171B(0) ne -1 then filenames171B=filenames(p171B)
if p195B(0) ne -1 then filenames195B=filenames(p195B)
if p284B(0) ne -1 then filenames284B=filenames(p284B)
if p304B(0) ne -1 then filenames304B=filenames(p304B)
endif

nscft=1 ; SOHO
if keyword_set(euvi) then nscft=2 ; STEREO

for isp=0,nscft-1 do begin
 if keyword_set(eit)  then begin
 instname='EIT'
 p171=p171A & p195=p195A & p284=p284A
 if p171(0) ne -1 then filenames171=filenames171A
 if p195(0) ne -1 then filenames195=filenames195A
 if p284(0) ne -1 then filenames284=filenames284A
endif
 if keyword_set(euvi) then begin
 if isp eq 0 then begin
 instname='EUVIA'
 p171=p171A & p195=p195A & p284=p284A& p304=p304A
 if p171(0) ne -1 then filenames171=filenames171A
 if p195(0) ne -1 then filenames195=filenames195A
 if p284(0) ne -1 then filenames284=filenames284A
 if p304(0) ne -1 then filenames304=filenames304A
 endif
 if isp eq 1 then begin
 instname='EUVIB'
 p171=p171B & p195=p195B & p284=p284B & p304=p304B
 if p171(0) ne -1 then filenames171=filenames171B
 if p195(0) ne -1 then filenames195=filenames195B
 if p284(0) ne -1 then filenames284=filenames284B
 if p304(0) ne -1 then filenames304=filenames304B
 endif
 endif
for ipb=0,3 do begin
 if ipb eq 0 then begin
 if p171(0) eq -1 then goto,nextpb
 nf=n_elements(filenames171)
 file=filenames171
 logfile='time_binning_logfile_'+instname+'_171.out'
 crpixfile='crpix-diff_logfile_'+instname+'_171.out'
 endif
 if ipb eq 1 then begin
 if p195(0) eq -1 then goto,nextpb
 nf=n_elements(filenames195)
 file=filenames195
 logfile='time_binning_logfile_'+instname+'_195.out'
 crpixfile='crpix-diff_logfile_'+instname+'_195.out'
 endif
 if ipb eq 2 then begin
 if p284(0) eq -1 then goto,nextpb
 nf=n_elements(filenames284)
 file=filenames284
 logfile='time_binning_logfile_'+instname+'_284.out'
 crpixfile='crpix-diff_logfile_'+instname+'_284.out'
 endif
 if ipb eq 3 then begin
 if p304(0) eq -1 then goto,nextpb
 nf=n_elements(filenames304)
 file=filenames304
 logfile='time_binning_logfile_'+instname+'_304.out'
 crpixfile='crpix-diff_logfile_'+instname+'_304.out'
 endif
 openw, 1,data_dir+logfile;,/append
 openw,11,data_dir+crpixfile

 i=0
 filename=data_dir+file(i)
 mreadfits,filename,hdr,image

if keyword_set(rotate) then begin
STOP
; If images are NOT rotated then ROTATE: 
hugenegnum=-1.e20
p=where(image eq -999.)
if p(0) ne -1 then image(p)=hugenegnum
;Rotate image: North up.
if hdr.crota2 ne 0. then begin
 ANGLE     = -hdr.crota2
 rimage    = rot(image,ANGLE,1,hdr.crpix1-1,hdr.crpix2-1,/pivot,missing=hugenegnum)
 hdr.crota2= 0.
 image     = rimage
endif
;Numbers below threshold are missing block or missing block spread,
;make them -999.
threshold=hugenegnum/1.e5
p=where(image lt threshold)
if p(0) ne -1 then image(p)=-999.
endif

makenewhdr,hdr,newhdr,instname

 if instname eq 'EIT' then begin
 read_eit, data_dir+strmid(file(i),0,18), index, data
 seconds=index.time/1000.
 bin=fix(seconds/dT)
 date=index.date
 timeobs=strmid(strmid(file(i),0,18),12,4)
 endif
 if instname eq 'EUVIA' or instname eq 'EUVIB' then begin
 dateobs=hdr.date_obs 
 hh=float(strmid(dateobs,11,2))
 mm=float(strmid(dateobs,14,2))
 ss=float(strmid(dateobs,17,6))
 seconds=hh*3600.+mm*60.+ss
 bin=fix(seconds/dT)
 date=strmid(hdr.date_obs,0,10)
 timeobs=strmid(dateobs,11,2)+strmid(dateobs,14,2)
 endif

 imsize=fix((size(image))(1))

NEWBIN:
  nib = 0 ; numer of images in current temporal-bin
  bincrpix1=0
  bincrpix2=0
  printf,1,'*********************** NEW BIN *****************************'
     print,'*********************** NEW BIN *****************************'
  printf,1,file(i),' ',seconds/3600./(24./ntb)
    print,file(i),' ',seconds/3600./(24./ntb)
  binimages = fltarr(imsize,imsize,1)
  timestrings = strarr(1)
  hdra=replicate(newhdr,1);replicate(hdr,1)
samebin:
  flag=''
  nib = nib + 1
  binimages(*,*,nib-1) = image
    timestrings(nib-1) = timeobs
           hdra(nib-1) = newhdr;hdr
  oldbin               = bin
  olddate              = date
  if i eq nf-1 then begin
    ilabel=i
    flag='out'
    goto,lastfile
  endif
  i=i+1
  filename=data_dir+file(i)
  mreadfits,filename,hdr,image

if keyword_set(rotate) then begin
STOP
; If images are NOT rotated then ROTATE: 
hugenegnum=-1.e20
p=where(image eq -999.)
if p(0) ne -1 then image(p)=hugenegnum
;Rotate image: North up.
if hdr.crota2 ne 0. then begin
 ANGLE     = -hdr.crota2
 rimage    = rot(image,ANGLE,1,hdr.crpix1-1,hdr.crpix2-1,/pivot,missing=hugenegnum)
 hdr.crota2= 0.
 image     = rimage
endif
;Numbers below threshold are missing block or missing block spread,
;make them -999.
threshold=hugenegnum/1.e5
p=where(image lt threshold)
if p(0) ne -1 then image(p)=-999.
endif
  
  makenewhdr,hdr,newhdr,instname
  if instname eq 'EIT' then begin
    read_eit, data_dir+strmid(file(i),0,18), index, data
    seconds=index.time/1000.
    bin=fix(seconds/dT)
    date=index.date
    timeobs=strmid(strmid(file(i),0,18),12,4)
  endif
  if instname eq 'EUVIA' or instname eq 'EUVIB' then begin
    dateobs=hdr.date_obs 
    hh=float(strmid(dateobs,11,2))
    mm=float(strmid(dateobs,14,2))
    ss=float(strmid(dateobs,17,6))
    seconds=hh*3600.+mm*60.+ss
    bin=fix(seconds/dT)
    date=strmid(hdr.date_obs,0,10)
    timeobs=strmid(dateobs,11,2)+strmid(dateobs,14,2)
  endif
  if (date eq olddate) AND (bin eq oldbin) then begin
     printf,1,file(i),' ',seconds/3600./(24./ntb)
        print,file(i),' ',seconds/3600./(24./ntb)
     temp  = fltarr(imsize,imsize,nib+1)
     temp2 = strarr(nib+1) 
     temp3 = replicate(newhdr,nib+1);replicate(hdr,nib+1)
     for im=0,nib-1 do temp(*,*,im)=binimages(*,*,im)
     binimages=temp
     for im=0,nib-1 do temp2(im)=timestrings(im)
     timestrings=temp2
     temp3(0:nib-1)=hdra
     hdra=temp3
     goto,samebin
  endif

 if (date ne olddate) OR (bin ne oldbin) then ilabel=i-1

lastfile:
  ;imprimir SUM y logfile
  if oldbin le 9 then strbin='0'+strmid(string(oldbin),7,1)
  if oldbin gt 9 then strbin=    strmid(string(oldbin),6,2)
  if oldbin gt ntb-1 then STOP ; !!!

  if instname eq 'EIT' then $
  filename=data_dir+strmid(string(file(ilabel)),0,12)+timestrings(nib/2)+'.'+$
                    strmid(string(file(ilabel)),19,3)+'.b.fts'

  if instname eq 'EUVIA' or instname eq 'EUVIB' then $
  filename=data_dir+strmid(string(file(ilabel)),0,8)+'.'+timestrings(nib/2)+'.'+$
           strmid(file(i-1),20,strlen(file(i-1))-24)+'.b'+strmid(string(ntb),7,1)+'.NEWHDR.fts'

  ; Take out a bad-centered OR miss-aligned images:
  printf,1,'Crpix1:',hdra.crpix1
  printf,1,'Crpix2:',hdra.crpix2
  print,1,' Crota:',hdra.crota
  if nib eq 1 then begin
  median_crpix1=hdra.crpix1
  median_crpix2=hdra.crpix2
  median_crota =hdra.crota
  endif
  if nib gt 1 then begin
  median_crpix1=median(hdra.crpix1)
  median_crpix2=median(hdra.crpix2)
  median_crota =median(hdra.crota)
  endif
  difcrpix1=abs(hdra.crpix1-median_crpix1)
  difcrpix2=abs(hdra.crpix2-median_crpix2)
  difcrota =abs(hdra.crota -median_crota )
                                ; difcrota is in DEGREES, by requesting it
                                ; to be < 2.e-2 we assure a
                                ; displacement of at most 0.2 pixels
                                ; at 1.5 Rsun, by trigonometry.
  iok=where(difcrpix1 le 0.1 AND difcrpix2 le 0.1 and difcrota le 0.02)
  if iok(0) eq -1 then begin
     printf,1,'No 2 images have same centering or alignment'
     iok=[nib/2]
  endif
; stop
  hdra      = hdra(iok)
  binimages = binimages(*,*,iok)
  nib       = fix(n_elements(iok))
  printf,1,'Selected images are indexed:',iok

  ;Get pixel by pixel averaged image of the bin
  meanimage=fltarr(imsize,imsize)
  for ix=0,imsize-1 do begin
  for iy=0,imsize-1 do begin
    nvp=0  ; number of valid data poins for the pixel
    sum=0. ; sum of the values of all valid data points for the pixel
    for im=0,nib-1 do begin
      ; Only use valid (ne -999) pixels for the binned image:
      if binimages(ix,iy,im) ne -999. then begin
         nvp=nvp+1
         sum=sum+binimages(ix,iy,im)
      endif
    endfor
    ; Set valid pixels equal to mean value of the nvp valid data points:
    if nvp gt 0 then meanimage(ix,iy)= sum/float(nvp)
    ; If there were not valid data points for the pixel then
    ; preserve it invalid (eq -999) in the binned image:
    if nvp eq 0 then meanimage(ix,iy)=-999.
  endfor
  endfor

  ;Create "median" header for bined file
    medianhdr=hdra(nib/2)

  ;Create "mean" header for bined file
    meanhdr=medianhdr ; equal to median, then average following quantities:
    meanhdr.crpix1  =mean(hdra.crpix1)
    meanhdr.crpix2  =mean(hdra.crpix2)
    meanhdr.cdelt1  =mean(hdra.cdelt1)
    meanhdr.cdelt2  =mean(hdra.cdelt2)
  if keyword_set(euvi) then begin

     printf,1,'CARR LONGS:',hdra.CRLN_OBS

     printf,1,'CROTA2:',hdra.crota

     printf,1,'CRPIX1:',hdra.crpix1

     printf,1,'CRPIX2:',hdra.crpix2

     printf,1,'compression:',hdra.COMPRSSN

    ;Change CARR_LONGS to [0,360]
    p=where(hdra.CRLN_OBS lt 0.)
    if p(0) ne -1 then begin
       correction=fltarr(nib) & correction(p)=360.
       hdra.CRLN_OBS=hdra.CRLN_OBS+correction
    endif
    ;Compute the average of LONGS properly
    if max(hdra.CRLN_OBS)-min(hdra.CRLN_OBS) gt 300. then begin
       printf,1,'Maxi diff of CRLN too large!'
       p=where(hdra.CRLN_OBS lt 50.)
       if p(0) ne -1 then begin
          correction=fltarr(nib) & correction(p)=360.
          hdra.CRLN_OBS=hdra.CRLN_OBS+correction 
          meanhdr.CRLN_OBS=mean(hdra.CRLN_OBS)
          if meanhdr.CRLN_OBS gt 360. then meanhdr.CRLN_OBS=meanhdr.CRLN_OBS-360.
       endif
       if p(0) eq -1 then STOP ; !!! Can't be.
    endif
    if max(hdra.CRLN_OBS)-min(hdra.CRLN_OBS) lt 50. then begin
       meanhdr.CRLN_OBS=mean(hdra.CRLN_OBS)
       if meanhdr.CRLN_OBS gt 360. then meanhdr.CRLN_OBS=meanhdr.CRLN_OBS-360.
    endif
    if max(hdra.CRLN_OBS)-min(hdra.CRLN_OBS) ge  50. and $
       max(hdra.CRLN_OBS)-min(hdra.CRLN_OBS) le 300. then STOP ; !!! Can't be.

    ;Change HG_LONGS to [0,360]
    p=where(hdra.HGLN_OBS lt 0.)
    if p(0) ne -1 then begin
       correction=fltarr(nib) & correction(p)=360.
       hdra.HGLN_OBS=hdra.HGLN_OBS+correction
    endif
    ;Compute the average of LONGS properly
    if max(hdra.HGLN_OBS)-min(hdra.HGLN_OBS) gt 300. then begin
       p=where(hdra.HGLN_OBS lt 50.)
       if p(0) ne -1 then begin
          correction=fltarr(nib) & correction(p)=360.
          hdra.HGLN_OBS=hdra.HGLN_OBS+correction 
          meanhdr.HGLN_OBS=mean(hdra.HGLN_OBS)
          if meanhdr.HGLN_OBS gt 360. then meanhdr.HGLN_OBS=meanhdr.HGLN_OBS-360.
       endif
       if p(0) eq -1 then STOP ; !!! Can't be.
    endif
    if max(hdra.HGLN_OBS)-min(hdra.HGLN_OBS) lt 50. then begin
       meanhdr.HGLN_OBS=mean(hdra.HGLN_OBS)
       if meanhdr.HGLN_OBS gt 360. then meanhdr.HGLN_OBS=meanhdr.HGLN_OBS-360.
    endif
    if max(hdra.HGLN_OBS)-min(hdra.HGLN_OBS) ge  50. and $
       max(hdra.HGLN_OBS)-min(hdra.HGLN_OBS) le 300. then STOP ; !!! Can't be.

    meanhdr.haex_obs=mean(hdra.haex_obs)
    meanhdr.haey_obs=mean(hdra.haey_obs)
    meanhdr.haez_obs=mean(hdra.haez_obs)
    meanhdr.hcix_obs=mean(hdra.hcix_obs)
    meanhdr.hciy_obs=mean(hdra.hciy_obs)
    meanhdr.hciz_obs=mean(hdra.hciz_obs)
    meanhdr.heex_obs=mean(hdra.heex_obs)
    meanhdr.heey_obs=mean(hdra.heey_obs)
    meanhdr.heez_obs=mean(hdra.heez_obs)
    meanhdr.heqx_obs=mean(hdra.heqx_obs)
    meanhdr.heqy_obs=mean(hdra.heqy_obs)
    meanhdr.heqz_obs=mean(hdra.heqz_obs)
    meanhdr.DSUN_OBS=mean(hdra.DSUN_OBS)
    meanhdr.CRLT_OBS=mean(hdra.CRLT_OBS)
    meanhdr.RSUN    =mean(hdra.RSUN)
    meanhdr.crota   =mean(hdra.crota)
  endif
  if keyword_set(eit) then begin
    meanhdr.hec_x=mean(hdra.hec_x)
    meanhdr.hec_y=mean(hdra.hec_y)
    meanhdr.hec_z=mean(hdra.hec_z)
    meanhdr.solar_R =mean(hdra.solar_R)
  endif

;Test pixels are either positive or -999.
  ;p=where(meanimage le 0. and meanimage ne -999.)
  ;if p(0) ne -1 then STOP

  MWRITEFITS,  meanhdr,meanimage, outfile=filename 

  if keyword_set(euvi) then $
  printf,1,'   AVERAGE CRLN_OBS:',meanhdr.CRLN_OBS

  printf,1,'-----'
  printf,1,filename
  printf,1,nib,' files'

if flag eq 'out' then goto,nextpb
if i le nf-1 then goto,newbin
nextpb:
close, 1 ; PB-logfile
close,11 ; CRPIX-dif file
endfor
endfor

return
end

;-------------------------------------------------------------------------------------
pro makemovie, data_dir, out_dir,time=time, filter=filter, step=step, save=save, $
               correctscale=correctscale,singlebp=singlebp,eit=eit,euvi=euvi,xrt=xrt,$
               photons=photons,dnsec=dnsec

;!path = '+'+basedir+'/dem/idl:'+!path
;!path=expand_path(!path)
;use_chianti,basedir+'/dem/dbase/'

filenames=findfile(data_dir)

if n_elements(filenames) eq 0 then begin
 print,'NO FILES TO READ!'
 stop
endif

;goto,skipthis
; FIRST ELIMINATE FILES THAT ARE NOT FTS FILES!
d=intarr(n_elements(filenames))
for i=0,n_elements(filenames)-1 do begin
  nstring=strlen(filenames(i))
  if (strmid(filenames(i),nstring-3,3) eq 'fts') then d(i)=1
endfor
p=where(d eq 1)
if p(0) eq -1 then begin
print,'THERE ARE NO FTS FILES HERE!'
stop
endif
filenames=filenames(p)
skipthis:

; set graph stuff
device, retain     = 2
device, true_color = 24
device, decomposed = 0

; assume movies by filter as default 
if keyword_set(singlebp) then goto,onebp
if keyword_set(eit)  then window,0,ys=256,xs=768
if keyword_set(euvi) then window,0,xs=1024,ys=512
if keyword_set(time)     then goto,bytime
nstring=strlen(filenames(0))

p171=where(strmid(filenames,19,3) eq '171')
p195=where(strmid(filenames,19,3) eq '195')
p284=where(strmid(filenames,19,3) eq '284')
p304=where(strmid(filenames,19,3) eq '304')

if p171(0) ne -1 then filenames171=filenames(p171)
if p195(0) ne -1 then filenames195=filenames(p195)
if p284(0) ne -1 then filenames284=filenames(p284)
if p304(0) ne -1 then filenames304=filenames(p304)
pb=[ 171, 195, 284, 304]

frame=-1
for ipb=0,3 do begin
 if ipb eq 0 then begin
 if p171(0) eq -1 then goto,nextpb
 nf=n_elements(filenames171)
 file=filenames171
 endif
 if ipb eq 1 then begin
 if p195(0) eq -1 then goto,nextpb
 nf=n_elements(filenames195)
 file=filenames195
 endif
 if ipb eq 2 then begin
 if p284(0) eq -1 then goto,nextpb
 nf=n_elements(filenames284)
 file=filenames284
 endif
 if ipb eq 3 then begin
 if p304(0) eq -1 then goto,nextpb
 nf=n_elements(filenames304)
 file=filenames304
 endif

if keyword_set(eit)  then $
eit_colors,pb(ipb)
if keyword_set(euvi) then $
SECCHI_COLORS, 'EUVI', pb(ipb), R, G, B,/load

for i=0,nf-1 do begin
 filename=data_dir+file(i)
 ;image = READFITS(filename)
 mreadfits,filename,hdr,image
 plate=rebin(image,512,512)
 if keyword_set(correctscale) then plate(0,0)=2000.
 if keyword_set(correctscale) then plate=plate<2001.
 pos0=0
 ;if keyword_set(euvi) and strmid(filenames(i),nstring-11,1) eq 'B' then pos0=3
 tvscl,alog10(plate>1),ipb+pos0
 print,i,' ',filename
 i=i+1
 wait,0.5
 if keyword_set(save) then begin
 frame=frame+1
 image24 = TVRD(True=1)
 image2d = Color_Quan(image24, 1, r, g, b)
 if frame ge   0 and frame le   9 then numberstring='00'+strmid(string(frame),7,1)
 if frame ge  10 and frame le  99 then numberstring= '0'+strmid(string(frame),6,2)
 if frame ge 100 and frame le 999 then numberstring=     strmid(string(frame),5,3)
 write_GIF,out_dir+'filtermovie_frame'+numberstring+'.gif', image2d, r, g, b 
 endif

 ;MPEG_PUT, mpegID, IMAGE=image2d, FRAME=i
 ;print,file(i)
 ;stop
endfor

nextpb:
endfor
goto,closemovie

bytime:
nf=n_elements(filenames)
nstring=strlen(filenames(0))
counter=0
frame  =0

counterA171=0
counterA195=0
counterA284=0
counterA304=0
counterB171=0
counterB195=0
counterB284=0
counterB304=0

for i=0,nf-1 do begin
 filename=data_dir+filenames(i)
 print,'*****',i,'  ',filename, '****'
 mreadfits,filename,hdr,image

hugenegnum=-1.e10
p=where(image eq -999.)
if p(0) ne -1 then image(p)=hugenegnum
goto,skiprot
;Rotate image: North up.
if hdr.crota2 ne 0. then begin
 ANGLE     = -hdr.crota2
 rimage    = rot(image,ANGLE,1,hdr.crpix1-1,hdr.crpix2-1,/pivot,missing=hugenegnum)
 hdr.crota2= 0.
 image     = rimage
endif
skiprot:
;Numbers below threshold are missing block or missing block spread,
;make them -999.
threshold=hugenegnum/1.e2
p=where(image lt threshold)
if p(0) ne -1 then image(p)=-999.

 if hdr.wavelnth eq 171 then begin
    pb=171
    pos0=0
    if keyword_set(dnsec) then begin
    bot=.002
    top=600.
    endif
    if keyword_set(euvi) then begin
      if hdr.OBSRVTRY eq 'STEREO_A' and counterA171 eq 0 then counterA171=1
      if hdr.OBSRVTRY eq 'STEREO_B' and counterB171 eq 0 then counterB171=1
    endif
 endif
 if hdr.wavelnth eq 195 then begin
    pb=195
    pos0=1
    if keyword_set(dnsec) then begin
    top=400.
    bot=.002
    top=200.
    endif
    if keyword_set(euvi) then begin
      if hdr.OBSRVTRY eq 'STEREO_A' and counterA195 eq 0 then counterA195=1
      if hdr.OBSRVTRY eq 'STEREO_B' and counterB195 eq 0 then counterB195=1
   endif
 endif
 if hdr.wavelnth eq 284 then begin
    pb=284
    pos0=2
    if keyword_set(dnsec) then begin
    bot=.001
    top=300.
    endif
    if keyword_set(euvi) then begin
      if hdr.OBSRVTRY eq 'STEREO_A' and counterA284 eq 0 then counterA284=1
      if hdr.OBSRVTRY eq 'STEREO_B' and counterB284 eq 0 then counterB284=1
   endif
 endif
 if hdr.wavelnth eq 304 then begin
    pb=304
    pos0=3
    if keyword_set(dnsec) then begin
    bot=.05
    top=1000
    endif
    if keyword_set(euvi) then begin
      if hdr.OBSRVTRY eq 'STEREO_A' and counterA304 eq 0 then counterA304=1
      if hdr.OBSRVTRY eq 'STEREO_B' and counterB304 eq 0 then counterB304=1
   endif
 endif

  counter=counterA171+counterB171+counterA195+counterB195+counterA284+counterB284+counterA304+counterB304

  if keyword_set(euvi) and hdr.OBSRVTRY eq 'STEREO_A' then pos0=pos0+4

  SECCHI_COLORS, 'EUVI', hdr.wavelnth, R, G, B,/load
  plate=rebin(image,256,256)

  if keyword_set(PHOTONS) then bot=.1
  ;if keyword_set(DNSEC)   then bot=.01
  ;top=1.*max(plate)

  if keyword_set(correctscale) then plate=plate>bot<top
   plate(0,0)=top
;  plate(0,1)=bot

;  if hdr.wavelnth eq 195 then begin
  tvscl,alog10(plate),pos0    
;  counter=counter+1
;  endif

 if keyword_set(save) and counter eq 8 then begin
;stop
 frame=frame+1
 image24 = TVRD(True=1)
 image2d = Color_Quan(image24, 1, r, g, b)
 if frame ge   0 and frame le   9 then numberstring='00'+strmid(string(frame),7,1)
 if frame ge  10 and frame le  99 then numberstring= '0'+strmid(string(frame),6,2)
 if frame ge 100 and frame le 999 then numberstring=     strmid(string(frame),5,3)
 write_GIF,out_dir+ 'timemovie_frame'+numberstring+'.gif', image2d, r, g, b 
 counterA171=0
 counterA195=0
 counterA284=0
 counterA304=0
 counterB171=0
 counterB195=0
 counterB284=0
 counterB304=0
 counter=0
 endif

 if keyword_set(step) then begin
 print,i,filenames(i)
 wait,1
 endif
endfor
goto,closemovie

onebp:
window,xs=512,ys=512
read,'bandpass? (171, 195, 284)',bandpass
if bandpass ne 171 and bandpass ne 195 and bandpass ne 284 then begin
print,'enter a valid passband!'
stop
end
nstring=strlen(filenames(0))
p171=where(strmid(filenames,nstring-9,3) eq '171')
p195=where(strmid(filenames,nstring-9,3) eq '195')
p284=where(strmid(filenames,nstring-9,3) eq '284')
if p171(0) ne -1 then filenames171=filenames(p171)
if p195(0) ne -1 then filenames195=filenames(p195)
if p284(0) ne -1 then filenames284=filenames(p284)
pb=bandpass
if pb eq 171 then filenames=filenames(p171)
if pb eq 195 then filenames=filenames(p195)
if pb eq 284 then filenames=filenames(p284)
nf=n_elements(filenames)
nstring=strlen(filenames(0))
for i=0,nf-1 do begin
 filename=data_dir+filenames(i)
 ;image = READFITS(filename)
 mreadfits,filename,hdr,image
 eit_colors,pb
 plate=image
 if keyword_set(correctscale) then plate(0,0)=6000.
 if keyword_set(correctscale) then plate=plate<6001.
 tvscl,alog10(plate>1)

 if keyword_set(save) then begin
 image24 = TVRD(True=1)
 image2d = Color_Quan(image24, 1, r, g, b)
 if i ge   0 and i le   9 then numberstring='00'+strmid(string(i),7,1)
 if i ge  10 and i le  99 then numberstring= '0'+strmid(string(i),6,2)
 if i ge 100 and i le 999 then numberstring=     strmid(string(i),5,3)
 write_GIF,filename+'.gif', image2d, r, g, b 
 endif
 if keyword_set(step) then begin
 print,i,filenames(i)
 wait,1
 endif
endfor

closemovie:

;MPEG_SAVE, mpegID, FILENAME='myMovie.mpg'
; Close the MPEG sequence:MPEG_CLOSE, mpegID
;MPEG_CLOSE, mpegID

return
end

;-------------------------------------------------------------------------------------
pro snow_detect, ima, out, estado

ima=rebin(ima,512,512)

east =reform(ima(  0: 65, 66:445))
west =reform(ima(446:511, 66:445))
north=reform(ima(      *,446:511))
south=reform(ima(      *,  0: 65))

width=5

ratio=total(abs(east -median(east ,width)))/abs(mean(median(east ,width)))+$
      total(abs(west -median(west ,width)))/abs(mean(median(west ,width)))+$
      total(abs(north-median(north,width)))/abs(mean(median(north,width)))+$
      total(abs(south-median(south,width)))/abs(mean(median(south,width)))

if ratio lt 15000. then estado='keep'
if ratio gt 45000. then estado='reject'
out=ratio

return
end

;--------------------------------------------------------------
pro makenewhdr,hdr,newhdr,instname

  newhdr=create_struct('crpix1',hdr.crpix1,$
                       'crpix2',hdr.crpix2,$
                       'naxis1',hdr.naxis1,$
                       'naxis2',hdr.naxis2,$
                       'cdelt1',hdr.cdelt1,$
                       'cdelt2',hdr.cdelt2,$
                       'wavelnth',hdr.wavelnth,$
                       'filter'  ,hdr.filter)

  if instname eq 'EUVIA' or instname eq 'EUVIB' then begin
    newhdr=create_struct(newhdr,$
        'COMPRSSN',hdr.COMPRSSN,$
        'crota'   ,hdr.crota2  ,$ ; for some reason this name changed in hdr
    	'haex_obs',hdr.haex_obs,$
    	'haey_obs',hdr.haey_obs,$
    	'haez_obs',hdr.haez_obs,$
    	'hcix_obs',hdr.hcix_obs,$
    	'hciy_obs',hdr.hciy_obs,$
    	'hciz_obs',hdr.hciz_obs,$
    	'heex_obs',hdr.heex_obs,$
    	'heey_obs',hdr.heey_obs,$
    	'heez_obs',hdr.heez_obs,$
    	'heqx_obs',hdr.heqx_obs,$
    	'heqy_obs',hdr.heqy_obs,$
    	'heqz_obs',hdr.heqz_obs,$
    	'DSUN_OBS',hdr.DSUN_OBS,$
    	'HGLN_OBS',hdr.HGLN_OBS,$
    	'CRLN_OBS',hdr.CRLN_OBS,$
    	'CRLT_OBS',hdr.CRLT_OBS,$
    	'RSUN'    ,hdr.RSUN    ,$
        'detector',hdr.detector,$
        'obsrvtry',hdr.obsrvtry,$
        'DATE_OBS',hdr.DATE_OBS)
  endif
  if instname eq 'EIT' then begin
    newhdr=create_struct(newhdr,$
        'sc_roll' ,hdr.sc_roll ,$
    	'hec_x'   ,hdr.hec_x   ,$
    	'hec_y'   ,hdr.hec_y   ,$
    	'hec_z'   ,hdr.hec_z   ,$
    	'solar_R' ,hdr.solar_R ,$
        'instrume',hdr.instrume,$
        'DATE_OBS',hdr.DATE_OBS)
  endif

return
end

;-------------------------------------------------------------------------------------
pro makemovie2, basedir, data_dir, time=time, filter=filter, step=step, save=save, $
               correctscale=correctscale,singlebp=singlebp,eit=eit,euvi=euvi

!path = '+'+basedir+'/dem/idl:'+!path
!path=expand_path(!path)
;use_chianti,basedir+'/dem/dbase/'

filenames=findfile(data_dir)

nf=0
x=''
openr,1,data_dir+'full.list.171.A'
readf,1,nf
filenames=strarr(nf)
for i=0,nf-1 do begin
readf,1,x
filenames(i)=x
endfor

if n_elements(filenames) eq 0 then begin
 print,'NO FILES TO READ!'
 stop
endif

goto, skipthis
; FIRST ELIMINATE FILES THAT ARE NOT BINNED FILES!
d=intarr(n_elements(filenames))
for i=0,n_elements(filenames)-1 do begin
  nstring=strlen(filenames(i))
  if (strmid(filenames(i),nstring-5,5) eq 'b.fts') then d(i)=1
endfor
p=where(d eq 1)
if p(0) eq -1 then begin
print,'THERE ARE NO BINNED FILES HERE!'
stop
endif
filenames=filenames(p)
skipthis:

; set graph stuff
device, retain     = 2
device, true_color = 24
device, decomposed = 0

; assume movies by filter as default 
if keyword_set(singlebp) then goto,onebp
if keyword_set(eit)  then window,0,ys=256,xs=768
if keyword_set(euvi) then window,1,ys=512,xs=768
if keyword_set(euvi) then window,1,ys=512,xs=512
if keyword_set(time)     then goto,bytime
nstring=strlen(filenames(0))

p171=where(strmid(filenames,nstring-9,3) eq '171')
p195=where(strmid(filenames,nstring-9,3) eq '195')
p284=where(strmid(filenames,nstring-9,3) eq '284')
p304=where(strmid(filenames,nstring-9,3) eq '304')

if p171(0) ne -1 then filenames171=filenames(p171)
if p195(0) ne -1 then filenames195=filenames(p195)
if p284(0) ne -1 then filenames284=filenames(p284)
if p304(0) ne -1 then filenames304=filenames(p304)
pb=[ 171, 195, 284, 304]

frame=-1
for ipb=0,3 do begin
 if ipb eq 0 then begin
 if p171(0) eq -1 then goto,nextpb
 nf=n_elements(filenames171)
 file=filenames171
 endif
 if ipb eq 1 then begin
 if p195(0) eq -1 then goto,nextpb
 nf=n_elements(filenames195)
 file=filenames195
 endif
 if ipb eq 2 then begin
 if p284(0) eq -1 then goto,nextpb
 nf=n_elements(filenames284)
 file=filenames284
 endif
 if ipb eq 3 then begin
 if p304(0) eq -1 then goto,nextpb
 nf=n_elements(filenames304)
 file=filenames304
 endif

if keyword_set(eit)  then $
eit_colors,pb(ipb)
if keyword_set(euvi) then $
SECCHI_COLORS, 'EUVI', pb(ipb), R, G, B,/load

for i=0,nf-1 do begin
 filename=data_dir+file(i)
 ;image = READFITS(filename)
 mreadfits,filename,hdr,image
 plate=rebin(image,256,256)
 if keyword_set(correctscale) then plate(0,0)=2000.
 if keyword_set(correctscale) then plate=plate<2001.
 pos0=0
 if keyword_set(euvi) and strmid(filenames(i),nstring-11,1) eq 'B' then pos0=3
 tvscl,alog10(plate>1),ipb+pos0
 print,i,' ',filename
 i=i+1
 wait,0.5
 if keyword_set(save) then begin
 frame=frame+1
 image24 = TVRD(True=1)
 image2d = Color_Quan(image24, 1, r, g, b)
 if frame ge   0 and frame le   9 then numberstring='00'+strmid(string(frame),7,1)
 if frame ge  10 and frame le  99 then numberstring= '0'+strmid(string(frame),6,2)
 if frame ge 100 and frame le 999 then numberstring=     strmid(string(frame),5,3)
 write_GIF,data_dir+'filtermovie_frame'+numberstring+'.gif', image2d, r, g, b 
 endif

 ;MPEG_PUT, mpegID, IMAGE=image2d, FRAME=i
 ;print,file(i)
 ;stop
endfor

nextpb:
endfor
goto,closemovie

bytime:
nf=n_elements(filenames)
nstring=strlen(filenames(0))
if keyword_set(euvi) then flags=fltarr(8)
if keyword_set(eit)  then flags=fltarr(3)
fn=1
record='no'
onlyB='yes'
print,'Is onlyB correctly set?: ',onlyB
stop
for i=0,nf-1 do begin
 filename=data_dir+filenames(i)
 print,'*****',i,'  ',filename, '****'
 ;image = READFITS(filename)
 mreadfits,filename,hdr,image

if hdr.wavelnth eq 171 then pb=171
if hdr.wavelnth eq 195 then pb=195
if hdr.wavelnth eq 284 then pb=284
if hdr.wavelnth eq 304 then pb=304

 if keyword_set(eit)  then eit_colors,pb
 if keyword_set(euvi) then $
 SECCHI_COLORS, 'EUVI', hdr.wavelnth, R, G, B,/load

loadct,33
 plate=rebin(image,512,512)

if keyword_set(correctscale) then begin
if hdr.wavelnth eq 171 then thr=200.;6000.
if hdr.wavelnth eq 195 then thr=300.;4000.
if hdr.wavelnth eq 284 then thr=200.;300.
if hdr.wavelnth eq 304 then thr=200.;300.
plate(0,0)=thr
plate=plate<thr+1
endif

 pos0=0
 if keyword_set(euvi) then $
 if hdr.obsrvtry eq 'STEREO_B' then pos0=3
 tvscl,alog10(plate>1);,fix((pb-72)/100)+pos0

 if keyword_set(euvi) then begin
 if hdr.wavelnth eq 171 and hdr.obsrvtry eq 'STEREO_A' then flags(0)=1.
 if hdr.wavelnth eq 195 and hdr.obsrvtry eq 'STEREO_A' then flags(1)=1.
 if hdr.wavelnth eq 284 and hdr.obsrvtry eq 'STEREO_A' then flags(2)=1.
 if hdr.wavelnth eq 304 and hdr.obsrvtry eq 'STEREO_A' then flags(3)=1.
 if hdr.wavelnth eq 171 and hdr.obsrvtry eq 'STEREO_B' then flags(4)=1.
 if hdr.wavelnth eq 195 and hdr.obsrvtry eq 'STEREO_B' then flags(5)=1.
 if hdr.wavelnth eq 284 and hdr.obsrvtry eq 'STEREO_B' then flags(6)=1.
 if hdr.wavelnth eq 304 and hdr.obsrvtry eq 'STEREO_B' then flags(7)=1.
 if total(flags(0:2)) ge 1. then onlyB='no'

 if total(flags) eq 8. then record='yes'
 if total(flags(4:7)) eq 4. AND onlyB eq 'yes' then record='yes' 
 record='yes'
;stop
 endif

 if keyword_set(eit) then begin
 if hdr.wavelnth eq 171 then flags(0)=1.
 if hdr.wavelnth eq 195 then flags(1)=1.
 if hdr.wavelnth eq 284 then flags(2)=1.
 if total(flags) eq 3. then record='yes'
 endif

 if keyword_set(save) and record eq 'yes' then begin
 image24 = TVRD(True=1)
 image2d = Color_Quan(image24, 1, r, g, b)
 if fn ge   0 and fn le   9 then numberstring='00'+strmid(string(fn),7,1)
 if fn ge  10 and fn le  99 then numberstring= '0'+strmid(string(fn),6,2)
 if fn ge 100 and fn le 999 then numberstring=     strmid(string(fn),5,3)
 write_GIF,data_dir+'timemovie_frame'+numberstring+'.gif', image2d, r, g, b 
 flags=flags*0.
 record='no'
 fn=fn+1
 endif

 if keyword_set(step) then begin
 print,i,filenames(i)
 wait,1
 endif
endfor
goto,closemovie

onebp:
window,xs=512,ys=512
read,'bandpass? (171, 195, 284)',bandpass
if bandpass ne 171 and bandpass ne 195 and bandpass ne 284 then begin
print,'enter a valid passband!'
stop
end
nstring=strlen(filenames(0))
p171=where(strmid(filenames,nstring-9,3) eq '171')
p195=where(strmid(filenames,nstring-9,3) eq '195')
p284=where(strmid(filenames,nstring-9,3) eq '284')
if p171(0) ne -1 then filenames171=filenames(p171)
if p195(0) ne -1 then filenames195=filenames(p195)
if p284(0) ne -1 then filenames284=filenames(p284)
pb=bandpass
if pb eq 171 then filenames=filenames(p171)
if pb eq 195 then filenames=filenames(p195)
if pb eq 284 then filenames=filenames(p284)
nf=n_elements(filenames)
nstring=strlen(filenames(0))
for i=0,nf-1 do begin
 filename=data_dir+filenames(i)
 ;image = READFITS(filename)
 mreadfits,filename,hdr,image
 eit_colors,pb
 plate=image
 if keyword_set(correctscale) then plate(0,0)=6000.
 if keyword_set(correctscale) then plate=plate<6001.
 tvscl,alog10(plate>1)

 if keyword_set(save) then begin
 image24 = TVRD(True=1)
 image2d = Color_Quan(image24, 1, r, g, b)
 if i ge   0 and i le   9 then numberstring='00'+strmid(string(i),7,1)
 if i ge  10 and i le  99 then numberstring= '0'+strmid(string(i),6,2)
 if i ge 100 and i le 999 then numberstring=     strmid(string(i),5,3)
 write_GIF,filename+'.gif', image2d, r, g, b 
 endif
 if keyword_set(step) then begin
 print,i,filenames(i)
 wait,1
 endif
endfor

closemovie:

;MPEG_SAVE, mpegID, FILENAME='myMovie.mpg'
; Close the MPEG sequence:MPEG_CLOSE, mpegID
;MPEG_CLOSE, mpegID

return
end

pro divide_by_ck0,image,hdr,newimage,eit=eit,euvi=euvi

  if keyword_set(eit) then begin 
;  if hdr.filter eq 'Clear' then begin
     if hdr.wavelnth eq 171 then PHI0=1.12713e-11
     if hdr.wavelnth eq 195 then PHI0=6.76171e-12
     if hdr.wavelnth eq 284 then PHI0=3.61825e-13
     if hdr.wavelnth eq 304 then PHI0=4.81284e-13
;  endif
; if hdr.filter eq 'Al +1' then begin
;    if hdr.wavelnth eq 171 then PHI0=5.78243e-12
;    if hdr.wavelnth eq 195 then PHI0=3.38957e-12
;    if hdr.wavelnth eq 284 then PHI0=1.32465e-13
;    if hdr.wavelnth eq 304 then PHI0=1.65909e-13
; endif
; if hdr.filter eq 'Al +2' then begin
;    if hdr.wavelnth eq 171 then PHI0=2.68898e-12
;    if hdr.wavelnth eq 195 then PHI0=1.50737e-12
;    if hdr.wavelnth eq 284 then PHI0=2.94981e-14
;    if hdr.wavelnth eq 304 then PHI0=3.03512e-14
; endif
; if hdr.filter ne 'Clear' AND hdr.filter ne 'Al +1' AND hdr.filter ne 'Al +2' then begin
;    print, '---> Unrecognized filter!'
;    STOP
; endif
endif

  if keyword_set(euvi) then begin
  Omega_p_A=5.92553e-11
  Omega_p_B=5.94216e-11
  if hdr.filter eq 'OPEN' then begin
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 171 then PHI0=2.8326392e-1 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 195 then PHI0=1.4195491e-1 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 284 then PHI0=2.8273739e-2 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 304 then PHI0=4.2421336e-2 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 171 then PHI0=2.4296904e-1 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 195 then PHI0=1.2297586e-1 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 284 then PHI0=2.4906123e-2 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 304 then PHI0=4.4347962e-2 * Omega_p_B
  endif
  if hdr.filter eq 'S1' then begin
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 171 then PHI0=1.3240548e-1 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 195 then PHI0=0.6640560e-1 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 284 then PHI0=0.8998714e-2 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_A' and hdr.wavelnth eq 304 then PHI0=1.2756461e-2 * Omega_p_A
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 171 then PHI0=1.1928132e-1 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 195 then PHI0=0.5919602e-1 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 284 then PHI0=0.8134195e-2 * Omega_p_B
    if hdr.obsrvtry eq 'STEREO_B' and hdr.wavelnth eq 304 then PHI0=1.3539108e-2 * Omega_p_B
  endif
  if hdr.filter ne 'OPEN' and hdr.filter ne 'S1' then begin
    print,'--! Filter position not identified, cant go on like this. !--'
    STOP
  endif
  endif

  Ck0=PHI0*1.e12
  newimage=image/Ck0
  ;Preserve "-999" pixels in newimage
  p=where(image eq -999.)
  if p(0) ne -1 then newimage(p)=-999.
  return
end

pro rescale,image,hdr,newimage,eit=eit,euvi=euvi
  if keyword_set(euvi) then begin
  if hdr.obsrvtry eq 'STEREO_A' then begin
  if hdr.wavelnth eq 171 then Amp=1.00010;1.00007
  if hdr.wavelnth eq 195 then Amp=1.02031;1.01898
  if hdr.wavelnth eq 284 then Amp=1.03499;1.03366
  if hdr.wavelnth eq 304 then Amp=0.99994;0.99956
  endif
  if hdr.obsrvtry eq 'STEREO_B' then begin
  if hdr.wavelnth eq 171 then Amp=0.999904;0.99993
  if hdr.wavelnth eq 195 then Amp=0.980481;0.98172
  if hdr.wavelnth eq 284 then Amp=0.967301;0.96847
  if hdr.wavelnth eq 304 then Amp=1.000060;1.00045
  endif
  endif
  newimage=image*Amp
  ;Preserve "-999" pixels in newimage
  p=where(image eq -999.)
  if p(0) ne -1 then newimage(p)=-999.
return
end

pro rescale_old,image,hdr,newimage,eit=eit,euvi=euvi
  if keyword_set(euvi) then begin
  if hdr.obsrvtry eq 'STEREO_A' then begin
  if hdr.wavelnth eq 171 then Amp=1.02654 
  if hdr.wavelnth eq 195 then Amp=1.03776 
  if hdr.wavelnth eq 284 then Amp=1.05133 
  if hdr.wavelnth eq 304 then Amp=1.
  endif
  if hdr.obsrvtry eq 'STEREO_B' then begin
  if hdr.wavelnth eq 171 then Amp=0.974796 
  if hdr.wavelnth eq 195 then Amp=0.964892 
  if hdr.wavelnth eq 284 then Amp=0.953450 
  if hdr.wavelnth eq 304 then Amp=1.
  endif
  endif
  newimage=image*Amp
  ;Preserve "-999" pixels in newimage
  p=where(image eq -999.)
  if p(0) ne -1 then newimage(p)=-999.
return
end

pro apply_mask_284B,datadir,filelist

mreadfits,'/data1/work/image.processing/mask_factor.fts'  ,h,mask_factor
mreadfits,'/data1/work/image.processing/mask_constant.fts',h,mask_constant

n=1
filename=''
openr,1,datadir+filelist
readf,1,n
for i=0,n-1 do begin
readf,1,filename
mreadfits,datadir+filename,hdr,ima
ima = ima * mask_factor + mask_constant
filename=filename+'.masked'
mwritefits,hdr,ima, outfile=datadir+filename
endfor
close,1

return
end


function euvi_decon, image, psf
; IDL code for deconvolution function
; input: psf, image
; output: image1 = deconned image
psf_size = (size(psf)  ) (1)  ; CORRECTED
img_size = (size(image)) (1)  ; CORRECTED
  ;--- Configure PSF for deconvolution, including padding and FFT
  
  psf_cent = floor(psf_size/2);CORRECTED         ; psf is, unsurprisingly centered at image center
  psf_pad = dblarr(2 * img_size, 2 * img_size)   ; pad to avoid edge artifacts
  psf_pad(0: img_size - 1, img_size: *) = psf    ; insert psf into pad
  
  psf_pad = shift(psf_pad, psf_cent * [-1, 1]) ; shift PSF for correct FFT result
  psf_hat = fft(psf_pad, 1)                    ; compute FFT'ed psf

  ;--- Prepare image and deconvolve

  img_pad = dblarr(2 * img_size, 2 * img_size)    ; image is padded as well
  img_pad(0: img_size - 1, img_size: *) = image   ; insert image into padded array
  img_hat = fft(img_pad, 1)                   ; compute FFT'ed padded image
  
  image1 = real_part(fft(img_hat/psf_hat, -1)) ; Deconvolve and apply inverse FFT
  image1 = image1(0: img_size - 1, img_size: *)   ; Remove padding
return, image1
end ; CORRECTED





