pro grab_data_aia_new

 t0='10-jan-2011 00:00'
 t1='10-jan-2011 00:10'

enderezzo='alberto.m.vasquez@gmail.com'

ssw_cutout_service,t0,t1,/full_disk,waves=[94,131,171,193,211,335],max_frames=24,instrument='aia',email=enderezzo,/rice

ssw_service_get_data,"",/loud,out_dir='./test_multiband/'

return
end
