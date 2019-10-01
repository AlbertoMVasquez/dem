pro record_gif,dir,filename,dev,aia=aia

; set graph stuff
if dev eq 'X'  then begin
device, retain     = 2
device, true_color = 24
device, decomposed = 0
endif
if dev eq 'Z' then begin
   if not keyword_set(aia) then   Device, Decomposed=0, Set_Pixel_Depth=24, Set_Resolution=[1024,1024]
   if     keyword_set(aia) then   Device, Decomposed=0, Set_Pixel_Depth=24, Set_Resolution=[4096,4096]
endif

image24 = TVRD(True=1)
image2d = Color_Quan(image24, 1, r, g, b)
write_GIF, dir+filename, image2d, r, g, b

return
end
