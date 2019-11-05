pro impose_min_max,datamax,data
pmin=(where(data eq min(data)))(0)
pmax=(where(data eq max(data)))(0)
data(pmin)=-datamax
data(pmax)=+datamax
return
end
