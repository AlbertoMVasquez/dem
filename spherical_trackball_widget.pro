;+
;  NAME: spherical_trackball_widget
;
;  PURPOSE:
;    Given vector fieldline data on a spherical grid, this procedure opens a
;    trackball widget that is used for interactive display and manipulation of
;    the fieldline data.
;
;  CALLING SEQUENCE:
;    spherical_trackball_widget,sph_data,im_data=im_data,imsc=imsc,
;      extra_objects=extra_objects
;
;  INPUTS:
;    sph_data = a structure of type spherical_field_data (see
;               spherical_field_data__define.pro) with the following fields
;               defined on input: ptr,ptth,ptph,nstep.  Basically, one needs
;               the trajectories of each fieldline in (r,theta,phi)-space.
;    im_data = optional structure of type spherical_image_data (see
;              spherical_image_data__define.pro) used as a background image
;              (default = br at the lowest radial gridpoint)
;    imsc = image scaling
;    extra_objects=extra objects to be added to all items in each view
;
;  OUTPUTS: (none)
;
;  NOTES:
;    1.  The code has been written so that multiple spherical datasets can be
;        accommodated in the future.
;    2.  Occasionally, the draw method from the IDLgrWindow destination object
;        sometimes generates underflow, overflow, and illegal operand errors
;        (at least in IDL v.6.1), but these tend to happen only
;        intermittently, and I don't see any problems with the output, so I
;        usually ignore the errors.  To minimize the screen output, I have set
;        !except=0 temporarily, but it will only get set back to what it was
;        only if File->Exit menu item is used.  I believe these are exceptions
;        returned from the renderer, and IDL passes them on.
;
;  MODIFICATION HISTORY:
;    M.DeRosa - 18 Jan 2006 - created
;               27 Jan 2006 - can now deal with incomplete spheres, and draws
;                             boundary lines at edges of domain at rmin
;               30 Jan 2006 - mouse movements can now pan as well as rotate
;               27 Mar 2007 - now deals with bounded images in a way
;                             consistent with spherical_draw_field
;                5 Apr 2007 - now zooms in (instead of out) when
;                             right-clicking and moving the mouse down, and
;                             vice versa (like Google Earth)
;               27 Jun 2007 - added outlines for bounded volumes (for full
;                             spherical volumes, only the latitudinal
;                             boundaries of the lower boundary are outlined)
;               23 Aug 2010 - added a check for whether fieldlines have been
;                             traced 
;               12 Nov 2010 - due to the way IDL version 8 interprets the "dot
;                             notation" wrt objects, I had to use square
;                             brackets instead of parentheses to get lineslike
;                             "olist.ofieldlines[i]->getproperty,color=col" to
;                             compile (7 places)
;               31 Jan 2011 - added extra_objects keyword
;                7 Feb 2011 - fixed issue with widget resize events that was
;                             causing the widget to crash in IDL v.8.0.
;               25 Apr 2011 - added an ambient and a fixed light (at the
;                             observer's position) so that surfaces
;                             provided through the extra_objects keyword are
;                             shaded properly
;               25 Apr 2011 - now accommodates the extra_objects tag in the
;                             sph_data structure
;               19 May 2011 - added "Keyword list" item to the main menu
;
;-

pro spherical_trackball_event,event

;  get event ID and widget state
widget_control,event.id,get_uval=uval

if event.id eq event.top then begin  ;  TLB resize event

  ;  redefine state
  state=uval
  
  ;  get current TLB size
  widget_control,event.top,tlb_get_size=newwid

  ;  compute difference between current and old TLB size
  dwid=newwid-state.twid

  ;  set new draw window size
  state.vwid=state.vwid+dwid

  ;  determine new zoom factor
  zoomfac=min(newwid/state.twid)
  state.vzoom=state.vzoom*zoomfac

  ;  resize the draw widget
  widget_control,state.wdraw,draw_x=state.vwid(0),draw_y=state.vwid(1)

  ;  compute new subwindow sizes
  state.vwidv=state.vwid/state.vmulti  ;  integer division

  ;  compute viewplane rectangle based on aspect ratio.
  aspect=float(state.vwidv(0))/float(state.vwidv(1))
  viewrect=[-1,-1,2,2]*state.drmax
  if aspect gt 1 then viewrect=viewrect*[aspect,1,aspect,1]*state.vzoom $
    else viewrect=viewrect/[1,aspect,1,aspect]*state.vzoom
;;   for i=0,state.nview-1 do state.oview[i]->setproperty,viewplane=viewrect, $
;;     dim=state.vwidv,loc=[(i mod state.vmulti(0))*state.vwidv(0),$
;;       (state.vmulti(1)-i/state.vmulti(0)-1)*state.vwidv(1)]
  ;  above command generates an error in IDLv8, simplified for single views
  ;  but may be fixed by making the argument to the loc keyword a float
  state.oview->setproperty,viewplane=viewrect,dim=state.vwidv

  ;  redraw view    
  state.owindow->draw

  ;  reset trackball
  state.otrack->reset,0.5*state.vwid,0.5*max(state.vwid)

endif else begin

  ;  get state
  widget_control,event.top,get_uval=state,/no_copy

  case (strsplit(uval,':',/extract))(0) of

    'EXIT': begin  ;  exit button pressed
      !except=state.dexcept  ;  return !except to what it was
      obj_destroy,state.ovgr
      widget_control,event.top,/destroy
      return
      end

    'DRAW': begin  ;  (re)draw view

      ;  determine whether we are panning or rotating
      if state.bmode eq 'pan' then translate=1b else translate=0b

      ;  handle trackball updates
      newview=state.otrack->update(event,transform=tmatrix,transl=translate)
      if newview gt 0 then begin
        if translate then begin  ;  apply translation to top model
          state.omodeltop->getproperty,transform=oldt
          state.omodeltop->setproperty,transform=oldt#tmatrix
        endif else begin  ;  apply rotation to inner models
          for i=0,state.nview-1 do begin
            state.omodelin[i]->getproperty,transform=oldt
            state.omodelin[i]->setproperty,transform=oldt#tmatrix
          endfor

          ;  compute rotation angles
          state.omodelin[0]->getproperty,transform=tmatrix
          yang=asin(tmatrix(2))
          if abs(cos(yang)) gt 0.005 then begin
            xang=atan(-tmatrix(6),tmatrix(10))
            zang=atan(-tmatrix(1),tmatrix(0))
          endif else begin
            xang=0.0
            zang=atan(tmatrix(4),tmatrix(5))
          endelse

          ;  convert these rotation angles to central (l,b)
          lc=((-zang*180./!dpi)+270) mod 360
          bc=((xang*180./!dpi)+90) mod 360
          if abs(bc) gt 90 then begin
            if bc gt 0 then bc=(180-bc) else bc=(-180-bc)
            lc=(lc+180) mod 360
          endif
          outstrlon='lon='+string(lc,f='(i4)')+'°'
          outstrlat='lat='+string(bc,f='(i4)')+'°'
          widget_control,state.wrotlon,set_val=outstrlon
          widget_control,state.wrotlat,set_val=outstrlat

        endelse

      endif

      ;  handle mouse button events within draw widget
      case event.type of  ;  button down
        0: begin
          widget_control,state.wdraw,/draw_motion_events
          state.bdowny=event.y
          state.bdownzoom=state.vzoom
          state.oview[0]->getproperty,viewplane=bdownview
          state.bdownview=bdownview
          case event.press of
            0b: state.bleft=1b
            4b: state.bright=1b
            else:
          endcase
          end
        1: begin   ;  button up
          widget_control,state.wdraw,draw_motion_events=0
          state.bleft=0b  &  state.bright=0b
          end
        2: begin  ;  adjust zoom
          if state.bright then begin
            for i=0,state.nview-1 do begin
              zoomfac=exp((event.y-state.bdowny)/float(state.vwid(1)))
              state.vzoom=state.bdownzoom*zoomfac
              state.oview[i]->setproperty,viewplane=state.bdownview*zoomfac
            endfor
          endif
          end
        else:  ;  pass through
      endcase

      ;  (re)draw view    
      state.owindow->draw

      end

    'SAVE': begin  ;  save to disk

      ;  (re)draw view    
      state.owindow->draw

      ;  get snapshot of draw window
      state.owindow->getproperty,image_data=outim

      ;  determine filename
      ftype=(strsplit(uval,':',/extract))(1)
      fname=dialog_pickfile(file='trackball_snapshot.'+strlowcase(ftype),/wri)

      ;  save to disk
      if fname ne '' then begin
        case ftype of
          'GIF': write_gif,fname,color_quan(outim,1,r,g,b),r,g,b
          'JPG': write_jpeg,fname,outim,/true,q=90
          'PNG': write_png,fname,outim
          'TIF': write_tiff,fname,reverse(outim,3)
          'EPS': begin  ;  clipboard idea from D.Fanning  (thanks David!)
            state.owindow->getproperty,dim=dim,units=units
            res300=replicate(2.54/300,2)  ;  300 DPI
            clip=obj_new('IDLgrClipboard',dim=dim,res=res,units=units,q=2)
            clip->draw,state.ovgr,file=fname,/postscript,/vector
            obj_destroy,clip
            end
          else:  ;  shouldn't be able to get here
        endcase
      endif

      end

    'KEYS': begin  ;  write out keywords for spherical_draw_field command

      ;  compute rotation angles, and convert to central (l,b)
      state.omodelin[0]->getproperty,transform=tmatrix
      yang=asin(tmatrix(2))
      if abs(cos(yang)) gt 0.005 then begin
        xang=atan(-tmatrix(6),tmatrix(10))
        zang=atan(-tmatrix(1),tmatrix(0))
      endif else begin
        xang=0.0
        zang=atan(tmatrix(4),tmatrix(5))
      endelse
      lc=((-zang*180./!dpi)+270) mod 360
      bc=((xang*180./!dpi)+90) mod 360
      if abs(bc) gt 90 then begin
        if bc gt 0 then bc=(180-bc) else bc=(-180-bc)
        lc=(lc+180) mod 360
      endif
      keystr='lcent='+strcompress(lc,/r)+',bcent='+strcompress(bc,/r)

      ;  compute roll angle (to do)

      ;  get image scaling
      keystr=keystr+',imsc='+strcompress(state.imsc,/r)

      ;  get size of view
      state.oview->getproperty,dim=dim
      keystr=keystr+',xsize='+strcompress(round(dim(0)),/r)
      keystr=keystr+',ysize='+strcompress(round(dim(1)),/r)

      ;  get scaling
      state.oview->getproperty,viewplane=viewrect
      keystr=keystr+',width='+strcompress(viewrect(2)/(2*state.drmax),/r)

      ;  account for panning (to do)

      ;  output string to window
      print,'-----'
      print,'(Some of) the keywords needed by spherical_draw_field.pro to replicate this rendering are:'
      print,keystr
      print,'-----'

      end

    'MOUSE': begin  ;  mouse behavior events

      ;  determine what happened
      if (strsplit(uval,':',/extract))(1) eq 'PAN' then state.bmode='pan' $
        else state.bmode='rot'
      
      ;  (re)draw view    
      state.owindow->draw

      end

    else: stop  ;  unrecognized event...

  endcase

  ;  set state
  widget_control,event.top,set_uval=state,/no_copy      

endelse

end

;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

pro spherical_trackball_widget,sph_data,im_data=im_data,imsc=imsc,$
  extra_objects=extra_objects,for_ps=for_ps,noimage=noimage

;  set RGB triplets of line colors
bla=byte([  0,  0,  0])
yel=byte([255,255,  0])
gre=byte([  0,255,  0])
red=byte([255,  0,255])
whi=byte([255,255,255])

;  set RGB triplets of line colors
bla=byte([  0,  0,  0])  ;  0 in palette
yel=byte([255,255,  0])  ;  not in palette
gre=byte([  0,255,  0])  ;  250/251 in palette
fucsia=byte([255,  0,255])  ;  252/253 in palette
whi=byte([255,255,255])  ;  254/255 in palette

red=byte([205,  0,0]) 
blu=byte([0,  0, 80])  

 darkblu=byte([0,  0, 170])  
lightblu=byte([0,  0, 255])  
 darkred=byte([170,  0,0]) 
lightred=byte([255,  0,0]) 

if keyword_set(for_ps) then backcol=whi else backcol=bla

;  usage message
if n_elements(sph_data) eq 0 then begin
  print,'  spherical_trackball_widget,sph_data,im_data=im_data,imsc=imsc'+$
    'extra_objects=extra_objects'
  return
endif

;  count extra objects
nextra1=n_elements(extra_objects)

;  turn off exception reporting
oldexcept=!except
!except=0

;  get number of views
nview=n_elements(sph_data)
if nview eq 1 then multiview=[1,1] else $
  if n_elements(multiview) ne 2 then multiview=[2,nview/2]  ;  [col,row]
nview=nview<round(product(multiview))

;  set initial window size
xwid=round(400*multiview(0)) < 1000
ywid=round(400*multiview(1)) <  800

;  set subwindow sizes
xwidv=xwid/multiview(0)  ;  integer division
ywidv=ywid/multiview(1)  ;  integer division

;  get ranges and scaling of x,y,z
rmax=max(*sph_data.rix,min=rmin)
thmin=min(*sph_data.theta,max=thmax)
phmin=min(*sph_data.phi,max=phmax)
xra=(yra=(zra=dblarr(2,nview)))
ranges=dblarr(2,nview)
for i=0,nview-1 do ranges(*,i)=[-1,1]*rmax
scales=[0.,1.]  ;  no scaling for x,y,z

;  determine if there are longitudinal bounds
if n_elements(sph_data.lonbounds) gt 0 then begin
  if (sph_data.lonbounds(0) ge 0) and (sph_data.lonbounds(1) ne 0) then begin
    ph1=sph_data.lonbounds(0)*!dpi/180
    ph2=sph_data.lonbounds(1)*!dpi/180
    bounded=1b
  endif else bounded=0b
endif else bounded=0b

;  create the widget
master=widget_base(/row,/base_align_left,/tlb_size_events,mbar=wmbar, $
  title='Field Line Renderer')

;  file menu
version=float(!version.release)  ;  for GIF availability test (below)
wmenuf=widget_button(wmbar,val='File ',/menu)
wsave=widget_button(wmenuf,val='Save as ...',/menu)
weps=widget_button(wsave,val='EPS',uval='SAVE:EPS')
if (version le 5.3) or (version ge 6.1) then $
  wgif=widget_button(wsave,val='GIF',uval='SAVE:GIF')
wjpeg=widget_button(wsave,val='JPEG',uval='SAVE:JPG')
wpng=widget_button(wsave,val='PNG',uval='SAVE:PNG')
wtiff=widget_button(wsave,val='TIFF',uval='SAVE:TIF')
wcommand=widget_button(wmenuf,val='Keyword list',uval='KEYS')
wexitb=widget_button(wmenuf,val='Exit',uval='EXIT')

;  options along left side
woptions=widget_base(master,/col)

;  rotation angle info
wrotb=widget_base(woptions,/col,frame=5)
wrotlon=widget_label(wrotb,val='lon=   0°',font='Courier')
wrotlat=widget_label(wrotb,val='lat=   0°',font='Courier')

;  rotate/pan button select
wpanb=widget_base(woptions,/col,frame=5,/exclusive)
wclickrot=widget_button(wpanb,val='rotate',uval='MOUSE:ROTATE')
wclickpan=widget_button(wpanb,val='pan',uval='MOUSE:PAN')
widget_control,wclickrot,/set_button  &  bmode='rot'

;  draw window
wdrawb=widget_base(master)
wdraww=widget_draw(wdrawb,xsize=xwid,ysize=ywid,uvalue='DRAW',retain=0, $
  /expose_events,/button_events,graphics_level=2)

;  compute viewplane rectangle based on aspect ratio
aspect=float(xwidv)/float(ywidv)  ;  aspect ratio of subwindow
viewrect=[-1,-1,2,2]*rmax
if aspect gt 1 then viewrect=viewrect*[aspect,1,aspect,1] $
  else viewrect=viewrect/[1,aspect,1,aspect]

;  set up each box
oview=objarr(nview)
omodeltop=objarr(nview)
omodelin=objarr(nview)
for j=0,nview-1 do begin

  ;  create views
  oview(j)=obj_new('IDLgrView',viewplane=viewrect,proj=1,     color=backcol,$
    dim=[xwidv,ywidv],zclip=[0.1,ranges(0,j)-ranges(1,j)-0.1],eye=2*rmax+1, $
    loc=[(j mod multiview(0))*xwidv,(multiview(1)-j/multiview(0)-1)*ywidv])


  ;  create the model
  omodeltop(j)=obj_new('IDLgrModel')
  oview(j)->add,omodeltop(j)

  ;  move top model so that the front coincides with the viewplane at z=0
  omodeltop(j)->translate,0,0,-rmax

  ;  add an ambient light and a directional light
  olight1=obj_new('IDLgrLight',type=1,loc=[0,0,2*rmax+1],intensity=0.8)
  olight2=obj_new('IDLgrLight',type=0,intensity=0.5)
  omodeltop(j)->add,olight1
  omodeltop(j)->add,olight2

  ;  put an inner model inside the outer model
  omodelin(j)=obj_new('IDLgrModel')
  omodeltop(j)->add,omodelin(j)

  ;  position model for first draw
  omodelin(j)->rotate,[1,0,0],-90  ;  point rotation axis up
  omodelin(j)->rotate,[0,1,0],-90  ;  rotate so that long=0 is at front

  ;  create fieldline objects
  if ptr_valid(sph_data(j).nstep) then begin
    nlines=n_elements(*sph_data(j).nstep)
    open=intarr(nlines)
    firstline=1
    for i=0l,nlines-1 do begin

      ;  only draw lines that have line data
      ns=(*sph_data(j).nstep)(i)
      if ns gt 0 then begin

        ;  determine whether field lines are open or closed
        if (max((*sph_data.ptr)(0:ns-1,i))-rmin)/(rmax-rmin) gt 0.99 then begin
          irc=get_interpolation_index(*sph_data.rix,(*sph_data.ptr)(0,i))
          ithc=get_interpolation_index( $
            *sph_data.lat,90-(*sph_data.ptth)(0,i)*!radeg)
          iphc=get_interpolation_index( $
            *sph_data.lon,((*sph_data.ptph)(0,i)*!radeg+360) mod 360)
          brc=interpolate(*sph_data.br,iphc,ithc,irc)
          if brc gt 0 then open(i)=1 else open(i)=-1
        endif  ;  else open(i)=0, which has already been done

        ;  flag those lines that go higher than the first radial gridpoint
        heightflag=max((*sph_data.ptr)(0:ns-1,i)) gt (*sph_data.rix)(1)

        ;  create an object for this line and add it to the model
        if heightflag then begin

          ;  set appropriate color
           case open(i) of
            -1: col=fucsia
             0: if keyword_set(for_ps) then col=bla else col=whi
             1: col=gre
          endcase

        GOTO,SKIPCOLOR4
           case i of
             0: col=darkred
             1: col=lightblu
             2: col=darkblu
             3: col=lightred
             4: col=bla
             5: col=bla
          endcase
       SKIPCOLOR4:

          thick=1

          ;  transform from spherical to cartesian coordinates
          linecoords=cv_coord(/to_rect,from_sph=transpose( $
            [[(*sph_data.ptph)(0:ns-1,i)],[!dpi/2-(*sph_data.ptth)(0:ns-1,i)], $
            [(*sph_data.ptr)(0:ns-1,i)]]))

          ;  create polyline object
          flobj=obj_new('IDLgrPolyline',linecoords,color=col,thick=thick, $
            xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
          if firstline then begin
            ofieldlines=flobj
            firstline=0
          endif else ofieldlines=[ofieldlines,flobj]
  
        endif

      endif

    endfor

    ;  add fieldlines to model
    if n_elements(ofieldlines) gt 0 then omodelin(j)->add,ofieldlines
  endif else begin
    print,'  WARNING from spherical_trackball_widget: no fieldlines are traced'
  endelse

;  create the texture map for the spherical surface at the lower
;  radius                          |
if not keyword_set(noimage) then begin

  ;  create the spherical_image_data structure
  if n_elements(im_data) eq 0 then begin
    im_data=spherical_image_create((*sph_data.br)(*,*,0),$
      *sph_data.lon,*sph_data.lat)
  endif

  ;  check to see if image completely spans all longitudes
  dphi=min(abs((*im_data.phi)(1:2)-(*im_data.phi)(0:1)))  ;  get grid spacing
  if round((2*!dpi)/dphi) ne im_data.nlon then imbound=1 else imbound=0

  ;  replicate last longitude so that no seam in the texture map appears
  imd2=im_data    
  if imbound eq 0 then begin
    imd2.image=ptr_new([*im_data.image,(*im_data.image)(0,*)])
    imd2.lon=ptr_new([*im_data.lon,360+(*im_data.lon)(0)])
    imd2.phi=ptr_new([*im_data.phi,2*!dpi+(*im_data.phi)(0)])
    imd2.nlon=im_data.nlon+1
  endif

  ;  get byte image to texture map
  if n_elements(imsc) eq 0 then $
    imsc=max([-min(*imd2.image),max(*imd2.image)])
  scim,*imd2.image,outim=byteimage,sc=imsc,/nowin,/quiet,top=249
  otexmap=obj_new('IDLgrImage',data=byteimage)

  ;  create vertex list for polygonal spherical surface
  phgrid=(*imd2.phi)#replicate(1,imd2.nlat)
  thgrid=!dpi/2-replicate(1,imd2.nlon)#(*imd2.theta)
  radgrid=replicate(imd2.rad,imd2.nlon,imd2.nlat)
  sph_vert=transpose([[[phgrid]],[[thgrid]],[[radgrid]]],[2,0,1])
  sph_vert=reform(sph_vert,3,imd2.nlon*imd2.nlat,/overwrite)
  rect_vert=cv_coord(from_sph=sph_vert,/to_rect)

  ;  create the connectivity array
  connect=lonarr(5,imd2.nlon-1,imd2.nlat-1)
  for i=0,(imd2.nlat-1)-1 do for k=0,(imd2.nlon-1)-1 do begin
    stpt=i*imd2.nlon+k  ;  index of starting point in vertex list
    connect(*,k,i)=[4,stpt,stpt+1,stpt+1+imd2.nlon,stpt+imd2.nlon]
  endfor

  ;  determine the texture map coordinates
  if imbound then begin
    phirange=(*imd2.phi)(imd2.nlon-1)-(*imd2.phi)(0)
    if phirange lt 0 then phirange=phirange+2*!dpi
    texture_coord=[(sph_vert(0,*)-(*imd2.phi)(0))/phirange, $
      (sph_vert(1,*)-min(thgrid))/(max(thgrid)-min(thgrid))]
  endif else begin
    texture_coord=[(sph_vert(0,*)-min(sph_vert(0,*)))/(2*!dpi), $
      (sph_vert(1,*)-min(thgrid))/(max(thgrid)-min(thgrid))]
  endelse

  ;  add the texture mapped image to the model
  osphere=obj_new('IDLgrPolygon',data=rect_vert,polygons=connect,col=whi,$
    style=2,texture_map=otexmap,texture_coord=texture_coord, $
    /texture_interp)
  omodelin(j)->add,osphere

  ;  finally create outlines

  ;  create outlines for image border
  if bounded then begin
    loncoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(ph1,sph_data.nlat)],[!dpi/2-(*sph_data.theta)], $
      [replicate(imd2.rad,sph_data.nlat)]]))
    blobj=obj_new('IDLgrPolyline',loncoords,color=whi,thick=thick, $
      xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
    loncoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(ph2,sph_data.nlat)],[!dpi/2-(*sph_data.theta)], $
      [replicate(imd2.rad,sph_data.nlat)]]))
    blobj=[blobj,obj_new('IDLgrPolyline',loncoords,color=whi,thick=thick, $
      xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)]
    omodelin(j)->add,blobj
  endif

  ;  create outlines for image_border
  latcoords=cv_coord(/to_rect,from_sph=transpose( $
    [[*sph_data.phi],[replicate(!dpi/2-thmin,sph_data.nlon)], $
    [replicate(imd2.rad,sph_data.nlon)]]))
  blobj=obj_new('IDLgrPolyline',latcoords,color=whi,thick=thick, $
   xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
  latcoords=cv_coord(/to_rect,from_sph=transpose( $
    [[*sph_data.phi],[replicate(!dpi/2-thmax,sph_data.nlon)], $
    [replicate(imd2.rad,sph_data.nlon)]]))
  blobj=[blobj,obj_new('IDLgrPolyline',latcoords,color=whi,thick=thick, $
    xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)]
  omodelin(j)->add,blobj

  ;  create outlines for longitudinal boundaries at bottom
  if bounded then begin
    loncoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(ph1,sph_data.nlat)],[!dpi/2-(*sph_data.theta)], $
      [replicate(rmin,sph_data.nlat)]]))
    blobj=obj_new('IDLgrPolyline',loncoords,color=whi,thick=thick, $
      xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
    loncoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(ph2,sph_data.nlat)],[!dpi/2-(*sph_data.theta)], $
      [replicate(rmin,sph_data.nlat)]]))
    blobj=[blobj,obj_new('IDLgrPolyline',loncoords,color=whi,thick=thick, $
      xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)]
    omodelin(j)->add,blobj
  endif

  ;  create outlines for latitudinal boundaries at bottom
  latcoords=cv_coord(/to_rect,from_sph=transpose( $
    [[*sph_data.phi],[replicate(!dpi/2-thmin,sph_data.nlon)], $
    [replicate(rmin,sph_data.nlon)]]))
  blobj=obj_new('IDLgrPolyline',latcoords,color=whi,thick=thick, $
   xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
  latcoords=cv_coord(/to_rect,from_sph=transpose( $
    [[*sph_data.phi],[replicate(!dpi/2-thmax,sph_data.nlon)], $
    [replicate(rmin,sph_data.nlon)]]))
  blobj=[blobj,obj_new('IDLgrPolyline',latcoords,color=whi,thick=thick, $
    xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)]
  omodelin(j)->add,blobj

  ;  create outlines for longitudinal boundaries at top
  if bounded then begin
    loncoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(ph1,sph_data.nlat)],[!dpi/2-(*sph_data.theta)], $
      [replicate(rmax,sph_data.nlat)]]))
    blobj=obj_new('IDLgrPolyline',loncoords,color=whi,thick=thick, $
      xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
    loncoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(ph2,sph_data.nlat)],[!dpi/2-(*sph_data.theta)], $
      [replicate(rmax,sph_data.nlat)]]))
    blobj=[blobj,obj_new('IDLgrPolyline',loncoords,color=whi,thick=thick, $
      xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)]
    omodelin(j)->add,blobj
  endif

  ;  create outlines for latitudinal boundaries at top
  if bounded then begin
    latcoords=cv_coord(/to_rect,from_sph=transpose( $
      [[*sph_data.phi],[replicate(!dpi/2-thmin,sph_data.nlon)], $
      [replicate(rmax,sph_data.nlon)]]))
    blobj=obj_new('IDLgrPolyline',latcoords,color=whi,thick=thick, $
     xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
    latcoords=cv_coord(/to_rect,from_sph=transpose( $
      [[*sph_data.phi],[replicate(!dpi/2-thmax,sph_data.nlon)], $
      [replicate(rmax,sph_data.nlon)]]))
    blobj=[blobj,obj_new('IDLgrPolyline',latcoords,color=whi,thick=thick, $
      xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)]
    omodelin(j)->add,blobj
  endif

  ;  create outlines for radial boundaries
  if bounded then begin
    radcoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(phmin,sph_data.nr)],[replicate(!dpi/2-thmin,sph_data.nr)], $
      [*sph_data.rix]]))
    blobj=obj_new('IDLgrPolyline',radcoords,color=whi,thick=thick, $
     xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
    omodelin(j)->add,blobj
    radcoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(phmax,sph_data.nr)],[replicate(!dpi/2-thmin,sph_data.nr)], $
      [*sph_data.rix]]))
    blobj=obj_new('IDLgrPolyline',radcoords,color=whi,thick=thick, $
     xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
    omodelin(j)->add,blobj
    radcoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(phmin,sph_data.nr)],[replicate(!dpi/2-thmax,sph_data.nr)], $
      [*sph_data.rix]]))
    blobj=obj_new('IDLgrPolyline',radcoords,color=whi,thick=thick, $
     xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
    omodelin(j)->add,blobj
    radcoords=cv_coord(/to_rect,from_sph=transpose( $
      [[replicate(phmax,sph_data.nr)],[replicate(!dpi/2-thmax,sph_data.nr)], $
      [*sph_data.rix]]))
    blobj=obj_new('IDLgrPolyline',radcoords,color=whi,thick=thick, $
     xcoord_conv=scales,ycoord_conv=scales,zcoord_conv=scales)
    omodelin(j)->add,blobj

  endif

endif

  ;  add extra objects
  if nextra1 gt 0 then begin
    for k=0,nextra1-1 do begin
      extra_objects[k]->setproperty,xcoord_conv=scales,ycoord_conv=scales,$
        zcoord_conv=scales
      omodelin(j)->add,extra_objects(k),/alias
    endfor
  endif
  if ptr_valid(sph_data.extra_objects) then begin
    nextra2=n_elements(*sph_data.extra_objects)
    if nextra2 gt 0 then begin
      for k=0,nextra2-1 do begin
        (*sph_data.extra_objects)(k)->setproperty,xcoord_conv=scales,$
          ycoord_conv=scales,zcoord_conv=scales
        omodelin(j)->add,(*sph_data.extra_objects)(k),/alias
      endfor
    endif
  endif

endfor

;  create holder containing trackball and view (for easy destruction)
otrack=obj_new('Trackball',0.5*[xwid,ywid],0.5*max([xwid,ywid]))
ovgr=obj_new('IDLgrViewgroup')
ovgr->add,otrack
ovgr->add,oview

;  realize widget
widget_control,master,/realize

;  get id of window object
widget_control,wdraww,get_value=owindow

;  add graphics tree
owindow->setproperty,graphics_tree=ovgr

;  get size of top level base
widget_control,master,tlb_get_size=twid

;  save state of widget (o=object,w=widget,b=mousebutton,v=view,t=TLB,d=data)
state={owindow:owindow,oview:oview,omodelin:omodelin,otrack:otrack,ovgr:ovgr,$
       omodeltop:omodeltop,wdraw:wdraww,wrotlon:wrotlon,wrotlat:wrotlat,$
       bleft:0b,bright:0b,bdowny:0l,bdownzoom:1.0,bdownview:viewrect,$
       bmode:bmode,imsc:imsc,$
       vwid:[xwid,ywid],vwidv:[xwidv,ywidv],vmulti:multiview,vzoom:1.0,$
       twid:twid,nview:nview,dexcept:oldexcept,drmax:rmax}
widget_control,master,set_uval=state,/no_copy

;  relinquish control to manager
xmanager,'spherical_trackball',master;,/no_block

end
