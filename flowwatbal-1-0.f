      program flowwatbal_1_0
!
      parameter (nx5min=4320,ny5min=2160,ncodes=9999999,nsumcod=10000,
     #nmnth=12,nmnthyr=13)
      integer isubcod(nsumcod),isumcod(nsumcod),imajcod(nsumcod),
     #isum(ny5min,nx5min),itst(nsumcod),itomem(nsumcod),ito(0:nsumcod),
     #isub(ny5min,nx5min),isubnum(0:ncodes),isumnum(0:ncodes)
      real rareaha(ny5min,nx5min),rarirrpct(ny5min,nx5min),
     #rgeto(ny5min,nx5min),rgprc(ny5min,nx5min),rgetact(ny5min,nx5min),
     #rgflx(ny5min,nx5min),rgrun(ny5min,nx5min),rgetaopw(ny5min,nx5min),
     #rgetawet(ny5min,nx5min),rgetairr(ny5min,nx5min),
     #rgetact2(ny5min,nx5min),rgprc2(ny5min,nx5min),
     #rgetact3(ny5min,nx5min),rgetirr3(ny5min,nx5min),
     #rgetopw3(ny5min,nx5min),rgetwet3(ny5min,nx5min),
     #rarea(nsumcod),reto(nmnthyr,nsumcod),rprc(nmnthyr,nsumcod),
     #rprc2(nmnthyr,nsumcod),reta(nmnthyr,nsumcod),
     #rrun(nmnthyr,nsumcod),rgwf(nmnthyr,nsumcod),ropw(nmnthyr,nsumcod),
     #rwet(nmnthyr,nsumcod),rarirr(nsumcod),retirr(nmnthyr,nsumcod),
     #retoy(nsumcod),rprcy(nsumcod),rprc2y(nsumcod),
     #retay(nsumcod),rruny(nsumcod),rgwfy(nsumcod),
     #ropwy(nsumcod),rwety(nsumcod),retirry(nsumcod),
     #retoy2(nsumcod),rprcy2(nsumcod),rprc2y2(nsumcod),
     #retay2(nsumcod),rruny2(nsumcod),rgwfy2(nsumcod),
     #ropwy2(nsumcod),rwety2(nsumcod),retirry2(nsumcod),
     #rareay2(nsumcod),rarirry2(nsumcod),
     #retoy3(nsumcod),rprcy3(nsumcod),
     #rprc2y3(nsumcod),retay3(nsumcod),
     #rruny3(nsumcod),rgwfy3(nsumcod),
     #ropwy3(nsumcod),rwety3(nsumcod),
     #retirr3(nmnthyr,nsumcod),retrfirr3(nmnthyr,nsumcod),
     #rareay3(nsumcod),rarirry3(nsumcod),
     #ropwat(ny5min,nx5min),rwetsol(ny5min,nx5min),
     #rowkc(ny5min,nx5min),rwetkc(ny5min,nx5min),
     #rcarryov(nsumcod),rcumarea(nsumcod),
     #rstorage(nmnthyr,nsumcod),rdischarge(nmnthyr,nsumcod),
     #rincome(nmnthyr,nsumcod),rcorr(nsumcod),rdeficit(nmnthyr,nsumcod)
      character  csubname(nsumcod)*50,cmajname(nsumcod)*50,
     #csumname(nsumcod)*60,cmnth(nmnthyr)*4,chead0*11,chead1*8,
     #chead2*50,chead3*8,chead4*50,chead5*11,chead6*11,chead7*11,
     #chead8*11,chead9*11,chead10*11,chead11*11,chead12*11,chead13*11,
     #chead14*11,chead15*11,chead16*11,chead17*11,chead20*8,chead21*20,
     #cformat*50
      character*100 cfil,cfilprc(nmnth),cfileto(nmnth),cfilprc2(nmnth),
     #cfileta(nmnth),cfilflx(nmnth),cfilrun(nmnth),cfilirr(nmnth),
     #cfilout2(nmnth),cfilsubbas,cfilout1mn,cfilout1y,cfilout1y2,
     #cfilout1y3,cfiloutcntmn,cfilout2yact,cfilout2yirr,cfilout2yopw,
     #cfilout2ywet,cfilowkc,cfilwetkc,cfilaccrun
      logical lmajtst(nsumcod)
!
!     open parameter file
      open(10,file='globwat-1-0.inp',status='old')
      read(10,*)
      read(10,*)
      read(10,*)
!     Skip mask grid
      read(10,*)cfil
      read(10,*)
!     read the grids with monthly precipitation
      do i = 1,nmnth
        read(10,*)cfilprc(i)
      enddo
      read(10,*)
!     read the grids with monthly reference evapotranspiration
      do i = 1,nmnth
        read(10,*)cfileto(i)
      enddo
      read(10,*)
!     Skip wet days files
      do i = 1,nmnth
        read(10,*)cfil
      enddo
      read(10,*)
!     Skip coefficient of variation of precipitation files
      do i = 1,nmnth
        read(10,*)cfil
      enddo
      read(10,*)
!     skip file with maximum soil moisture
      read(10,*)cfil
      read(10,*)
!     skip file with initial soil moisture
      read(10,*)cfil
      read(10,*)
!     skip file with maximum percolation flux
      read(10,*)cfil
      read(10,*)
!     skip file with average effective rooting depth
      read(10,*)cfil
      read(10,*)
!     skip file with land use classes
      read(10,*)cfil
      read(10,*)
!     skip file with soil moisture correction factor
      read(10,*)cfil
      read(10,*)
!     skip file with percolation flux correction factor
      read(10,*)cfil
      read(10,*)
!     skip file with kc-value per land use class
      read(10,*)cfil
      read(10,*)
!     skip number of iterations
      read(10,*)
      read(10,*)
!     skip random indicator
      read(10,*)
      read(10,*)
!     if irrigation should be calculated the variable irri should be 1
      read(10,*)irri
      read(10,*)
!     skip grid with codes to be linked with irrigation kc-values and
!     intensities
      read(10,*)cfil
      read(10,*)
!     open grid with irrigation density
      read(10,*)cfil
      if(irri.eq.1) open(16,file=cfil,status='old')
      read(10,*)
!     skip file kc-values
      read(10,*)cfil
      read(10,*)
!     skip file with irrigation intensity
      read(10,*)cfil
      read(10,*)
!     skip grid with soil moisture
      read(10,*)cfil
      read(10,*)
!     read the grids with monthly generated precipitation
      do i = 1,nmnth
        read(10,*)cfilprc2(i)
      enddo
      read(10,*)
!     read the grids with monthly actual ET
      do i = 1,nmnth
        read(10,*)cfileta(i)
      enddo
      read(10,*)
!     read the grids with monthly deep percolation
      do i = 1,nmnth
        read(10,*)cfilflx(i)
      enddo
      read(10,*)
!     read the grids with monthly runoff
       do i = 1,nmnth
        read(10,*)cfilrun(i)
      enddo
      read(10,*)
!     read the grids with monthly irrigation
      do i = 1,12
        read(10,*)cfilirr(i)
      enddo
      read(10,*)
      read(10,*)
      read(10,*)
!     open grid with area in ha of 5 minute cells
      read(10,*)cfil
      open(31,file=cfil,status='old')
      read(10,*)
!     open grid with open water density
      read(10,*)cfil
      open(32,file=cfil,status='old')
      read(10,*)
!     open grid with open water kc-values
      read(10,*)cfilowkc
      open(33,file=cfilowkc,status='old')
      read(10,*)
!     open grid with wetlands and marshes
      read(10,*)cfil
      open(34,file=cfil,status='old')
      read(10,*)
!     open grid with wetlands and marshes kc-values
      read(10,*)cfilwetkc
      if(cfilwetkc.ne.cfilowkc) open(35,file=cfilwetkc,status='old')
      read(10,*)
!     open grid with subbasins
      read(10,*)cfil
      open(38,file=cfil,status='old')
      read(10,*)
!     open list with names and flow direction codes for each sub-basin
      read(10,*)cfilsubbas
      open(40,file=cfilsubbas,status='old')
      read(10,*)
!     open grid with summary codes
      read(10,*)cfil
      open(42,file=cfil,status='old')
      read(10,*)
!     open list with names for each summary code
      read(10,*)cfil
      open(44,file=cfil,status='old')
      read(10,*)
!     if needed write monthly output files per subbasin (imnout = 1)
      read(10,*)ioutmn
      read(10,*)
!     read the output file name for monthly balances per subbasin
      read(10,*)cfilout1mn
      read(10,*)
!     read the output file name for the yearly balance per subbasin
      read(10,*)cfilout1y
      read(10,*)
!     read the output file name for the yearly balance per major basin
      read(10,*)cfilout1y2
      read(10,*)
!     if needed write monthly output files per subbasin (imncntout = 1)
      read(10,*)ioutcntmn
      read(10,*)
!     read the output file name for monthly balances per subbasin
      read(10,*)cfiloutcntmn
      read(10,*)
!     read the output file name for the yearly balance per summary code
      read(10,*)cfilout1y3
      read(10,*)
!     if needed write output grids with monthly actual
!     evapotranspiration adjusted for open water evaporation
      read(10,*)imnoutgrd
      read(10,*)
!     read the output file names for monthly grids
      do i = 1,nmnth
        read(10,*)cfilout2(i)
      enddo
      read(10,*)
!     read output grid with yearly actual evaporation corrected for
!     incremental evapotranspiration of irrigation, water and wetlands
      read(10,*)cfilout2yact
      read(10,*)
!     read output grid with yearly incremental evapotranspiration over
!     irrigated areas
      read(10,*)cfilout2yirr
      read(10,*)
!     read output grid with yearly incremental evapotranspiration over
!     open water
      read(10,*)cfilout2yopw
      read(10,*)
!     read output grid with yearly incremental evapotranspiration over
!     wetlands
      read(10,*)cfilout2ywet
      read(10,*)
!     if needed calculate monthly accumulated runoff per subbabsin
!     (iaccrun = 1)
      read(10,*)iaccrun
      read(10,*)
!     read the output file name for the monthly ccumulated runoff
!     per subbasin
      read(10,*)cfilaccrun
!     end reading parameter file
!
!-----------------------------------------------------------------------
!     Initialise character strings
      cmnth(1) = 'jan.'
      cmnth(2) = 'feb.'
      cmnth(3) = 'mar.'
      cmnth(4) = 'apr.'
      cmnth(5) = 'may '
      cmnth(6) = 'jun.'
      cmnth(7) = 'jul.'
      cmnth(8) = 'aug.'
      cmnth(9) = 'sep.'
      cmnth(10) = 'oct.'
      cmnth(11) = 'nov.'
      cmnth(12) = 'dec.'
      cmnth(13) = 'tot.'
!
!     initialise grids
      do j=1,ny5min
        do k=1,nx5min
           rareaha(j,k) = -9999.
           ropwat(j,k) = -9999.
           rowkc(j,k)  = -9999.
           rwetsol(j,k) = -9999.
           rwetkc(j,k) = -9999.
           rarirrpct(j,k) = -9999.
           isub(j,k) = -9999
           isum(j,k) = -9999
           rgetact3(j,k) = -9999.
           rgetirr3(j,k) = -9999.
           rgetopw3(j,k) = -9999.
           rgetwet3(j,k) = -9999.
        enddo
      enddo
!     read input grids and fill variables
!     skip the first 6 rows
      do i=1,6
        if(irri.eq.1)read(16,*)
        read(31,*)
        read(32,*)
        read(33,*)
        read(34,*)
        if(cfilwetkc.ne.cfilowkc)read(35,*)
        read(38,*)
        read(42,*)
      enddo
      do iy=1,ny5min
        if(irri.eq.1)read(16,*)(rarirrpct(iy,ix),ix=1,nx5min)
        read(31,*)(rareaha(iy,ix),ix=1,nx5min)
        read(32,*)(ropwat(iy,ix),ix=1,nx5min)
        read(33,*)(rowkc(iy,ix),ix=1,nx5min)
        read(34,*)(rwetsol(iy,ix),ix=1,nx5min)
        if(cfilwetkc.ne.cfilowkc)read(35,*)(rwetkc(iy,ix),ix=1,nx5min)
        read(38,*)(isub(iy,ix),ix=1,nx5min)
        read(42,*)(isum(iy,ix),ix=1,nx5min)
      enddo
      if(irri.eq.1)close(16)
      close(31)
      close(32)
      close(33)
      close(34)
      if(cfilwetkc.ne.cfilowkc)close(35)
      close(38)
      close(42)
!
!     initialise values per summary code
      do i=1,nsumcod
        do j=1,nmnthyr
          reto(j,i)=0.
          rprc(j,i)=0.
          rprc2(j,i)=0.
          reta(j,i)=0.
          rgwf(j,i)=0.
          rrun(j,i)=0.
          ropw(j,i)=0.
          rwet(j,i)=0.
          retirr(j,i)=0.
          retirr3(j,i)=0.
          retrfirr3(j,i)=0.
!         variables used to calculate accumulated runoff
          rstorage(j,i)=0.
          rdischarge(j,i)=0.
          rincome(j,i)=0.
          rdeficit(j,i)=0.
        enddo
        rarea(i)=0.
        retoy(i)=0.
        rprcy(i)=0.
        rprc2y(i)=0.
        retay(i)=0.
        rgwfy(i)=0.
        rruny(i)=0.
        ropwy(i)=0.
        rwety(i)=0.
        rareay2(i)=0.
        retoy2(i)=0.
        rprcy2(i)=0.
        rprc2y2(i)=0.
        retay2(i)=0.
        rgwfy2(i)=0.
        rruny2(i)=0.
        ropwy2(i)=0.
        rwety2(i)=0.
        rareay3(i)=0.
        isubcod(i)=0
        imajcod(i)=0
        isumcod(i)=0
        csubname(i)='xxx---xxx'
        cmajname(i)='xxx---xxx'
        csumname(i)='xxx---xxx'
        lmajtst(i)=.false.
        retirry(i) = 0.
        retirry2(i) = 0.
        rarirry2(i) = 0.
        rarirry3(i) = 0.
        retoy3(i) = 0.
        rprcy3(i) = 0.
        rprc2y3(i) = 0.
        retay3(i) = 0.
        rgwfy3(i) = 0.
        rruny3(i) = 0.
        ropwy3(i) = 0.
        rwety3(i) = 0.
        rarirr(i) = 0.
        rcumarea(i)=0.
        rcarryov(i)=0.
        itst(i)=0
        rcorr(i)=1.
      enddo
      do i=0,ncodes
         isubnum(i)=-1
         isumnum(i)=-1
         ito(i)=-1
      enddo
!
!     read subbasin list
      read(40,'(a50)')cformat
      read(40,*)
      i=1
10    read(40,cformat,end=20)isubcod(i),itomem(i),
     #rcarryov(i),csubname(i),imajcod(i),cmajname(i)
!
      isubnum(isubcod(i))=i
      lmajtst(imajcod(i))=.true.
      i = i + 1
      goto 10
20    nsub = i - 1
      close(40)
!     end reading subbasin code information
!     read summary list
      read(44,'(a50)')cformat
      read(44,*)
      i=1
30    read(44,cformat,end=40)isumcod(i),csumname(i)
      isumnum(isumcod(i))=i
      i = i + 1
      goto 30
40    nsum = i - 1
      close(44)
!     end reading summary code information
!-----------------------------------------------------------------------
!     Check subbasin code information
      do i=1,nsub
        if (itomem(i).gt.0) then
          ito(i) = isubnum(itomem(i))
        else
          ito(i) = 0
        end if
        if(ito(i).lt.0)then
          write(*,*)
     #    'To_subbasin ',itomem(i),' does not exist as subbasin'
          write(*,*)
     #    'Line: ',i+2,' file: ', cfilsubbas
          stop
        end if
      enddo
!
!-----------------------------------------------------------------------
!     Start monthly calculations
      imnth = 1
!     Open monthly files
50    open(20,file=cfilprc(imnth),status='old')
      open(21,file=cfileto(imnth),status='old')
      open(22,file=cfilprc2(imnth),status='old')
      open(23,file=cfileta(imnth),status='old')
      open(24,file=cfilflx(imnth),status='old')
      open(25,file=cfilrun(imnth),status='old')
      if(irri.eq.1)open(26,file=cfilirr(imnth),status='old')
!     initialise grids
      do j=1,ny5min
        do k=1,nx5min
           rgeto(j,k) = -9999.
           rgprc(j,k) = -9999.
           rgprc2(j,k) = -9999.
           rgetact(j,k) = -9999.
           rgetaopw(j,k) = -9999.
           rgetawet(j,k) = -9999.
           rgetact2(j,k) = -9999.
           rgflx(j,k) = -9999.
           rgrun(j,k) = -9999.
        enddo
      enddo
!
!     read data
      write(*,*)'Read data for month number:',imnth
!     skip the first 6 rows
      do i=1,6
        read(20,*)
        read(21,*)
        read(22,*)
        read(23,*)
        read(24,*)
        read(25,*)
        if(irri.eq.1)read(26,*)
      enddo
!     read monthly input grids and fill variables
      do iy=1,ny5min
        read(20,*,end=60)(rgprc(iy,ix),ix=1,nx5min)
        read(21,*)(rgeto(iy,ix),ix=1,nx5min)
        read(22,*)(rgprc2(iy,ix),ix=1,nx5min)
        read(23,*)(rgetact(iy,ix),ix=1,nx5min)
        read(24,*)(rgflx(iy,ix),ix=1,nx5min)
        read(25,*)(rgrun(iy,ix),ix=1,nx5min)
        if(irri.eq.1)read(26,*)(rgetairr(iy,ix),ix=1,nx5min)
      enddo
!
60    close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      if(irri.eq.1)close(26)
!
!     calculate open water and wetland evapotranspiration
      do iy=1,ny5min
        do ix=1,nx5min
          if ( (rgeto(iy,ix).ge.0).and.(rgprc(iy,ix).ge.0).and.
     #         (rgprc2(iy,ix).ge.0).and.(rgetact(iy,ix).ge.0).and.
     #         (rgflx(iy,ix).ge.0).and.(rgrun(iy,ix).ge.0).and.
     #         (rareaha(iy,ix).ge.0) ) then
             if((ropwat(iy,ix).gt.0).or.(rwetsol(iy,ix).gt.0)) then
!              first calculate components of the terrestrial water balance
               retterr =  rgetact(iy,ix)
               runterr = rgrun(iy,ix)
!              calculate components of the open water balance
               if(ropwat(iy,ix).gt.0) then
!                calculate open water ET
                 retow = rowkc(iy,ix) *  rgeto(iy,ix)
!                calculate vertical water balance
                 rowbal = rgprc2(iy,ix) - retow
!                if open water balance is positive 'runoff' is generated
                 if (rowbal.gt.0) then
                   runow = rowbal
                   retaow = retow
                   retaowinc = 0.
!                 if open water balance is negative extra ET is generated
                 else
                   runow = 0.
                   retaow = rgprc2(iy,ix)
                   retaowinc = -1.* rowbal
                 endif
               endif
!
!              calculate components of wetlands balance
               if(cfilowkc.eq.cfilwetkc)rwetkc(iy,ix) = rowkc(iy,ix)
               if(rwetsol(iy,ix).gt.0) then
!                calculate ET over wetlands
                 retwetl = rwetkc(iy,ix) *  rgeto(iy,ix)
!                calculate vertical water balance
                 rwetlbal = rgprc2(iy,ix) - retwetl
!                if wetland balance is positive 'runoff' is generated
                 if (rwetlbal.gt.0) then
                    runwet = rwetlbal
                    retawet = retwetl
                    retawetinc = 0.
!                if wetland balance is negative extra ET is generated
                 else
                    runwet = 0.
                    retawet = rgprc2(iy,ix)
                    retawetinc = -1 * rwetlbal
                  endif
               endif
!              calculate openwater, wetland and terrestrail fractions:
               if(rwetsol(iy,ix).lt.0.)rwetsol(iy,ix)=0.
               if(ropwat(iy,ix).lt.0.)ropwat(iy,ix)=0.
               if(rwetsol(iy,ix).gt.1.)rwetsol(iy,ix)=1.
               if(ropwat(iy,ix).gt.1.)ropwat(iy,ix)=1.
               if((rwetsol(iy,ix)+ropwat(iy,ix)).gt.1.) then
                  ropwat(iy,ix) = ropwat(iy,ix) / 2.
                  rwetsol(iy,ix) = rwetsol(iy,ix) / 2.
               endif
               rterr = 1. - (rwetsol(iy,ix)+ropwat(iy,ix))
!              correct output grids for wetlands and openwater
               rgflx(iy,ix) = rterr * rgflx(iy,ix)
               rgrun(iy,ix) = (rterr * runterr) +
     #            (ropwat(iy,ix) * runow) + (rwetsol(iy,ix) * runwet)
               rgetact(iy,ix) = (rterr * retterr) +
     #            (ropwat(iy,ix) * retaow) + (rwetsol(iy,ix) * retawet)
               rgetaopw(iy,ix) = ropwat(iy,ix) * retaowinc
               rgetawet(iy,ix) = rwetsol(iy,ix) * retawetinc
             endif
          endif
        enddo
      enddo
!     correct ET for open water, wetlands and irrigation:
      do iy=1,ny5min
        do ix=1,nx5min
          if ( (rgeto(iy,ix).ge.0).and.(rgprc(iy,ix).ge.0).and.
     #         (rgprc2(iy,ix).ge.0).and.(rgetact(iy,ix).ge.0).and.
     #         (rgflx(iy,ix).ge.0).and.(rgrun(iy,ix).ge.0).and.
     #         (rareaha(iy,ix).ge.0) ) then
            if (rgetairr(iy,ix).lt.0)rgetairr(iy,ix)=0.
            if (rgetaopw(iy,ix).lt.0)rgetaopw(iy,ix)=0.
            if (rgetawet(iy,ix).lt.0)rgetawet(iy,ix)=0.
!           adjust actual ET for irrigation
            rgetact2(iy,ix) = rgetact(iy,ix) + rgetairr(iy,ix)
!           adjust actual ET for open water
            if((rgetact(iy,ix).ge.0).and.(ropwat(iy,ix).gt.0).and.
     #      (rgeto(iy,ix).gt.0)) then
              rgetact2(iy,ix) = rgetact2(iy,ix) + rgetaopw(iy,ix)
            endif
!           adjust actual ET for wetlands
            if((rgetact(iy,ix).ge.0).and.(rwetsol(iy,ix).gt.0).and.
     #      (rgeto(iy,ix).gt.0)) then
              rgetact2(iy,ix) = rgetact2(iy,ix) + rgetawet(iy,ix)
            endif
            if(imnth .eq. 1) then
             rgetact3(iy,ix) = rgetact2(iy,ix)
             rgetirr3(iy,ix) = rgetairr(iy,ix)
             rgetopw3(iy,ix) = rgetaopw(iy,ix)
             rgetwet3(iy,ix) = rgetawet(iy,ix)
            else
             rgetact3(iy,ix) = rgetact3(iy,ix) + rgetact2(iy,ix)
             rgetirr3(iy,ix) = rgetirr3(iy,ix) + rgetairr(iy,ix)
             rgetopw3(iy,ix) = rgetopw3(iy,ix) + rgetaopw(iy,ix)
             rgetwet3(iy,ix) = rgetwet3(iy,ix) + rgetawet(iy,ix)
            endif
          endif
        enddo
      enddo
!
!     calculate subbasin sums in 10^6 m3:
      do iy=1,ny5min
        do ix=1,nx5min
          if ( (isub(iy,ix).gt.0).and.
     #         (rgeto(iy,ix).ge.0).and.(rgprc(iy,ix).ge.0).and.
     #         (rgprc2(iy,ix).ge.0).and.(rgetact(iy,ix).ge.0).and.
     #         (rgflx(iy,ix).ge.0).and.(rgrun(iy,ix).ge.0).and.
     #         (rareaha(iy,ix).ge.0) ) then
!
             if( imnth.eq.1) then
               rarea(isubnum(isub(iy,ix))) =
     #           rarea(isubnum(isub(iy,ix))) +
     #           rareaha(iy,ix)
!
               if((rarirrpct(iy,ix).gt.0).and.(irri.eq.1))
     #           rarirr(isubnum(isub(iy,ix))) =
     #           rarirr(isubnum(isub(iy,ix))) +
     #           (rarirrpct(iy,ix) / 100. * rareaha(iy,ix))
             endif
!
             reto(imnth,isubnum(isub(iy,ix))) =
     #         reto(imnth,isubnum(isub(iy,ix))) +
     #         (rgeto(iy,ix) * rareaha(iy,ix) / 100000.)
!
             rprc(imnth,isubnum(isub(iy,ix))) =
     #         rprc(imnth,isubnum(isub(iy,ix))) +
     #         (rgprc(iy,ix) * rareaha(iy,ix) / 100000.)
!
             rprc2(imnth,isubnum(isub(iy,ix))) =
     #         rprc2(imnth,isubnum(isub(iy,ix))) +
     #         (rgprc2(iy,ix) * rareaha(iy,ix) / 100000.)
!
             reta(imnth,isubnum(isub(iy,ix))) =
     #         reta(imnth,isubnum(isub(iy,ix))) +
     #         (rgetact(iy,ix) * rareaha(iy,ix) / 100000.)
!
             rgwf(imnth,isubnum(isub(iy,ix))) =
     #         rgwf(imnth,isubnum(isub(iy,ix))) +
     #         (rgflx(iy,ix) * rareaha(iy,ix) / 100000.)
!
             rrun(imnth,isubnum(isub(iy,ix))) =
     #         rrun(imnth,isubnum(isub(iy,ix))) +
     #         (rgrun(iy,ix) * rareaha(iy,ix) / 100000.)
!
             if(rgetaopw(iy,ix).gt.0)
     #         ropw(imnth,isubnum(isub(iy,ix))) =
     #         ropw(imnth,isubnum(isub(iy,ix))) +
     #         (rgetaopw(iy,ix) * rareaha(iy,ix) / 100000.)
!
             if(rgetawet(iy,ix).gt.0)
     #         rwet(imnth,isubnum(isub(iy,ix))) =
     #         rwet(imnth,isubnum(isub(iy,ix))) +
     #         (rgetawet(iy,ix) * rareaha(iy,ix) / 100000.)
!
            if((rgetairr(iy,ix).gt.0).and.(irri.eq.1))
     #         retirr(imnth,isubnum(isub(iy,ix))) =
     #         retirr(imnth,isubnum(isub(iy,ix))) +
     #         (rgetairr(iy,ix) * rareaha(iy,ix) / 100000.)
          endif
        enddo
      enddo
!
!     calculate year sums for subbasins
      do i=1,nsub
        retoy(i) = retoy(i) + reto(imnth,i)
        rprcy(i) = rprcy(i) + rprc(imnth,i)
        rprc2y(i) = rprc2y(i) + rprc2(imnth,i)
        retay(i) = retay(i) + reta(imnth,i)
        rgwfy(i) = rgwfy(i) + rgwf(imnth,i)
        rruny(i) = rruny(i) + rrun(imnth,i)
        ropwy(i) = ropwy(i) + ropw(imnth,i)
        rwety(i) = rwety(i) + rwet(imnth,i)
        if(irri.eq.1) retirry(i) = retirry(i) + retirr(imnth,i)
      enddo
!
!     calculate summary sums in 10^6 m3:
      do iy=1,ny5min
        do ix=1,nx5min
          if ( (isum(iy,ix).gt.0).and.
     #         (rgeto(iy,ix).ge.0).and.(rgprc(iy,ix).ge.0).and.
     #         (rgprc2(iy,ix).ge.0).and.(rgetact(iy,ix).ge.0).and.
     #         (rgflx(iy,ix).ge.0).and.(rgrun(iy,ix).ge.0).and.
     #         (rareaha(iy,ix).ge.0) ) then
!
             if(imnth .eq. 1) then
               rareay3(isumnum(isum(iy,ix))) =
     #           rareay3(isumnum(isum(iy,ix))) +
     #           rareaha(iy,ix)
!
               if((rarirrpct(iy,ix).gt.0).and.(irri.eq.1))
     #           rarirry3(isumnum(isum(iy,ix))) =
     #           rarirry3(isumnum(isum(iy,ix))) +
     #           (rarirrpct(iy,ix) / 100. * rareaha(iy,ix))
             endif
!
             retoy3(isumnum(isum(iy,ix))) =
     #         retoy3(isumnum(isum(iy,ix))) +
     #         (rgeto(iy,ix) * rareaha(iy,ix) / 100000.)
!
             rprcy3(isumnum(isum(iy,ix))) =
     #         rprcy3(isumnum(isum(iy,ix))) +
     #         (rgprc(iy,ix) * rareaha(iy,ix) / 100000.)
!
             rprc2y3(isumnum(isum(iy,ix))) =
     #         rprc2y3(isumnum(isum(iy,ix))) +
     #         (rgprc2(iy,ix) * rareaha(iy,ix) / 100000.)
!
             retay3(isumnum(isum(iy,ix))) =
     #         retay3(isumnum(isum(iy,ix))) +
     #         (rgetact(iy,ix) * rareaha(iy,ix) / 100000.)
!
             rgwfy3(isumnum(isum(iy,ix))) =
     #         rgwfy3(isumnum(isum(iy,ix))) +
     #         (rgflx(iy,ix) * rareaha(iy,ix) / 100000.)
!
             rruny3(isumnum(isum(iy,ix))) =
     #         rruny3(isumnum(isum(iy,ix))) +
     #         (rgrun(iy,ix) * rareaha(iy,ix) / 100000.)
!
             if(rgetaopw(iy,ix).gt.0) then
               ropwy3(isumnum(isum(iy,ix))) =
     #         ropwy3(isumnum(isum(iy,ix))) +
     #         (rgetaopw(iy,ix) * rareaha(iy,ix) / 100000.)
             endif
!
             if(rgetawet(iy,ix).gt.0) then
               rwety3(isumnum(isum(iy,ix))) =
     #         rwety3(isumnum(isum(iy,ix))) +
     #         (rgetawet(iy,ix) * rareaha(iy,ix) / 100000.)
             endif
!
             if((rgetairr(iy,ix).gt.0).and.(irri.eq.1)) then
               retirr3(imnth,isumnum(isum(iy,ix))) =
     #         retirr3(imnth,isumnum(isum(iy,ix))) +
     #         (rgetairr(iy,ix) * rareaha(iy,ix) / 100000.)
               retirr3(13,isumnum(isum(iy,ix))) =
     #         retirr3(13,isumnum(isum(iy,ix))) +
     #         (rgetairr(iy,ix) * rareaha(iy,ix) / 100000.)
             endif
!
             if((rarirrpct(iy,ix).gt.0).and.(irri.eq.1))then
               retrfirr3(imnth,isumnum(isum(iy,ix))) =
     #           retrfirr3(imnth,isumnum(isum(iy,ix))) +
     #           (rgetact(iy,ix) * (rarirrpct(iy,ix) / 100.) *
     #           rareaha(iy,ix) / 100000.)
             endif
          endif
        enddo
      enddo
!
!-----if necessary write monthly output grids for open water adjusted
!     actual evapotranspiration
      if(imnoutgrd.eq.1)then
        write(*,*)'Write monthly actual evapotranspiration grids'
        open(50,file=cfilout2(imnth),status='unknown')
        write(50,'(a23)')'ncols         4320     '
        write(50,'(a23)')'nrows         2160     '
        write(50,'(a23)')'xllcorner     -180     '
        write(50,'(a23)')'yllcorner     -90      '
        write(50,'(a23)')'cellsize      0.083333 '
        write(50,'(a23)')'NODATA_value  -9999    '
        do j=1,ny5min
          write(50,'(4320(f8.0))')(rgetact2(j,k),k=1,nx5min)
        enddo
        close(50)
      endif
!
!     re-run the program for next month
!
      imnth=imnth+1
      if (imnth.le.nmnth) goto 50

!
!-----------------------------------------------------------------------
!     store yearly data in arrays for subbasins
      do i=1,nsub
         rprc(13,i) = rprcy(i)
         reto(13,i) = retoy(i)
         rprc2(13,i) = rprc2y(i)
         reta(13,i) =  retay(i)
         ropw(13,i) =  ropwy(i)
         rwet(13,i) =  rwety(i)
         rgwf(13,i) =  rgwfy(i)
         rrun(13,i) =  rruny(i)
         if(irri.eq.1)retirr(13,i) = retirry(i)
      enddo
!
!-----------------------------------------------------------------------
!     if needed, calculate monthly accumulated runoff per subbasin
      if (iaccrun.ne.1) goto 140
!     initialise variables to calulate contributing area
      do i=1,nsub
         rcumarea(i) = rarea(i)
      enddo
!
!     find lowest ranking watersheds with help of itst,
!     if itst = 0 ranking to be found
!     if itst = 1 higher ranking than current loop
!     if itst = 2 ranking found
!
      imem = 1
90    do i=1, nsub
        if (itst(i).eq.0) then
          do j = 1, nsub
            if ((itst(j).ne.2).and.(i.eq.ito(j)))itst(i)=1
          enddo
        end if
      enddo
!
      do i=1, nsub
         if (itst(i).eq.0) then
           itst(i) = 2
!
!          initialise iteration parameter
           iter=0
!          make first estimation of average storage and yearly discharge
           rincome(13,i)= rincome(1,i) + rincome(2,i) +
     #       rincome(3,i) + rincome(4,i) + rincome(5,i) +
     #       rincome(6,i) + rincome(7,i) + rincome(8,i) +
     #       rincome(9,i) + rincome(10,i) + rincome(11,i) +
     #       rincome(12,i)
           rdistmp = rincome(13,i) + rprc2(13,i) -
     #       reta(13,i) - ropw(13,i) -  rwet(13,i) - retirr(13,i)
           rmnthavrun = max(0.,rdistmp/real(nmnth))
           rstorage(12,i) = rmnthavrun / (1.-rcarryov(i))
           rdischarge(12,i) =
     #        (1. - rcarryov(i)) * rstorage(12,i)
!
!          If internal basin, or expected yearly outflow will be below 0
!          then the yearly outflow should become equal to 0,
!          and open water and wetland evapotranspiration
!          need to be adjusted:
           if ((itomem(i).eq.-888).or.(rdistmp.lt.0.))then
             if((ropw(13,i) + rwet(13,i)).le.0.)then
               ropw(13,i)= 0.
               rwet(13,i)= 0.1
               do imnth = 1, nmnth
                 ropw(imnth,i) = 0.
                 rwet(imnth,i) = rwet(13,i) / 12.
               enddo
             endif
             rcorr(i) = (ropw(13,i) + rwet(13,i) + rdistmp) /
     #                  (ropw(13,i) + rwet(13,i))
             if(rcorr(i).lt.0) rcorr(i) = 0.
             do imnth = 1, nmnth
                ropw(imnth,i) = rcorr(i) * ropw(imnth,i)
                rwet(imnth,i) = rcorr(i) * rwet(imnth,i)
             enddo
           endif
!          calculate storage and discharge for each month:
           rdisyr = -999.
100        idismem = nint(rdisyr*10.)
           do imnth=1,nmnth
              if(imnth.eq.1)then
                rstor0 = rstorage(12,i)
                rdisch0 = rdischarge(12,i)
                rdef0 = rdeficit(12,i)
                rdisyr = 0.
              else
                rstor0 = rstorage(imnth-1,i)
                rdisch0 = rdischarge(imnth-1,i)
                rdef0 = rdeficit(imnth-1,i)
              endif
              rstorage(imnth,i) = rstor0 + rincome(imnth,i) +
     #          rprc2(imnth,i) - reta(imnth,i) -  ropw(imnth,i) -
     #          rwet(imnth,i) - retirr(imnth,i) - rdisch0 - rdef0
              if(rstorage(imnth,i).lt.0)then
                 rdeficit(imnth,i) = -1. * rstorage(imnth,i)
                 rstorage(imnth,i) = 0.
              else
                 rdeficit(imnth,i) = 0.
              endif
              rdischarge(imnth,i) =
     #           (1. - rcarryov(i)) * rstorage(imnth,i)
              rdisyr = rdisyr + rdischarge(imnth,i)
           enddo
           iter =iter+1
           if(iter.gt.100) goto 110
           if (idismem.ne.nint(rdisyr*10.)) goto 100
!
110        do imnth = 1,nmnth
             rincome(imnth,ito(i)) =
     #          rincome(imnth,ito(i)) + rdischarge(imnth,i)
           enddo
!          calculate contributing area
           rcumarea(ito(i))= rcumarea(ito(i)) + rcumarea(i)
         end if
         if (itst(i).eq.1)itst(i)=0
      enddo
!
!     loop test
      imem = imem + 1
      if(imem.gt.nsub) goto 120
      do i= 1, nsub
        if(itst(i).ne.2) goto 90
      enddo
!
!     calculate incoming flow per subbasin per year and month
120   do i=1,nsub
         rdischarge(13,i)= rdischarge(1,i) + rdischarge(2,i) +
     #     rdischarge(3,i) + rdischarge(4,i) + rdischarge(5,i) +
     #     rdischarge(6,i) + rdischarge(7,i) + rdischarge(8,i) +
     #     rdischarge(9,i) + rdischarge(10,i) + rdischarge(11,i) +
     #     rdischarge(12,i)
         rincome(13,i)= rincome(1,i) + rincome(2,i) +
     #     rincome(3,i) + rincome(4,i) + rincome(5,i) +
     #     rincome(6,i) + rincome(7,i) + rincome(8,i) +
     #     rincome(9,i) + rincome(10,i) + rincome(11,i) +
     #     rincome(12,i)
         ropw(13,i)= ropw(1,i) + ropw(2,i) +
     #     ropw(3,i) + ropw(4,i) + ropw(5,i) +
     #     ropw(6,i) + ropw(7,i) + ropw(8,i) +
     #     ropw(9,i) + ropw(10,i) + ropw(11,i) +
     #     ropw(12,i)
         rwet(13,i)= rwet(1,i) + rwet(2,i) +
     #     rwet(3,i) + rwet(4,i) + rwet(5,i) +
     #     rwet(6,i) + rwet(7,i) + rwet(8,i) +
     #     rwet(9,i) + rwet(10,i) + rwet(11,i) +
     #     rwet(12,i)
      enddo
!
!-----------------------------------------------------------------------
!     write results
140   write(*,*)'Write results'
!
      if (iaccrun.eq.1) then
!       write output for monthly monthly accumulated runoff per subbasin
        open(50,file=cfilaccrun,status='unknown')
        write(50,*)
     #    '    All areas in ha, all volumes in 10^6 cubic metre.'
        write(50,'(a18,13(8x,a4,1x),a13)')'From_code  To_code',
     #  (cmnth(i),i=1,nmnthyr),'        Area '
        do i=1, nsub
          write(50,'(2(i8,1x),14(f12.0,1x))')
     #    isubcod(i),itomem(i),
     #    (rdischarge(j,i),j=1,nmnthyr), rcumarea(i)
        enddo
        close(50)
      endif
!
!     write yearly output grid with yearly actual evaporation corrected
!     for incremental evapotranspiration of
!     irrigation, water and wetlands
      open(50,file=cfilout2yact,status='unknown')
      write(50,'(a23)')'ncols         4320     '
      write(50,'(a23)')'nrows         2160     '
      write(50,'(a23)')'xllcorner     -180     '
      write(50,'(a23)')'yllcorner     -90      '
      write(50,'(a23)')'cellsize      0.083333 '
      write(50,'(a23)')'NODATA_value  -9999    '
      do j=1,ny5min
        write(50,'(4320(f8.0))')(rgetact3(j,k),k=1,nx5min)
      enddo
      close(50)
!
!     write yearly output grid with yearly incremental
!     evapotranspiration over irrigated areas
      open(50,file=cfilout2yirr,status='unknown')
      write(50,'(a23)')'ncols         4320     '
      write(50,'(a23)')'nrows         2160     '
      write(50,'(a23)')'xllcorner     -180     '
      write(50,'(a23)')'yllcorner     -90      '
      write(50,'(a23)')'cellsize      0.083333 '
      write(50,'(a23)')'NODATA_value  -9999    '
      do j=1,ny5min
        write(50,'(4320(f8.0))')(rgetirr3(j,k),k=1,nx5min)
      enddo
      close(50)
!
!     write yearly output grid with yearly incremental
!     evapotranspiration over open water
      open(50,file=cfilout2yopw,status='unknown')
      write(50,'(a23)')'ncols         4320     '
      write(50,'(a23)')'nrows         2160     '
      write(50,'(a23)')'xllcorner     -180     '
      write(50,'(a23)')'yllcorner     -90      '
      write(50,'(a23)')'cellsize      0.083333 '
      write(50,'(a23)')'NODATA_value  -9999    '
      do j=1,ny5min
        write(50,'(4320(f8.0))')(rgetopw3(j,k),k=1,nx5min)
      enddo
      close(50)
!
!     write yearly output grid with yearly incremental
!     evapotranspiration over wetlands
      open(50,file=cfilout2ywet,status='unknown')
      write(50,'(a23)')'ncols         4320     '
      write(50,'(a23)')'nrows         2160     '
      write(50,'(a23)')'xllcorner     -180     '
      write(50,'(a23)')'yllcorner     -90      '
      write(50,'(a23)')'cellsize      0.083333 '
      write(50,'(a23)')'NODATA_value  -9999    '
      do j=1,ny5min
        write(50,'(4320(f8.0))')(rgetwet3(j,k),k=1,nx5min)
      enddo
      close(50)
!
      chead0 = '      Month'
      chead1 = 'Sub_Code'
      chead2 = ' Name Sub-basin'
      chead3 = 'Maj_Code'
      chead4 = ' Name Major Basin'
      chead5 = '       Area'
      chead6 = '    Precip.'
      chead7 = '        ETo'
      chead8 = ' Gen Precip'
      chead9 = '     ET act'
      chead10 = 'ET open wat'
      chead11 = 'ET wetlands'
      chead12 = 'Groundwater'
      chead13 = '   Drainage'
      chead14 = 'Area irrig.'
      chead15 = '  ET irrig.'
      chead16 = '  Inc. flow'
      chead17 = '    Outflow'
      chead20 = 'Sum_Code'
      chead21 = 'Summary Names       '
!
!-----Write monthly results per subbasin
      if(ioutmn.eq.1)then
        open(50,file=cfilout1mn,status='unknown')
        write(50,*)
     #  '    All areas in ha, all volumes in 10^6 cubic metre.'
        if(irri.eq.1)then
          write(50,'(a8,1x,a50,1x,a8,1x,a50,1x,14(a11,1x))')
     #    chead1,chead2,chead3,chead4,chead5,chead0,chead6,chead7,
     #    chead16,chead8,chead9,chead10,chead11,chead12,chead13,
     #    chead14,chead15,chead17
        else
          write(50,'(a8,1x,a50,1x,a8,1x,a50,1x,12(a11,1x))')
     #    chead1,chead2,chead3,chead4,chead5,chead0,chead6,chead7,
     #    chead16,chead8,chead9,chead10,chead11,chead12,chead13,
     #    chead17
        endif
!
        do i=1,nsub
          if(irri.eq.1)then
            do imnth=1,nmnth
             write(50,'(i8,1x,a50,1x,i8,1x,a50,1x,f11.0,8x,
     #                a4,1x,5(f11.0,1x),
     #                2(f11.2,1x),3(f11.0,1x),(f11.2,1x),(f11.0,1x))')
     #       isubcod(i),csubname(i),imajcod(i),cmajname(i),rarea(i),
     #       cmnth(imnth),rprc(imnth,i),reto(imnth,i),rincome(imnth,i),
     #       rprc2(imnth,i),reta(imnth,i),ropw(imnth,i),rwet(imnth,i),
     #       rgwf(imnth,i),rrun(imnth,i),rarirr(i),retirr(imnth,i),
     #       rdischarge(imnth,i)
            enddo
          else
            do imnth=1,nmnth
             write(50,'(i8,1x,a50,1x,i8,1x,a50,1x,f11.0,8x,
     #                a4,1x,5(f11.0,1x),
     #                2(f11.2,1x),3(f11.0,1x))')
     #       isubcod(i),csubname(i),imajcod(i),cmajname(i),rarea(i),
     #       cmnth(imnth),rprc(imnth,i),reto(imnth,i),rincome(imnth,i),
     #       rprc2(imnth,i),reta(imnth,i),ropw(imnth,i),rwet(imnth,i),
     #       rgwf(imnth,i),rrun(imnth,i),rdischarge(imnth,i)
            enddo
          endif
        enddo
        close(50)
      endif
!
!-----Write yearly results per subbasin
      open(50,file=cfilout1y,status='unknown')
      write(50,*)
     #  '    All areas in ha, all volumes in 10^6 cubic metre.'
      if(irri.eq.1)then
        write(50,'(a8,1x,a50,1x,a8,1x,a50,4x,13(a11,1x))')
     #  chead1,chead2,chead3,chead4,chead5,chead6,chead7,chead16,
     #  chead8,chead9,chead10,chead11,chead12,chead13,chead14,
     #  chead15,chead17
        do i=1,nsub
          write(50,'(i8,1x,a50,1x,i8,1x,a50,4x,6(f11.0,1x),
     #             2(f11.2,1x),3(f11.0,1x),(f11.2,1x),(f11.0,1x))')
     #     isubcod(i),csubname(i),imajcod(i),cmajname(i),rarea(i),
     #     rprc(imnth,i),reto(imnth,i),rincome(imnth,i),
     #     rprc2(imnth,i),reta(imnth,i),ropw(imnth,i),rwet(imnth,i),
     #     rgwf(imnth,i),rrun(imnth,i),rarirr(i),retirr(imnth,i),
     #     rdischarge(imnth,i)
        enddo
      else
        write(50,'(a8,1x,a50,1x,a8,1x,a50,4x,11(a11,1x))')
     #  chead1,chead2,chead3,chead4,chead5,chead6,chead7,chead16,
     #  chead8,chead9,chead10,chead11,chead12,chead13,chead17
        do i=1,nsub
          write(50,'(i8,1x,a50,1x,i8,1x,a50,4x,6(f11.0,1x),
     #             2(f11.2,1x),3(f11.0,1x))')
     #     isubcod(i),csubname(i),imajcod(i),cmajname(i),rarea(i),
     #     rprc(imnth,i),reto(imnth,i),rincome(imnth,i),
     #     rprc2(imnth,i),reta(imnth,i),ropw(imnth,i),rwet(imnth,i),
     #     rgwf(imnth,i),rrun(imnth,i),rdischarge(imnth,i)
        enddo
      endif
      close(50)
!
!-----results for major basins
      do i=1,nsub
!--------calculate yearly results for major basins
         rareay2(imajcod(i)) = rareay2(imajcod(i)) + rarea(i)
         rprcy2(imajcod(i)) = rprcy2(imajcod(i)) + rprc(13,i)
         retoy2(imajcod(i)) = retoy2(imajcod(i)) + reto(13,i)
         rprc2y2(imajcod(i)) = rprc2y2(imajcod(i)) + rprc2(13,i)
         retay2(imajcod(i)) = retay2(imajcod(i)) + reta(13,i)
         ropwy2(imajcod(i)) = ropwy2(imajcod(i)) + ropw(13,i)
         rwety2(imajcod(i)) = rwety2(imajcod(i)) + rwet(13,i)
         rgwfy2(imajcod(i)) = rgwfy2(imajcod(i)) + rgwf(13,i)
         rruny2(imajcod(i)) = rruny2(imajcod(i)) + rrun(13,i)
         if(irri.eq.1)then
            retirry2(imajcod(i)) = retirry2(imajcod(i)) + retirry(i)
            rarirry2(imajcod(i)) = rarirry2(imajcod(i)) + rarirr(i)
         end if
      enddo
!-----Write results per major basin
      open(55,file=cfilout1y2,status='unknown')
      write(55,*)
     #'    All areas in ha, all volumes in 10^6 cubic metre.'
      if(irri.eq.1)then
        write(55,'(a8,1x,a50,4x,11(a11,1x))')
     #  chead3,chead4,chead5,chead6,chead7,
     #  chead8,chead9,chead10,chead11,chead12,chead13,chead14,chead15
        do i=1,nsub
          if(lmajtst(imajcod(i)))
     #      write(55,'(i8,1x,a50,4x,11(f11.0,1x))')
     #      imajcod(i),cmajname(i),rareay2(imajcod(i)),
     #      rprcy2(imajcod(i)),retoy2(imajcod(i)),rprc2y2(imajcod(i)),
     #      retay2(imajcod(i)),ropwy2(imajcod(i)),rwety2(imajcod(i)),
     #      rgwfy2(imajcod(i)),rruny2(imajcod(i)),
     #      rarirry2(imajcod(i)),retirry2(imajcod(i))
          lmajtst(imajcod(i))=.false.
        enddo
      else
        write(55,'(a8,1x,a50,4x,9(a11,1x))')
     #  chead3,chead4,chead5,chead6,chead7,
     #  chead8,chead9,chead10,chead11,chead12,chead13
        do i=1,nsub
          if(lmajtst(imajcod(i)))
     #      write(55,'(i8,1x,a50,4x,9(f11.0,1x))')
     #      imajcod(i),cmajname(i),rareay2(imajcod(i)),
     #      rprcy2(imajcod(i)),retoy2(imajcod(i)),rprc2y2(imajcod(i)),
     #      retay2(imajcod(i)),ropwy2(imajcod(i)),rwety2(imajcod(i)),
     #      rgwfy2(imajcod(i)),rruny2(imajcod(i))
          lmajtst(imajcod(i))=.false.
        enddo
      endif
!
!-----Write monthly results regarding evaporation over
!     irrigated areas per summary code
      if((ioutcntmn.eq.1).and.(irri.eq.1))then
        open(58,file=cfiloutcntmn,status='unknown')
        write(58,*)
     #  '     All areas in ha, all volumes in 10^6 cubic metre.
     #                      <-------------------------------------------
     #-----------Rainfed evaporation over irrigated areas---------------
     #--------------------------------> <-------------------------------
     #-----------------------Incremental evaporation due to irrigation -
     #-------------------------------------------->'
        write(58,'(a8,1x,a60,4x,25(a11,1x))')
     #  chead20,chead21,chead14,
     #  (cmnth(i),i=1,nmnth),(cmnth(i),i=1,nmnth)
        do i=1,nsum
          write(58,'(i8,1x,a60,4x,25(f11.0,1x))')
     #    isumcod(i),csumname(i),rarirry3(i),
     #    (retrfirr3(j,i),j=1,nmnth),(retirr3(j,i),j=1,nmnth)
        enddo
        close(58)
      endif
!
!-----Write yearly results per summary code
      open(58,file=cfilout1y3,status='unknown')
      write(58,*)
     #'    All areas in ha, all volumes in 10^6 cubic metre.'
      if(irri.eq.1)then
        write(58,'(a8,1x,a60,4x,11(a11,1x))')
     #  chead20,chead21,chead5,chead6,chead7,chead8,chead9,
     #  chead10,chead11,chead12,chead13,chead14,chead15
        do i=1,nsum
          write(58,'(i8,1x,a60,4x,11(f11.0,1x))')
     #      isumcod(i),csumname(i),rareay3(i),
     #      rprcy3(i),retoy3(i),rprc2y3(i),
     #      retay3(i),ropwy3(i),rwety3(i),
     #      rgwfy3(i),rruny3(i),rarirry3(i),
     #      retirr3(13,i)
        enddo
      else
        write(58,'(a8,1x,a60,4x,9(a11,1x))')
     #  chead20,chead21,chead5,chead6,chead7,chead8,chead9,
     #  chead10,chead11,chead12,chead13
        do i=1,nsum
          write(58,'(i8,1x,a60,4x,9(f11.0,1x))')
     #      isumcod(i),csumname(i),rareay3(i),
     #      rprcy3(i),retoy3(i),rprc2y3(i),
     #      retay3(i),ropwy3(i),rwety3(i),
     #      rgwfy3(i),rruny3(i)
        enddo
      endif
!
      print *,'End of program'
      stop
      end

