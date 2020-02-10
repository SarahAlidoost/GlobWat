      program soilwatbal_1_0
!
      parameter (nmnth=12, ndaay=31, nx5min=4320, ny5min=216, 
     #nbatch = 10,
     #nadmin=10000, nluclass = 10000, nout = 5, rwetdcoef=0.5)                   !!!!!!!! I added ndaay=366
      integer:: w, j, k
      integer iluc(ny5min,nx5min),iluckc(nluclass,nmnth),
     #irkc(nadmin,nmnth),irint(nadmin,nmnth),
     #iadmin(ny5min,nx5min),mask(ny5min,nx5min)
      integer*4 itimeArray(3)    ! Holds the hour, minute, and second !!!!!!!!!!!!! I have deleted rwet(nmnth,ny5min,nx5min),rcov(nmnth,ny5min,nx5min) from real defined data 
      real rprc(ndaay,ny5min,nx5min),reto(nmnth,ny5min,nx5min),
     #rflux(ny5min,nx5min),rsmax(ny5min,nx5min),rtdpt(ny5min,nx5min),
     #rsini(ny5min,nx5min) ,rgcor(ny5min,nx5min),
     #rscor(ny5min,nx5min),tsact(ny5min,nx5min),
     #tsred(ny5min,nx5min),smetact(nmnth,ny5min,nx5min),
     #smprc(nmnth,ny5min,nx5min),smflx(nmnth,ny5min,nx5min),
     #smrun(nmnth,ny5min,nx5min),
     #smirr(nmnth,ny5min,nx5min),
     #rirriar(ny5min,nx5min)
      character*100  cfilin, cfilout, cfilout2(nout,nmnth)
      logical first
! New variables for daily counting: 
!      integer iyrday
!
!     open parameter file
      open(10,file='globwat-1-0.inp',status='old')
      read(10,*)
      read(10,*)
      read(10,*)
!     Open mask grid
      read(10,*) cfilin
      open(9,file=cfilin,status='old')
      read(10,*)
!      write(*,*) "Hello here I say cfilin: ",  cfilin
!     Open precipitation files
      do i = 101, 131 ! 11,376
        read(10,*) cfilin
        open(i,file=cfilin,status='old')
      enddo
      read(10,*)
!     Open reference evapotranspiration files
      do i = 201, 212 
        read(10,*)cfilin
        open(i,file=cfilin,status='old')
      enddo
      read(10,*)
!!     Open wet days files
!      do i = 301, 312 
!        read(10,*)cfilin
!        open(i,file=cfilin,status='old')
!      enddo
!      read(10,*)
!!     Open coefficient of variation of precipitation files
!      do i = 401, 412 
!        read(10,*) cfilin
!        open(i,file=cfilin,status='old')
!      enddo
!      read(10,*)
!     open file with maximum soil moisture
      read(10,*) cfilin
      open(70,file=cfilin,status='old') 
      read(10,*)
!     open file with initial soil moisture
      read(10,*)cfilin
      open(71,file=cfilin,status='old')
      read(10,*)
!     open file with maximum percolation flux
      read(10,*)cfilin
      open(72,file=cfilin,status='old')
      read(10,*)
!     open file with average effective rooting depth
      read(10,*)cfilin
      open(73,file=cfilin,status='old')
      read(10,*)
!     open file with land use classes
      read(10,*)cfilin
      open(74,file=cfilin,status='old')
      read(10,*)
!     open file with soil moisture correction factor
      read(10,*)cfilin
      open(75, file=cfilin,status='old')
      read(10,*)
!     open file with percolation flux correction factor
      read(10,*)cfilin
      open(76, file=cfilin,status='old')
      read(10,*)
!      open file with kc-value per land use class
      read(10,*)cfilin
      open(77,file=cfilin,status='old')
!     skip first line of kc-file
      read(77,*)
!     fill kc list per land use class per month
      do i = 1,nluclass
        do j = 1,nmnth
          iluckc(i,j) = 0
         enddo
      enddo
5     read(77,*,end=6)iluclass,(iluckc(iluclass,imnth),imnth=1,nmnth)
      goto 5
6     read(10,*)
!     read number of iterations to reach equilibrium
      read(10,*)iter
      read(10,*)
!     if random generator needs to be different every time the program is run,
!     the variable irand should be 1
      read(10,*)irand
      read(10,*)
!!     reset random generator
      if (irand.eq.1) then
        call itime(itimeArray)     ! Get the current time
        i = rand ( itimeArray(1)+itimeArray(2)+itimeArray(3) )
      endif
!
!     if irrigation should be calculated the variable irri should be 1
      read(10,*)irri
      read(10,*)
      if(irri.eq.1) then
!        open grid with country codes
         read(10,*)cfilin
         open(80,file=cfilin,status='old')
         read(10,*)
!        open grid with irrigation density
         read(10,*)cfilin
         open(81,file=cfilin,status='old')
         read(10,*)
!        open file kc-values
         read(10,*)cfilin
         open(85,file=cfilin,status='old')
         read(10,*)
!        open file with irrigation intensity
         read(10,*)cfilin
         open(86,file=cfilin,status='old')
         read(10,*)
!        skip first line
         read(85,*)
         read(86,*)
!        fill kc's and irrigation intensities
         do i = 1,nadmin
           do j = 1,nmnth
             irkc(i,j) = 0
             irint(i,j) = 0
           enddo
         enddo
9        read(85,*,end=10)
     #     icnt,(irkc(icnt,imnth),imnth=1,nmnth)
         goto 9
10       read(86,*,end=15)
     #     icnt,(irint(icnt,imnth),imnth=1,nmnth)
         goto 10
      else
         do i=1,8
            read(10,*)
         enddo
      endif
!
!     fill variables for use of monthly output grids
!
15    do i = 1,nout
         do j = 1,nmnth
           cfilout2(i,j) = "zzz.zzz"
         enddo
      enddo
!
!     run the grid in equal parts to prevent memory problems
!     the following is the amount of parts in which the grid is divided
!     nbatch * my5min gives the total size of y values of the grid
      ibatch = 1
20    write(*,*)'Processing part ',ibatch,' of ',nbatch
!
!     fill the input grids
      do w=1,ndaay                     !!!!!!!!! I have added this loop 
        do j=1,ny5min
          do k=1,nx5min
            rprc(w,j,k) = -9999
          enddo
        enddo
      enddo
      do i=1,nmnth
        do j=1,ny5min
            do k=1,nx5min
              reto(i,j,k) = -9999
              !rwet(i,j,k) = -9999
              !rcov(i,j,k) = -9999
              smprc(i,j,k) = -9999
              smetact(i,j,k) = -9999
              smflx(i,j,k) = -9999
              smrun(i,j,k) = -9999
              if(irri.eq.1) then
                smirr(i,j,k) = -9999
              endif
            enddo
        enddo
      enddo
!
      do j=1,ny5min
        do k=1,nx5min
           mask(j,k) = -9999
           rsmax(j,k) = -9999
           rsini(j,k) = -9999
           rflux(j,k) = -9999
           rtdpt(j,k) = -9999
           iluc(j,k) = 100
           rscor(j,k) = -9999
           rgcor(j,k) = -9999
           tsact(j,k) = -9999
           tsred(j,k) = -9999
           if(irri.eq.1) then
             iadmin(j,k) = 0
             rirriar(j,k) = -9999
           endif
        enddo
      enddo
!
!     read climate data
      write(*,*)'Read data'
      ifilprc = 101 
!loop up for reading prc data 
      !ifileto = 23
      ifileto = 201   
!loop up for reading prc data 
      !ifilwet = 35
      !ifilcov = 47
      imnth = 1
      idaay = 1 !!!!!!!!!! I added this 
!     skip the first 6 rows
30    if (ibatch.eq.1) then
        do i=1,6
          !read(ifilprc,*)
          read(ifileto,*)
          ! read(ifilwet,*)
          ! read(ifilcov,*)
        enddo
      endif
35    if (ibatch.eq.1) then   !!! I added this if,35 and do
        do i=1,6
          read(ifilprc,*)
        enddo
      endif
      write(*,*) "Hello ifileto is:", ifileto  
      if (ifileto.LE.212) then
        write(*,*) "I am reading because:", ifileto, "is .le. 12" 
        do iy=1,ny5min
          read(ifileto,*,end=40)(reto(imnth,iy,ix),ix=1,nx5min)
        enddo
      endif
      do iy=1,ny5min
        read(ifilprc,*,end=45)(rprc(idaay,iy,ix),ix=1,nx5min)
        !read(ifileto,*)(reto(imnth,iy,ix),ix=1,nx5min)
        !read(ifilwet,*)(rwet(imnth,iy,ix),ix=1,nx5min)
        !read(ifilcov,*)(rcov(imnth,iy,ix),ix=1,nx5min)
      enddo
!     next file next month  
40    ifileto = ifileto + 1
45    ifilprc = ifilprc + 1 
      !ifilwet = ifilwet + 1
      !ifilcov = ifilcov + 1
      imnth = imnth + 1
      idaay = idaay + 1 
      if(imnth.le.12)goto 30
      if(idaay.le.31)goto 35   !!! I added 35 
!     read mask, soil moisture, initial ground water flux, rooting depth
!     and land use class files
      ifilmask = 9
      ifilsmax = 70
      ifilsini = 71
      ifilflux = 72
      ifilrtdp = 73
      ifilluc = 74
!     groundwater and soil correction files
      ifilscor = 75
      ifilgcor = 76
!     boundary and irrigation files
      if(irri.eq.1) then
        ifiladm = 80
        ifilirr = 81
      endif
!
!     skip the first 6 rows
      if (ibatch.eq.1) then
        do i=1,6
          read(ifilmask,*)
          read(ifilsmax,*)
          read(ifilsini,*)
          read(ifilflux,*)
          read(ifilrtdp,*)
          read(ifilluc,*)
          read(ifilscor,*)
          read(ifilgcor,*)
          if(irri.eq.1) then
            read(ifiladm,*)
            read(ifilirr,*)
          endif
        enddo
      endif
      do iy=1,ny5min
        read(ifilmask,*,end=50)(mask(iy,ix),ix=1,nx5min)
        read(ifilsmax,*)(rsmax(iy,ix),ix=1,nx5min)
        read(ifilsini,*)(rsini(iy,ix),ix=1,nx5min)
        read(ifilflux,*)(rflux(iy,ix),ix=1,nx5min)
        read(ifilrtdp,*)(rtdpt(iy,ix),ix=1,nx5min)
        read(ifilluc,*) (iluc(iy,ix),ix=1,nx5min)
        read(ifilscor,*)(rscor(iy,ix),ix=1,nx5min)
        read(ifilgcor,*)(rgcor(iy,ix),ix=1,nx5min)
        if(irri.eq.1) then
           read(ifiladm,*)(iadmin(iy,ix),ix=1,nx5min)
           read(ifilirr,*)(rirriar(iy,ix),ix=1,nx5min)
        endif
      enddo
!
50    write(*,*)'Perform water balance calculations'
!     calculate ET-reduction point and set initial soil moisture:
      do j=1,ny5min
        do k=1,nx5min
          if(mask(j,k).gt.-9) then
            if(rscor(j,k).gt.-99) rsmax(j,k) =
     #        rsmax(j,k) * rtdpt(j,k) * rscor(j,k)
            if(rscor(j,k).gt.-99) rsini(j,k) =
     #        rsini(j,k) * rtdpt(j,k) * rscor(j,k)
            if(rgcor(j,k).gt.-99)rflux(j,k) = rflux(j,k) * rgcor(j,k)
            tsred(j,k) = 0.5 * rsmax(j,k)
            tsact(j,k) = rsini(j,k)
          endif
        enddo
      enddo
!     do loop for amount of years to reach equilibrium
      do iyr = 1,iter
       iyrday=0
       do i=1,nmnth
!       set number of days per month
        nday=31
        if(i.eq.2)nday=28
        if((i.eq.4).or.(i.eq.6).or.(i.eq.9).or.(i.eq.11))nday=30
!
! Pablo
!      write(*,*) "I write ibatch,  ny5min, nx5min, ndaay:", ibatch,
!     $       ny5min, nx5min, ndaay 
!      write(*,*) "I say size(rprc):", size(rprc,1), size(rprc,2), 
!     $ size(rprc,3) 
!      write(*,*) "... that must be equal to:", ndaay,ny5min,nx5min 
! End Pablo
!      iyrday=1
        do j=1,ny5min
          do k=1,nx5min
            do idaay=1,ndaay
! Pablo
!      write(*,*) "I write j, k , idaay", j, k , idaay 
! End Pablo      
!            !!!!!!!!!!!!!!!!! instead of dily prc which model calculates, I added tprecip as the input data below
              tprecip = rprc(idaay,j,k)
              do iday=1,nday
!                iyrday=iyrday+1
! Pablo 
!              write(*,*) "iyard:", iyard 
! End Pablo 
!             perform calculation only if data are available
              !!!!!!!! there was a "(rprc(i,j,k).gt.-99).and. (rwet(i,j,k).gt.-99).and. (rcov(i,j,k).gt.-99)" in if which I removed from the if condition
                if((reto(i,j,k).gt.-99).and.
     #             (rsmax(j,k).gt.-99).and.(rsini(j,k).gt.-99).and.
     #             (rflux(j,k).gt.-99).and.(mask(j,k).gt.-99))then
!               initialise monthly grids
                if(iday.eq.1)then
                  smprc(i,j,k)=0            !!!!!!!!! here I is the number of month that I am not sure if I have to change it
                  smetact(i,j,k)=0
                  smflx(i,j,k)=0
                  smrun(i,j,k)=0
                  first=.true.
! HERE COMMENT
!! !                 calculate wetdays and average daily precipitation:
!!                   rwet(i,j,k)=rwet(i,j,k)*rscor(j,k)
!!                   if(rwet(i,j,k).lt.0.1)rwet(i,j,k)=0.1
!!                   if(rwet(i,j,k).gt.nday)rwet(i,j,k)=real(nday)
!!                   rainmean = (rprc(i,j,k) / rwet(i,j,k))
!! !                 calculate average chance on a wet day
!!                   rprainm = rwet(i,j,k)/ real(nday)
!! !                 calculate  chance on a wet day after a wet day
!!                   wetcorr = rwetdcoef *
!!      #                   (real(nday)-rwet(i,j,k))/real(nday)
!!                   rprainw = (1. +  wetcorr ) * rprainm
!! !                 calculate  chance on a wet day after a dry day
!!                   if (rprainm.ne.1) then
!!                      rpraind = rprainm * (1.-rprainw) / (1.-rprainm)
!!                   else
!!                      rpraind = 1. - rwetdcoef
!!                   endif
!! !                 calculate coefficient of variance
!!                   if (rcov(i,j,k).lt.1.) rcov(i,j,k) = 1.
!!                   covar = rcov(i,j,k) / 100.
!! !                 calculate variance
!!                   var = ( covar *  rainmean ) ** 2
!! !                 calculate scale factor for gamma distribution
!!                   bscale = var / rainmean
!! !                 calculate shape factor for gamma distribution
!!                   ashape = ( rainmean ** 2 ) / var
!! !                 initialise wet yesterday
!!                   if ((iyr.eq.1).and.(i.eq.1)) wetyestday = 0
!!                 endif
!! !               from here calculate daily precipitation:
!! !               fill random test variable to check if it is a rainy day
!!                 rtest1 = rand()
!!                 tprecip = 0.
!! !               initialise wet yesterday
!!                 if ( (i.eq.1).and.(iyr.eq.1).and.(iday.eq.1)) then
!!                    if (rtest1.lt.rprainm) wetyestday = 1
!!                 end if
!! !               check chances for rain
!!                 if (wetyestday.eq.1) then
!!                    rprain = rprainw
!!                 else
!!                    rprain = rpraind
!!                 endif
!!                 if (rtest1.lt.rprain) then
!! !                 it rains
!!                   tprecip = random_gamma(ashape, bscale,first)
!!                   tprecip = max (0.1,tprecip)
!!                   first = .false.
!!                   wetyestday = 1
!!                 else
!! !                 it doesn't rain
!!                   wetyestday = 0
!!                 endif
! END HERE COMMENT 
!               calculate direct surface runoff:
                surfrun = 0.05 * tprecip
!               calculate 'effective' rainfall for soil water balance
                tprecip2 = tprecip - surfrun
!               maximum ET = ET0 * kc
                retm = reto(i,j,k) *
     #          real(iluckc(iluc(j,k),i)) / 100.
! Pablo 
!       write(*,*) "Pablo says tprecip2", tprecip2, j, k , idaay
! End Pablo 
!               perform calculations
                if(tsred(j,k).gt.0)then
!                 calculate daily actual evapotranspiration:
                  if( tsact(j,k).ge.tsred(j,k) ) then

                      tetact = retm / real(nday)
                  else
                      tetact =
     #                  (tsact(j,k)/tsred(j,k)) * retm / real(nday)
                  endif
!                 calculate ground water flux:
                  if( tsact(j,k).ge.tsred(j,k) ) then
                      tgwflx = rflux(j,k) * (tsact(j,k)-tsred(j,k)) /
     #                (rsmax(j,k) - tsred(j,k))
                  else
                      tgwflx = 0.
                  endif
!                 calculate soil water balance
                  tbal = tsact(j,k)+tprecip2-tetact-tgwflx
!                 make sure water balance is not negative
!                 if so adjust out fluxes
                  if (tbal.lt.0.) then
                    tetfrac = tetact / (tetact+tgwflx)
                    tgwfrac = tgwflx / (tetact+tgwflx)
                    tetact = tetfrac * (tsact(j,k)+tprecip2)
                    tgwflx = tgwfrac * (tsact(j,k)+tprecip2)
                    tbal = tsact(j,k)+tprecip2-tetact-tgwflx
                  endif
!                 calculate runoff and soil moisture:
                  if(tbal.gt.rsmax(j,k))then
                    trunoff  = tbal - rsmax(j,k)
                    tsact(j,k) = rsmax(j,k)
                  else
                    trunoff  = 0.
                    tsact(j,k) = tbal
                  endif
                  trunoff = trunoff + surfrun
!                 sum generated precipitation
                  smprc(i,j,k) = smprc(i,j,k) + tprecip
!                 sum actual evapotranspiration
                  smetact(i,j,k) = smetact(i,j,k) + tetact
!                 sum groundwater fluxes
                  smflx(i,j,k) = smflx(i,j,k) + tgwflx
!                 sum runoff:
                  smrun(i,j,k) = smrun(i,j,k) + trunoff
                endif
               endif !356
              endif !352
            enddo            !!!!! I added this enddo to read tprcip
            enddo
            if((irri.eq.1).and.(rirriar(j,k).gt.0).and.
     #         (smetact(i,j,k).gt.0).and.(iadmin(j,k).gt.0)) then
              smirr(i,j,k)=0.
              rwatreq = reto(i,j,k)*(real(irkc(iadmin(j,k),i)) / 100.)
              rirrmm = rwatreq - smetact(i,j,k)
              if (rirrmm.gt.0.) then
                 smirr(i,j,k) =  rirrmm * (rirriar(j,k) / 100.) *
     #              (real(irint(iadmin(j,k),i)) / 100.)
              endif
            endif
          enddo
        enddo
       enddo
      enddo
!     write results
      write(*,*)'Write soil moisture at end of simulation period'
      ifil = 88
      ifil = ifil+1
      if (ibatch.eq.1) then
        read(10,*)cfilout
        open(ifil,file=cfilout,status='unknown')
        write(ifil,'(a23)')'ncols         4320     '
        write(ifil,'(a23)')'nrows         2160     '
        write(ifil,'(a23)')'xllcorner     -180     '
        write(ifil,'(a23)')'yllcorner     -90      '
        write(ifil,'(a23)')'cellsize      0.083333 '
        write(ifil,'(a23)')'NODATA_value  -9999    '
      endif
      do j=1,ny5min
         write(ifil,'(4320(f8.0))')(tsact(j,k),k=1,nx5min)
      enddo
!
      if (ibatch.eq.1) read(10,*)
!
      iout = 0
80    iout = iout + 1
      if(iout.eq.1)
     #     write(*,*)'Write monthly precipitation grids'
      if(iout.eq.2)
     #     write(*,*)'Write monthly evapotranspiration grids'
      if(iout.eq.3)
     #     write(*,*)'Write monthly ground water fux grids'
      if(iout.eq.4)
     #     write(*,*)'Write monthly drainage grids'
      if(iout.eq.5)
     #     write(*,*)'Write monthly irrigation evapotranspiration grids'
      imonth = 1
90    if (ibatch.eq.1) then
         read(10,*)cfilout
         cfilout2(iout,imonth)=cfilout
         open(99,file=cfilout2(iout,imonth),status='unknown')
         write(99,'(a23)')'ncols         4320     '
         write(99,'(a23)')'nrows         2160     '
         write(99,'(a23)')'xllcorner     -180     '
         write(99,'(a23)')'yllcorner     -90      '
         write(99,'(a23)')'cellsize      0.083333 '
         write(99,'(a23)')'NODATA_value  -9999    '
      else
         open(99,file=cfilout2(iout,imonth),status='old')
         do while (.true.)
            read(99,*,end=95)
         enddo
      endif
95    if (ibatch.gt.1)backspace(99)
      do j=1,ny5min
         if(iout.eq.1)
     #     write(99,'(4320(f8.1))')(smprc(imonth,j,k),k=1,nx5min)
         if(iout.eq.2)
     #     write(99,'(4320(f8.1))')(smetact(imonth,j,k),k=1,nx5min)
         if(iout.eq.3)
     #     write(99,'(4320(f8.1))')(smflx(imonth,j,k),k=1,nx5min)
         if(iout.eq.4)
     #     write(99,'(4320(f8.1))')(smrun(imonth,j,k),k=1,nx5min)
         if(iout.eq.5)
     #     write(99,'(4320(f8.1))')(smirr(imonth,j,k),k=1,nx5min)
      enddo
      close(99)
      imonth = imonth+1
      if (imonth.le.nmnth)goto 90
      if (ibatch.eq.1) read(10,*)
      if(iout.lt.5) goto 80
!
      ibatch = ibatch + 1
      if (ibatch.le.nbatch) goto 20
      print *,'End of program'
      stop
      end
!
!!----- routines to random numbers from the gamma distribution
!!      http://jblevins.org/mirror/amiller/
!      function random_gamma(ashape, bscale, first)
!!     Adapted from Fortran 77 code from the book:
!!     Dagpunar, J. 'Principles of random variate generation'
!!     Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
!
!!     N.B. This version is in `double precision' and includes scaling
!
!!     FUNCTION GENERATES A RANDOM GAMMA VARIATE.
!!     CALLS EITHER random_gamma1 (S > 1.0)
!!     OR random_exponential (S = 1.0)
!!     OR random_gamma2 (S < 1.0).
!
!!     ashape = SHAPE PARAMETER OF DISTRIBUTION (0 < REAL).
!!     bscale = Scale parameter
!      real random_gamma,ashape,bscale
!      logical first
!!
!      if (ashape .le. 0.) then
!          write(*,*) 'Shape parameter must be positive'
!          stop
!      endif
!      if (ashape.ge.1) then
!           fn_val = random_gamma1(ashape, first)
!      else
!           fn_val = random_gamma2(ashape, first)
!      endif
!!     Now scale the random variable
!      random_gamma = fn_val * bscale
!      return
!      end
!!
!      function random_gamma1(ashape, first)
!!     Adapted from Fortran 77 code from the book:
!!        Dagpunar, J. 'Principles of random variate generation'
!!           Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
!
!!           FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
!!           A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO GAMMA**(S-1)*EXP(-GAMMA),
!!           BASED UPON BEST'S T DISTRIBUTION METHOD
!!
!!           ashape = SHAPE PARAMETER OF DISTRIBUTION    (1.0 < REAL)
!!
!      real random_gamma1, ashape
!      logical first
!!
!      save b, h
!!
!!     Initialization
!      if (first) then
!         b = ashape - 1.
!         h = sqrt(3.0*ashape - 0.75)
!      endif
!!
!      do
!        r = rand()
!        g = r - r*r
!        if (g .lt. 0) cycle
!        f = (r - 0.5)*h/sqrt(g)
!        x = b + f
!        if (x .le. 0.) cycle
!        r = rand()
!        d = 64.*g*(r*g)**2
!        if (d .le. 0.) exit
!        if (d*x < x - 2.*f*f) exit
!        if (log(d) < 2.*(b*log(x/b) - f)) exit
!      enddo
!!
!      random_gamma1 = x
!!
!      return
!      end
!
!      function random_gamma2(ashape, first)
!
!!     Adapted from Fortran 77 code from the book:
!!           Dagpunar, J. 'Principles of random variate generation'
!!           Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
!
!!     FUNCTION GENERATES A RANDOM VARIATE IN [0,INFINITY) FROM
!!     A GAMMA DISTRIBUTION WITH DENSITY PROPORTIONAL TO
!!     GAMMA2**(S-1) * EXP(-GAMMA2),
!!     USING A SWITCHING METHOD.
!
!!     ashape = SHAPE PARAMETER OF DISTRIBUTION     (REAL < 1.0)
!
!      real random_gamma2, ashape
!      logical first
!!
!      save a, p, c, uf, vr, d
!!
!      vsmall = epsilon(1.)
!!
!!     Initialization
!      if (first) then
!         a = 1. - ashape
!         p = a/(a + ashape*exp(-a))
!         if (ashape < vsmall) ashape = vsmall
!         c = 1./ashape
!         uf = p*(vsmall/a)**ashape
!         vr = 1. - vsmall
!         d = a*log(a)
!      endif
!
!      do
!        r = rand()
!        if (r .ge. vr) then
!           cycle
!        elseif (r .gt. p) then
!           x = a - log((1. - r)/(1. - p))
!           w = a*log(x)-d
!        elseif (r .gt. uf) then
!           x = a*(r/p)**c
!           w = x
!        else
!           random_gamma2 = 0.0
!           return
!        endif
!        r = rand()
!        if(1.-r .le.w .and. r .gt. 0.) then
!          if  (r*(w + 1.) .ge. 1.) cycle
!          if(-log(r) .le. w) cycle
!        endif
!        exit
!      enddo
!!
!      random_gamma2 = x
!!
!      return
!      end
