!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: doSoilMoisture
!
! !DESCRIPTION: This subroutine ...
!\\
!\\
! !INTERFACE:
!
SUBROUTINE doSoilMoisture
!
! !USES:
!
  USE defineConstants
  USE loadCASAinput
  USE defineArrays

  IMPLICIT NONE
!
! !REVISION HISTORY:
!  09 July 2010 - C. Carouge  - Parallelization
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
  INTEGER :: i
  CHARACTER(LEN=f_len_output+4) :: filename3
  

  filename3(1:f_len_output)=outputpath
  !PLAN to have different moisture scalars for grass and trees
      
!$OMP PARALLEL        &
!$OMP DEFAULT(SHARED) 
!$OMP DO PRIVATE(i)
  !relative drying rate (rdr) algorithm
  DO i=1,n_veg
     rdr(i,1)=(1+SMparams(i,4))/((1+(SMparams(i,4)*(last_soilm(i,1)&
          /SMparams(i,2))**SMparams(i,5))))
     IF (ppt1(i,mo) .gt. PET(i,1)) THEN
        rdr(i,1)=1.00d0 ! rdr is 1 if PPT>PET
     END IF
  END DO
!$OMP END DO
!$OMP END PARALLEL
      
  IF (mo .eq. 1 .and. yr .eq. 1) THEN
     last_pack(:,1)=0.000d0
  ENDIF

!$OMP PARALLEL    &
!$OMP DEFAULT(SHARED)
!$OMP WORKSHARE 
  current_ppt(:,1)=ppt1(:,mo)
  fid(:,1)=0.0d0
  fid(:,1)=last_pack(:,1)+ppt1(:,mo) ! add this month's ppt to last
                               !month's snowpack
  WHERE (airt1(:,mo) < 0d0) 
     spack(:,1)=fid(:,1)  !snowpack gets last
                          !month's snowpack and current ppt
     current_ppt(:,1)=0.000d0 !current ppt not avail to plants
  END WHERE

  fid(:,1)=current_ppt(:,1)+last_pack(:,1)  !add last month's snowpack to this
                                            ! months ppt

  WHERE (airt1(:,mo) >= 0d0)
     current_ppt(:,1)=fid(:,1) ! current ppt increases
     spack(:,1)=0.00d0            ! snowpack is melted 
  END WHERE
      
  !begin estimating evapotranspiration and calc new soil moist

  !if pet exceeds ppt, eet is limited
  eeta(:,1)=current_ppt(:,1)+((PET(:,1)-current_ppt(:,1))*rdr(:,1))
  eetb(:,1)=current_ppt(:,1)+last_soilm(:,1)-SMparams(:,1)

  !eet is the smallest of eeta and eetb
  WHERE (eeta(:,1) > eetb(:,1))
     EET(:,1)=eetb(:,1)
  ELSEWHERE
     EET(:,1)=eeta(:,1)
  END WHERE

  !if ppt exceeds pet, eet is not limited

  WHERE (current_ppt(:,1) >= PET(:,1))
     EET(:,1)=PET(:,1)
  END WHERE

  this_soilm(:,1)=last_soilm(:,1)+current_ppt(:,1)-EET(:,1)

  WHERE (this_soilm(:,1) > SMparams(:,2)) ! soil m > runoff
     this_soilm(:,1)=SMparams(:,2)
  END WHERE
      
  
  soilm(:,1)=this_soilm(:,1)
      
  fid(:,1)=fid(:,1)*0.0d0
!  fid(:,1)=0.500d0+(0.500d0*(EET(:,1)/PET(:,1)))
      
  WHERE (PET(:,1) > 0d0)
     fid(:,1)=0.500d0+(0.500d0*(EET(:,1)/PET(:,1)))
     NPPmoist_temp(:,1)=fid(:,1)
  END WHERE
  bgratio(:,1)=(last_soilm(:,1)-SMparams(:,1))
  bgratio(:,1)=bgratio(:,1)+current_ppt(:,1)
  bgratio(:,1)=bgratio(:,1)/PET(:,1)

  
  fid(:,1)=fid(:,1)*0.0d0
!$OMP END WORKSHARE

!$OMP DO PRIVATE(i)      
  DO i=1, n_veg
     IF (bgratio(i,1) .ge. 0d0 .and. bgratio(i,1) .lt. 1d0) THEN
        fid(i,1)=0.10d0+(0.90d0*bgratio(i,1))
        bgmoist_temp(i,1)=fid(i,1)
     ELSE IF (bgratio(i,1) .ge. 1d0 .and. bgratio(i,1) .le. 2d0) THEN
        bgmoist_temp(i,1)=1.000d0
     ELSE IF (bgratio(i,1) .gt. 2d0 .and. bgratio(i,1) .lt. 30d0) THEN
        fid(i,1)=(1+1/28.000d0)-((0.5d0/28.000d0)*bgratio(i,1))
        bgmoist_temp(i,1)=fid(i,1)
     ELSE IF (bgratio(i,1) .gt. 30d0) THEN
        bgmoist_temp(i,1)=0.500d0
     ENDIF

     !set up moisture factors for NPP calculation and BG run 
     !in case PET is zero
     IF (PET(i,1) .le. 0d0) THEN
        NPPmoist_temp(i,1)=NPPmoistpret(i,1)
        bgmoist_temp(i,1)=bgmoistpret(i,1)
     ENDIF
     NPPmoist(i,mo)=NPPmoist_temp(i,1)
     bgmoist(i,mo)=bgmoist_temp(i,1)

     !set snow pack, soil moisture, bgmoistpret, NPPmoistpret
     !for next step
  
     last_pack(i,1)=spack(i,1)
     last_soilm(i,1)=soilm(i,1)
     bgmoistpret(i,1)=bgmoist(i,mo)
     NPPmoistpret(i,1)=NPPmoist(i,mo)
  END DO
!$OMP END DO
!$OMP END PARALLEL

END SUBROUTINE doSoilMoisture
!EOC
