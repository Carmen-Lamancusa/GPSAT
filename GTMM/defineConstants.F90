!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: defineConstants
!
! !DESCRIPTION: Define some constants for the run, including input and output
!  directories.
!\\
!\\
! !INTERFACE:
!
MODULE defineConstants
!
! !USES:
!
  IMPLICIT NONE
!
! !REVISION HISTORY:
!  09 July 2010 - C. Carouge  - Adapted to restart simulations.
!EOP
!------------------------------------------------------------------------------
!BOC
!
  integer             ::  rows=180 ! for 1x1 degree
  
  integer             ::  columns=360 ! for 1x1 degree
  
  integer             ::  NPPequilibriumYear  !# of iteration years
                                              ! necessary to get soil 
                                              ! moisture (and thus NPP and 
                                              ! the decay scalars) in
                                              ! equilibrium.  3 is ok
  
  integer             ::  HgPoolsequilibriumYear !# of iteration
                                                 ! years necessary to get Hg 
                                                 ! pools in equilibrium.  
                                                 ! ~30000 years with an 
                                                 ! f_decomp = 0.16, +161 to go
                                                 ! from preind -> ind
  integer             ::  preindYear !last preindustrial year of simulation
  integer             ::  indYear  ! last industrial year of simulation
                                   ! 161 years covering 1840 to 2000
  
  integer             ::  n_age_classes=1  !# of age classes, set to 1
                                           ! GTMM DOES NOT FULLY SUPPORT 
                                           ! AGE CLASSES
                                           ! SO LEAVE = 1
  
  real*8                ::  number_age_classes=1.000d0 ! real of above
  
  real*8                ::  Q10=1.500d0 !effect of temperature on soil fluxes
  
  real*8                ::  EMAX=0.700d0 !maximum light use efficiency
  
  real*8                ::  aboveWoodFraction=0.7500d0 ! fraction of wood that
                                                       ! is above ground
  
  real*8                ::  herbivoreEff=0.500d0 ! efficiency of herbivory
                                                 ! (part autotrophic 
                                                 ! respiration, part to
                                                 ! surface litter pools)
  
  real*8                ::  decompHgEff=0.163d0 ! fraction of organic bound
                                                ! Hg that is reduced and 
                                                ! re-emitted
  
  integer             ::  f_len=24 !length of filepath - CHANGE THIS
  
  character(len=24)   ::  filepath='/home/ccarouge/GTM/data/'
                        ! filepath for data used by GTMM model
                        ! if you modify this path, be sure to 
                        ! change the length in the character 
                        ! declaration 
  
  integer             ::  f_len_output=35 !length of the outputpath
  
  character(len=35)   ::  outputpath=&
       '/home/ccarouge/GTM/output/ifort/v2/'
  
  integer             ::  n_veg=14268 !number of vegetated pixels (non-ice)
  integer             ::  h=1
  integer             ::  yr=1
  integer             ::  mo=1
  integer             ::  age_class=1

  ! Add restart variable to know if a stand-alone run is a continuation run
  ! (restart=.true.) or a run starting from beginning (restart=.false.).
  LOGICAL             :: LRESTART
  
  CHARACTER(len=255)  :: restartfile
  
END MODULE defineConstants
!EOC
