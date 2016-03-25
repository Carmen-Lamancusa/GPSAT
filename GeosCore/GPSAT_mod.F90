!------------------------------------------------------------------------------
!       Computational Atmospheric Chemistry and Exposure (CACE) Lab           !
!------------------------------------------------------------------------------
!BOP
!
! !MODULE: GPSAT_MOD
!
! !DESCRIPTION: Module GPSAT\_MOD contains variables and routines to
! run the Flexible Source Apportionment Technology modules.
!\\
!\\
! !INTERFACE:
!
      MODULE GPSAT_MOD
!
! !USES:
!
      IMPLICIT NONE
      PRIVATE
!
! !PUBLIC DATA MEMBERS:
!
      LOGICAL, PUBLIC :: DO_SAVE_GPSAT
      INTEGER, PUBLIC :: GPSAT_NREG, GPSAT_NCUS
      INTEGER, PUBLIC, ALLOCATABLE :: GPSAT_MAP(:,:)
      INTEGER, PUBLIC, ALLOCATABLE :: GPSAT_GRID(:,:,:,:,:)
      ! GPSAT_GRID Dimensions are as follows:
      ! Region, Tracer, Lat, Lon, Vert
      INTEGER, PUBLIC, ALLOCATABLE :: GPSAT_EMISS(:,:,:,:)
      ! GPSAT_EMISS dimensions are as follows:
      ! Lat, Lon, Vert, Tracer


!
! !PUBLIC MEMBER FUNCTIONS:
!
!      PUBLIC  :: GPSAT
!      PUBLIC  :: ITS_TIME_FOR_GPSAT
      PUBLIC  :: INIT_GPSAT
!
! !PRIVATE MEMBER FUNCTIONS:
!
!      PRIVATE :: ITS_TIME_TO_CLOSE_FILE
!      PRIVATE :: GET_I
!
! !PRIVATE TYPES:
!      LOGICAL,            INTENT(IN) :: DO_GPSAT
!      INTEGER,            INTENT(IN) :: NREG, NCUS
!      CHARACTER(LEN=255), INTENT(IN) :: FNAME

      CONTAINS
!EOC

!------------------------------------------------------------------------------
!       Computational Atmospheric Chemistry and Exposure (CACE) Lab           !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: init_GPSAT
!
! !DESCRIPTION: Subroutine INIT\_GPSAT allocates and zeroes all module
!  arrays.  It also gets values for module variables from
!  "input\_mod.f".
!\\
!\\
! !INTERFACE:
!

      SUBROUTINE INIT_GPSAT( DO_GPSAT, NREG, NCUS, FNAMES, FNAMEO )
!
! !USES:
!
!      USE ERROR_MOD, ONLY : ERROR_STOP

!
! !INPUT PARAMETERS:
!
      ! DO_GPSAT : Logical switch to turn on the GPSAT tool
      ! NREG     : Number of source regions in GPSAT, from "input_mod.f"
      ! NCUS     : Number of custom sources in GPSAT, from "input_mod.f"
      ! FNAMES   : Source region map filename, from "input_mod.f"
      ! FNAMEO   : GPSAT output file name, from "input_mod.f"

      !!! Carmen Note:
      !!! May need to add frequency variable to time the outputs.

      LOGICAL,            INTENT(IN) :: DO_GPSAT
      INTEGER,            INTENT(IN) :: NREG, NCUS
      CHARACTER(LEN=255), INTENT(IN) :: FNAMES, FNAMEO
      
      CHARACTER(LEN=255) :: LOCATION
      INTEGER            :: i, j

      !=================================================================
      ! INIT_GPSAT begins here!
      !=================================================================
      
      ! Initialize
      LOCATION          = 'INIT_GPSAT ("GPSAT_mod.F90")'
      
      ! Assign variables using values from "input_mod.f"
      DO_SAVE_GPSAT     = DO_GPSAT
      GPSAT_NREG        = NREG
      GPSAT_NCUS        = NCUS
      
      ! Return if we are not saving GPSAT diagnostics
      IF ( .not. DO_SAVE_GPSAT ) RETURN

      ! Create the GPSAT_GRID using the values from "inputs_mod.f"
      ! This is placed after the DO_SAVE_GPSAT test to save memory if
      ! not using GPSAT
      ALLOCATE(GPSAT_GRID(NREG+NCUS,2,91,144,47))

      ALLOCATE(GPSAT_EMISS(91,144,47,2))

      ! Currently GPSAT is only set to run at the 2x2.5 grid resolution.
      ! This will be changed eventually to allow for use with any
      ! resolution.
      ALLOCATE(GPSAT_MAP(91,144))
     
      OPEN(UNIT=10,FILE=FNAMES,STATUS='OLD')
      READ(10,*) ((GPSAT_MAP(i,j),j=1,144),i=1,91)
      CLOSE(10)
 
      call SYSTEM('rm TEST.TXT')
     
      OPEN(UNIT=10,FILE='TEST.TXT',STATUS='NEW')
      WRITE(10, 100) ((GPSAT_MAP(i,j),j=1,144),i=1,91)
      CLOSE(10)
      
 100  FORMAT(144I5)

      END SUBROUTINE INIT_GPSAT      
!EOC

!------------------------------------------------------------------------------
!       Computational Atmospheric Chemistry and Exposure (CACE) Lab           !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: PRE_EMISS_GPSAT
!
! !DESCRIPTION: Subroutine PRE\_EMISS\_GPSAT sets the GPSAT\_MAP variable to the
!  current conentration values in the main GEOS-Chem module so that the
!  emissions can be sorted more easily. This subroutine is called in the
!  "emissions\_mod.F" file.
!\\
!\\
! !INTERFACE:
!

      SUBROUTINE PRE_EMISS_GPSAT( State_Chm )
!
! !USES:
!
!      USE ERROR_MOD, ONLY : ERROR_STOP

!
! !INPUT PARAMETERS:
!
      ! State_Chm : Logical switch to turn on the GPSAT tool
      TYPE(ChmState), INTENT(IN) :: State_Chm




      END SUBROUTINE PRE_EMISS_GPSAT

      END MODULE GPSAT_MOD






















