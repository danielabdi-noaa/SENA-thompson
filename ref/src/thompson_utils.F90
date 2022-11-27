MODULE thompson_utils

  use machine , only : kind_phys

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: print_state

CONTAINS

  !------------------------------------------------------------------
  ! print_state
  !
  ! Prints statistics for the kernel state variables
  !------------------------------------------------------------------
  SUBROUTINE print_state(msg,   &
       ext_ndiag3d,      &
       spechum,          &
       qc,               &
       qr,               &
       qi,               &
       qs,               &
       qg,               &
       ni,               &
       nr,               &
       nc,               &
       nwfa,             &
       nifa,             &
       nwfa2d,           &
       nifa2d,           &
       tgrs,             &
       prsl,             &
       phil,             &
       area,             &
       diag3d,           &
       omega,            &
       prcp,             &
       rain,             &
       graupel,          &
       ice,              &
       snow,             &
       sr,               &
       refl_10cm,        &
       phii,             &
       spp_wts_mp,       &
       spp_prt_list,     &
       spp_stddev_cutoff &
       )

    INTEGER :: i
    CHARACTER(LEN=9) :: varn

    CHARACTER(LEN=*) :: msg
    INTEGER ext_ndiag3d
    REAL(kind_phys) spechum(:,:)
    REAL(kind_phys) qc(:,:)
    REAL(kind_phys) qr(:,:)
    REAL(kind_phys) qi(:,:)
    REAL(kind_phys) qs(:,:)
    REAL(kind_phys) qg(:,:)
    REAL(kind_phys) ni(:,:)
    REAL(kind_phys) nr(:,:)
    REAL(kind_phys) nc(:,:)
    REAL(kind_phys) nwfa(:,:)
    REAL(kind_phys) nifa(:,:)
    REAL(kind_phys) nwfa2d(:)
    REAL(kind_phys) nifa2d(:)
    REAL(kind_phys) tgrs(:,:)
    REAL(kind_phys) prsl(:,:)
    REAL(kind_phys) phil(:,:)
    REAL(kind_phys) area(:)
    REAL(kind_phys) diag3d(:,:,:)
    REAL(kind_phys) omega(:,:)
    REAL(kind_phys) prcp(:)
    REAL(kind_phys) rain(:)
    REAL(kind_phys) graupel(:)
    REAL(kind_phys) ice(:)
    REAL(kind_phys) snow(:)
    REAL(kind_phys) sr(:)
    REAL(kind_phys) refl_10cm(:,:)
    REAL(kind_phys) phii(:,:)
    REAL(kind_phys) spp_wts_mp(:,:)
    REAL(kind_phys) spp_prt_list(:)
    REAL(kind_phys) spp_stddev_cutoff(:)

    WRITE(*,'(A4)') "TEST"
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("=",117)
    WRITE(*,'(A5,A32)') "TEST ", msg
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("=",117)
    WRITE(*,'(A5,A17,5A20)') "TEST ", "Variable", "Min", "Max", "First", "Last", "RMS"
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("-",117)

    CALL print_2d_variable("spechum", spechum)
    CALL print_2d_variable("qc", qc)
    CALL print_2d_variable("qr", qr)
    CALL print_2d_variable("qi", qi)
    CALL print_2d_variable("qs", qs)
    CALL print_2d_variable("qg", qg)
    CALL print_2d_variable("ni", ni)
    CALL print_2d_variable("nr", nr)
    CALL print_2d_variable("nc", nc)
    CALL print_2d_variable("nwfa", nwfa)
    CALL print_2d_variable("nifa", nifa)
    CALL print_1d_variable("nwfa2d", nwfa2d)
    CALL print_1d_variable("nifa2d", nifa2d)
    CALL print_2d_variable("tgrs", tgrs)
    CALL print_2d_variable("prsl", prsl)
    CALL print_2d_variable("phil", phil)
    CALL print_1d_variable("area", area)
    DO i = 1, ext_ndiag3d
        WRITE(varn, '(A,I0.2)') "diag3d_", i
        CALL print_2d_variable(varn, diag3d(:,:,i))
    ENDDO
    CALL print_2d_variable("omega", omega)
    CALL print_1d_variable("prcp", prcp)
    CALL print_1d_variable("rain", rain)
    CALL print_1d_variable("graupel", graupel)
    CALL print_1d_variable("ice", ice)
    CALL print_1d_variable("snow", snow)
    CALL print_1d_variable("sr", sr)
    CALL print_2d_variable("refl_10cm", refl_10cm)
    CALL print_2d_variable("phii", phii)
    CALL print_2d_variable("spp_wts_mp", spp_wts_mp)
    CALL print_1d_variable("spp_prt_list", spp_prt_list)
    CALL print_1d_variable("spp_stddev_cutoff", spp_stddev_cutoff)

    WRITE(*,'(A5,A117)') "TEST ", REPEAT("-",117)
    WRITE(*,'(A4)') "TEST"

  END SUBROUTINE print_state

  !------------------------------------------------------------------
  ! print_1d_variable
  !
  ! Prints statistics for a 1d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_1d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1), &
                            data(SIZE(data,1)),            &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_1d_variable

  !------------------------------------------------------------------
  ! print_2d_variable
  !
  ! Prints statistics for a 2d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_2d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1), &
                            data(SIZE(data,1), SIZE(data,2)),            &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_2d_variable

  !------------------------------------------------------------------
  ! print_3d_variable
  !
  ! Prints statistics for a 3d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_3d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_3d_variable

  !------------------------------------------------------------------
  ! print_4d_variable
  !
  ! Prints statistics for a 4d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_4d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_4d_variable


  !------------------------------------------------------------------
  ! print_5d_variable
  !
  ! Prints statistics for a 5d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_5d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4), SIZE(data,5)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_5d_variable

  !------------------------------------------------------------------
  ! print_1d_variable
  !
  ! Prints statistics for a 1d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_1d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    INTEGER         :: data(:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1), &
                            data(SIZE(data,1)),            &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_1d_variable_int

  !------------------------------------------------------------------
  ! print_2d_variable
  !
  ! Prints statistics for a 2d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_2d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    INTEGER         :: data(:,:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1), &
                            data(SIZE(data,1), SIZE(data,2)),            &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_2d_variable_int

  !------------------------------------------------------------------
  ! print_3d_variable
  !
  ! Prints statistics for a 3d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_3d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3)), &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_3d_variable_int

  !------------------------------------------------------------------
  ! print_4d_variable
  !
  ! Prints statistics for a 4d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_4d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4)), &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_4d_variable_int


  !------------------------------------------------------------------
  ! print_5d_variable
  !
  ! Prints statistics for a 5d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_5d_variable_int(name, data)

    CHARACTER(LEN=*) :: name
    REAL(kind_phys)         :: data(:,:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,4I20,ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4), SIZE(data,5)), &
                            SQRT(REAL(SUM(data**2) / SIZE(data)))

  END SUBROUTINE print_5d_variable_int


END MODULE thompson_utils
