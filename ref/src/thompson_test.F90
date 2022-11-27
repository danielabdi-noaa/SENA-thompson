program test_thompson
   USE mt19937
   USE thompson_utils
   USE machine, only: kind_phys
   USE mp_thompson
#ifdef _OPENMP
   USE omp_lib
#endif
#ifdef _OPENACC
   USE accel_lib
#endif

   IMPLICIT NONE
#ifdef MPI
   INCLUDE 'mpif.h'
#endif

   !===============================
   ! Interface variables
   integer                    :: ncol
   integer                    :: nlev
   real(kind_phys)            :: con_g, con_rd, con_eps
   logical                    :: restart
   integer                    :: imp_physics
   integer                    :: imp_physics_thompson
   ! Hydrometeors
   logical                    :: convert_dry_rho
   real(kind_phys), allocatable     :: spechum(:,:)
   real(kind_phys), allocatable     :: qc(:,:)
   real(kind_phys), allocatable     :: qr(:,:)
   real(kind_phys), allocatable     :: qi(:,:)
   real(kind_phys), allocatable     :: qs(:,:)
   real(kind_phys), allocatable     :: qg(:,:)
   real(kind_phys), allocatable     :: ni(:,:)
   real(kind_phys), allocatable     :: nr(:,:)
   ! Aerosols
   logical                    :: is_aerosol_aware
   real(kind_phys), allocatable     :: nc(:,:)
   real(kind_phys), allocatable     :: nwfa(:,:)
   real(kind_phys), allocatable     :: nifa(:,:)
   real(kind_phys), allocatable     :: nwfa2d(:)
   real(kind_phys), allocatable     :: nifa2d(:)
   ! State variables
   real(kind_phys), allocatable     :: tgrs(:,:)
   real(kind_phys), allocatable     :: prsl(:,:)
   real(kind_phys), allocatable     :: phil(:,:)
   real(kind_phys), allocatable     :: area(:)
   ! MPI information
   integer                    :: mpicomm
   integer                    :: mpirank
   integer                    :: mpiroot
   integer                    :: mpisize
   ! Threading/blocking information
   integer                    :: threads
   ! Extended diagnostics
   logical                    :: ext_diag
   real(kind_phys), allocatable     :: diag3d(:,:,:)
   ! CCPP error handling
   character(len=256)           :: errmsg
   integer                    :: errflg
   !===============================
   ! Aerosols
   logical                     :: reset_dBZ
   logical                     :: aero_ind_fdb
   ! State variables and timestep information
   real(kind_phys), allocatable            :: omega(:,:)
   real(kind_phys)            :: dtp
   logical                    :: first_time_step
   integer                    :: istep, nsteps
   real(kind_phys)            :: dt_inner
   ! Precip/rain/snow/graupel fall amounts and fraction of frozen precip
   real(kind_phys), allocatable            :: prcp(:)
   real(kind_phys), allocatable            :: rain(:)
   real(kind_phys), allocatable            :: graupel(:)
   real(kind_phys), allocatable            :: ice(:)
   real(kind_phys), allocatable            :: snow(:)
   real(kind_phys), allocatable            :: sr(:)
   ! Radar reflectivity
   real(kind_phys), allocatable            :: refl_10cm(:,:)
   ! State variables and timestep information
   real(kind_phys), allocatable     :: phii(:,:)
   logical                    :: do_radar_ref
   logical                       :: sedi_semi
   integer                       :: decfl
   ! MPI and block information
   integer                       :: blkno
   ! SPP
   integer                   :: spp_mp
   integer                   :: n_var_spp
   real(kind_phys),           allocatable :: spp_wts_mp(:,:)
   real(kind_phys),           allocatable :: spp_prt_list(:)
   character(len=3),          allocatable :: spp_var_list(:)
   real(kind_phys),           allocatable :: spp_stddev_cutoff(:)
   ! Extended diagnostic output
   logical                  :: reset_diag3d

   !===============================
   integer, parameter :: ext_ndiag3d = 37
   !===============================

   integer :: count_rate, count_start, count_end
   real :: elapsed

   integer :: alloc_stat
   integer :: n_omp_threads, s, e, tid
   integer :: N_GPUS, gpuid
   integer, parameter :: DTEND_DIM = 12

   integer ierror
   integer :: ncol_per_thread, ncol_rem
   character(64) :: str
   !===============================
#ifdef MPI
   mpicomm = MPI_COMM_WORLD
   mpiroot = 0
   CALL MPI_INIT(ierror)
   CALL MPI_COMM_SIZE(mpicomm, mpisize, ierror)
   CALL MPI_COMM_RANK(mpicomm, mpirank, ierror)

   gpuid = mpirank
#else
   gpuid = 0
#endif
   PRINT*, 'MPI rank', mpirank

#ifdef _OPENACC
   CALL acc_set_device_num(gpuid,acc_device_nvidia)
#endif

#ifdef _OPENMP
!$omp parallel
!$omp single
   n_omp_threads = omp_get_num_threads()
!$omp end single
!$omp end parallel
#endif

#ifdef _OPENACC
   N_GPUS = 1
   n_omp_threads = N_GPUS
   CALL omp_set_num_threads(n_omp_threads)
#else
   N_GPUS = 0
#endif

#ifdef _OPENMP
   WRITE(6,'(" Using ",i3," threads")') n_omp_threads
#endif

   !===============================
   if (COMMAND_ARGUMENT_COUNT().GE.1) THEN
      CALL GET_COMMAND_ARGUMENT(1, str)
      READ(str,*) ncol
   else
      ncol = 512
   endif
   if (COMMAND_ARGUMENT_COUNT().GE.2) THEN
      CALL GET_COMMAND_ARGUMENT(2, str)
      READ(str,*) nlev
   else
      nlev = 64
   endif
   print*, ncol, nlev
   !===============================
   !===============================
   threads = n_omp_threads
   ext_diag = .TRUE.
   is_aerosol_aware = .FALSE.
   convert_dry_rho = .FALSE.
   con_g = 9.80665
   con_rd = 287.05
   con_eps = 0.6219993
   restart = .FALSE.
   imp_physics = 1
   imp_physics_thompson = 1
   !===============================
   reset_dBZ = .FALSE.
   aero_ind_fdb = .FALSE.
   first_time_step = .TRUE.
   dtp = 0.01
   istep = 1
   nsteps = 20
   dt_inner = 0.02
   do_radar_ref = .TRUE.
   sedi_semi = .FALSE.
   decfl = 1
   blkno = 1
   spp_mp = 1
   n_var_spp = 2
   reset_diag3d = .TRUE.
   !===============================

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   PRINT*, "Allocating arrays"
   ALLOCATE(                        &
       spechum(ncol,nlev),          &
       qc(ncol,nlev),               &
       qr(ncol,nlev),               &
       qi(ncol,nlev),               &
       qs(ncol,nlev),               &
       qg(ncol,nlev),               &
       ni(ncol,nlev),               &
       nr(ncol,nlev),               &
       nc(ncol,nlev),               &
       nwfa(ncol,nlev),             &
       nifa(ncol,nlev),             &
       nwfa2d(ncol),                &
       nifa2d(ncol),                &
       tgrs(ncol,nlev),             &
       prsl(ncol,nlev),             &
       phil(ncol,nlev),             &
       area(ncol),                  &
       diag3d(ncol,nlev,ext_ndiag3d), &
       omega(ncol,nlev),            &
       prcp(ncol),                  &
       rain(ncol),                  &
       graupel(ncol),               &
       ice(ncol),                   &
       snow(ncol),                  &
       sr(ncol),                    &
       refl_10cm(ncol,nlev),        &
       phii(ncol,nlev+1),             &
       spp_wts_mp(ncol,nlev),       &
       spp_prt_list(ncol),          &
       spp_var_list(ncol),          &
       spp_stddev_cutoff(ncol),     &
       STAT=alloc_stat)
   IF (alloc_stat /= 0) STOP "Error allocating arrays"

   !=============================================================
   PRINT*, "Initializing arrays"
   s = 1
   e = ncol

   CALL mt19937_real2d(spechum(s:e,:))
   spechum=spechum*0.01
   CALL mt19937_real2d(qc(s:e,:))
   CALL mt19937_real2d(qr(s:e,:))
   CALL mt19937_real2d(qi(s:e,:))
   CALL mt19937_real2d(qs(s:e,:))
   CALL mt19937_real2d(qg(s:e,:))
   CALL mt19937_real2d(ni(s:e,:))
   CALL mt19937_real2d(nr(s:e,:))
   CALL mt19937_real2d(nc(s:e,:))
   CALL mt19937_real2d(nwfa(s:e,:))
   nwfa=nwfa*1000000
   CALL mt19937_real2d(nifa(s:e,:))
   nifa=nifa*1000000
   CALL mt19937_real1d(nwfa2d(s:e))
   nwfa2d=61817
   CALL mt19937_real1d(nifa2d(s:e))
   nifa2d=0
   CALL mt19937_real2d(tgrs(s:e,:))
   tgrs=tgrs + 300.0
   CALL mt19937_real2d(prsl(s:e,:))
   prsl=prsl * 200000
   CALL mt19937_real2d(phil(s:e,:))
   phil=phil*800000
   CALL mt19937_real1d(area(s:e))
   area=2000000000.0
   CALL mt19937_real3d(diag3d(s:e,:,:))
   CALL mt19937_real2d(omega(s:e,:))
   omega=omega-1.0
   CALL mt19937_real1d(prcp(s:e))
   prcp=0
   CALL mt19937_real1d(rain(s:e))
   rain=0
   CALL mt19937_real1d(graupel(s:e))
   graupel=0
   CALL mt19937_real1d(ice(s:e))
   ice=0
   CALL mt19937_real1d(snow(s:e))
   snow=0
   CALL mt19937_real1d(sr(s:e))
   sr=0
   CALL mt19937_real2d(refl_10cm(s:e,:))
   refl_10cm=-35
   CALL mt19937_real2d(phii(s:e,:))
   phii=phii*800000
   CALL mt19937_real2d(spp_wts_mp(s:e,:))
   CALL mt19937_real1d(spp_prt_list(s:e))
   CALL mt19937_real1d(spp_stddev_cutoff(s:e))
   spp_var_list(:)=''
   !=============================================================

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   CALL SYSTEM_CLOCK (count_rate=count_rate)
   CALL SYSTEM_CLOCK (count=count_start)

#ifdef _OPENACC
   PRINT*, "Copying arrays to GPU"
   s = 1
   e = ncol
!$acc enter data copyin( spechum(s:e,:) )               
!$acc enter data copyin( qc(s:e,:) )              
!$acc enter data copyin( qr(s:e,:) )              
!$acc enter data copyin( qi(s:e,:) )              
!$acc enter data copyin( qs(s:e,:) )              
!$acc enter data copyin( qg(s:e,:) )              
!$acc enter data copyin( ni(s:e,:) )              
!$acc enter data copyin( nr(s:e,:) )              
!$acc enter data copyin( nc(s:e,:) )              
!$acc enter data copyin( nwfa(s:e,:) )              
!$acc enter data copyin( nifa(s:e,:) )              
!$acc enter data copyin( nwfa2d(s:e) )              
!$acc enter data copyin( nifa2d(s:e) )              
!$acc enter data copyin( tgrs(s:e,:) )            
!$acc enter data copyin( prsl(s:e,:) )              
!$acc enter data copyin( phil(s:e,:) )              
!$acc enter data copyin( area(s:e) )     
!$acc enter data copyin( diag3d(s:e,:,:) )                
!$acc enter data copyin( omega(s:e,:) )     
!$acc enter data copyin( prcp(s:e) )     
!$acc enter data copyin( rain(s:e) )     
!$acc enter data copyin( graupel(s:e) )     
!$acc enter data copyin( ice(s:e) )     
!$acc enter data copyin( snow(s:e) )     
!$acc enter data copyin( sr(s:e) )     
!$acc enter data copyin( refl_10cm(s:e,:) )     
!$acc enter data copyin( phii(s:e,:) )     
!$acc enter data copyin( spp_wts_mp(s:e,:) )     
!$acc enter data copyin( spp_prt_list(s:e) )     
!$acc enter data copyin( spp_var_list(s:e) )     
!$acc enter data copyin( spp_stddev_cutoff(s:e) )     
#endif

   CALL SYSTEM_CLOCK (count=count_end)
   elapsed = REAL (count_end - count_start) / REAL (count_rate)
   PRINT*, "Finished copying data in =", elapsed  
   PRINT*

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   !--- Print state
   CALL print_state("Input state",   &
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
   !-------------

   PRINT*, "Calling init"
   CALL mp_thompson_init(ncol, nlev, con_g, con_rd, con_eps,   &
                        restart, imp_physics,                 &
                        imp_physics_thompson, convert_dry_rho,&
                        spechum, qc, qr, qi, qs, qg, ni, nr,  &
                        is_aerosol_aware, nc, nwfa2d, nifa2d, &
                        nwfa, nifa, tgrs, prsl, phil, area,   &
                        mpicomm, mpirank, mpiroot,            &
                        threads, ext_diag, diag3d,            &
                        errmsg, errflg)

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   PRINT*, "Calling run"
   CALL SYSTEM_CLOCK (count_rate=count_rate)
   CALL SYSTEM_CLOCK (count=count_start)

   ncol_per_thread = (ncol / n_omp_threads)
   ncol_rem = n_omp_threads - mod(ncol, n_omp_threads)
#ifndef _OPENACC
!$omp parallel do private(tid,s,e)
#endif
   DO tid = 0, n_omp_threads - 1

       !--start and end--------
       if(tid <= ncol_rem) then
         s = tid * ncol_per_thread + 1
       else
         s = ncol_rem * ncol_per_thread + &
             (tid - ncol_rem) * (ncol_per_thread + 1) + 1
       endif
       if(tid < ncol_rem) then
         e = s + ncol_per_thread - 1
       else
         e = s + ncol_per_thread
       endif
       e = MIN(e, ncol)
       PRINT*, tid, s, ":", e, "=", e - s + 1
       !------------------------

       CALL mp_thompson_run(e-s+1, nlev, con_g, con_rd,        &
                            con_eps, convert_dry_rho,            &
                            spechum(s:e,:), qc(s:e,:), qr(s:e,:), qi(s:e,:), qs(s:e,:), qg(s:e,:), ni(s:e,:), nr(s:e,:), &
                            is_aerosol_aware, nc(s:e,:), nwfa(s:e,:), nifa(s:e,:),    &
                            nwfa2d(s:e), nifa2d(s:e), aero_ind_fdb,        &
                            tgrs(s:e,:), prsl(s:e,:), phii(s:e,:), omega(s:e,:),             &
                            sedi_semi, decfl, dtp, dt_inner,     & 
                            first_time_step, istep, nsteps,      &
                            prcp(s:e), rain(s:e), graupel(s:e), ice(s:e), snow(s:e), sr(s:e),  &
                            refl_10cm(s:e,:), reset_dBZ, do_radar_ref,  &
                            mpicomm, mpirank, mpiroot, blkno,    &
                            ext_diag, diag3d(s:e,:,:), reset_diag3d,      &
                            spp_wts_mp(s:e,:), spp_mp, n_var_spp,       &
                            spp_prt_list(s:e), spp_var_list(s:e),          &
                            spp_stddev_cutoff(s:e),                   &
                            errmsg, errflg)
   ENDDO
#ifndef _OPENACC
!$omp end parallel do
#endif

#ifdef MPI
   CALL MPI_Barrier(MPI_COMM_WORLD,ierror)
#endif

   CALL SYSTEM_CLOCK (count=count_end)
   elapsed = REAL (count_end - count_start) / REAL (count_rate)
   PRINT*
   PRINT*, "Finished executing kernel in =", elapsed  
   PRINT*

   PRINT*, "Calling finalize"
   CALL mp_thompson_finalize(errmsg, errflg)

#ifdef _OPENACC
   s = 1
   e = ncol
!$acc update self( spechum(s:e,:) )               
!$acc update self( qc(s:e,:) )              
!$acc update self( qr(s:e,:) )              
!$acc update self( qi(s:e,:) )              
!$acc update self( qs(s:e,:) )              
!$acc update self( qg(s:e,:) )              
!$acc update self( ni(s:e,:) )              
!$acc update self( nr(s:e,:) )              
!$acc update self( nc(s:e,:) )              
!$acc update self( nwfa(s:e,:) )              
!$acc update self( nifa(s:e,:) )              
!$acc update self( nwfa2d(s:e) )              
!$acc update self( nifa2d(s:e) )              
!$acc update self( tgrs(s:e,:) )            
!$acc update self( prsl(s:e,:) )              
!$acc update self( phil(s:e,:) )              
!$acc update self( area(s:e) )     
!$acc update self( diag3d(s:e,:,:) )                
!$acc update self( omega(s:e,:) )     
!$acc update self( prcp(s:e) )     
!$acc update self( rain(s:e) )     
!$acc update self( graupel(s:e) )     
!$acc update self( ice(s:e) )     
!$acc update self( snow(s:e) )     
!$acc update self( sr(s:e) )     
!$acc update self( refl_10cm(s:e,:) )     
!$acc update self( phii(s:e,:) )     
!$acc update self( spp_wts_mp(s:e,:) )     
!$acc update self( spp_prt_list(s:e) )     
!$acc update self( spp_var_list(s:e) )     
!$acc update self( spp_stddev_cutoff(s:e) )     
#endif

   !--- Print state
   CALL print_state("Output state",   &
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
   !-------------

#ifdef _OPENACC
   PRINT*, "Copying arrays to GPU"
   s = 1
   e = ncol
!$acc exit data delete( spechum(s:e,:) )               
!$acc exit data delete( qc(s:e,:) )              
!$acc exit data delete( qr(s:e,:) )              
!$acc exit data delete( qi(s:e,:) )              
!$acc exit data delete( qs(s:e,:) )              
!$acc exit data delete( qg(s:e,:) )              
!$acc exit data delete( ni(s:e,:) )              
!$acc exit data delete( nr(s:e,:) )              
!$acc exit data delete( nc(s:e,:) )              
!$acc exit data delete( nwfa(s:e,:) )              
!$acc exit data delete( nifa(s:e,:) )              
!$acc exit data delete( nwfa2d(s:e) )              
!$acc exit data delete( nifa2d(s:e) )              
!$acc exit data delete( tgrs(s:e,:) )            
!$acc exit data delete( prsl(s:e,:) )              
!$acc exit data delete( phil(s:e,:) )              
!$acc exit data delete( area(s:e) )     
!$acc exit data delete( diag3d(s:e,:,:) )                
!$acc exit data delete( omega(s:e,:) )     
!$acc exit data delete( prcp(s:e) )     
!$acc exit data delete( rain(s:e) )     
!$acc exit data delete( graupel(s:e) )     
!$acc exit data delete( ice(s:e) )     
!$acc exit data delete( snow(s:e) )     
!$acc exit data delete( sr(s:e) )     
!$acc exit data delete( refl_10cm(s:e,:) )     
!$acc exit data delete( phii(s:e,:) )     
!$acc exit data delete( spp_wts_mp(s:e,:) )     
!$acc exit data delete( spp_prt_list(s:e) )     
!$acc exit data delete( spp_var_list(s:e) )     
!$acc exit data delete( spp_stddev_cutoff(s:e) )     
#endif

end program test_thompson
