! Hello World, Fortran style
! MPI version. This simply gets each parallel MPI task (rank) to
! independently say Hello World, and report its rank number.
program hello_world
    use mpi
    implicit none
    integer :: mpi_size, mpi_rank ! MPI world size & rank
    integer :: ierr ! Used to check for errors in MPI calls

    ! Initialize the MPI environment.
    ! This should be first thing you do in any MPI program.
    call MPI_Init(ierr)

    ! Get details about this process (mpi_rank)
    ! and the total number of processes (mpi_size)
    call MPI_Comm_size(MPI_COMM_WORLD, mpi_size, ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, mpi_rank, ierr)

    ! Embellish our original "Hello World" message to
    ! report this process' rank & size
    write(*,"(A,I0,A,I0,A)") "Hello World! (This is parallel rank ", &
        mpi_rank, " out of ", mpi_size, ")"

    ! Shut down the MPI environment. This must be done just before
    ! exiting your MPI program.
    ! (Remember also to do this if you need to exit early.)
    call MPI_Finalize(ierr)

end program hello_world
