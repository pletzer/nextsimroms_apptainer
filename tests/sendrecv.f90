PROGRAM send_recv_mpi
include 'mpif.h'

integer process_Rank, size_Of_Cluster, ierror, message_Item

call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

IF(process_Rank == 0) THEN
    message_Item = 42
    print *, "Sending message containing: ", message_Item
ELSE IF(process_Rank == 1) THEN
    print *, "Received message containing: ", message_Item
END IF

call MPI_FINALIZE(ierror)
END PROGRAM
