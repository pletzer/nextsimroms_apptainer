PROGRAM send_recv_mpi
use mpi
implicit none

integer process_Rank, size_Of_Cluster, ierror, message_Item, status(MPI_STATUS_SIZE)

call MPI_INIT(ierror)
call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

IF(process_Rank == 0) THEN
    message_Item = 42
    print *, "Sending message containing: ", message_Item
    call mpi_send(message_Item, 1, MPI_INTEGER, 1, 123, MPI_COMM_WORLD, ierror)
ELSE IF(process_Rank == 1) THEN
    call mpi_recv(message_Item, 1, MPI_INTEGER, 0, 123, MPI_COMM_WORLD, status, ierror)
    print *, "Received message containing: ", message_Item
END IF

call MPI_FINALIZE(ierror)
END PROGRAM
