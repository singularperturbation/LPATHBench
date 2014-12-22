Module RouteTypesAndMethods 
  Implicit none

  Type :: Route
    Integer :: Dest
    Integer :: Cost
  END Type Route

  Type :: Node
    Type(Route), Dimension(:), Allocatable :: Neighbors
  End Type Node

  contains

  Subroutine add_routes_to_neighbors(nodes,length,node_counts)
    Implicit none
    Integer, Intent(in)                           :: length
    Type (Node), Dimension(length), Intent(inout) :: nodes
    Integer, Dimension(length), Intent(in)        :: node_counts

    Integer :: err,temp,node_minus_one,destination_minus_one,cost

    OPEN(UNIT=10,FILE='agraph',STATUS='OLD',ACTION='READ',IOSTAT=err)
    IF (err /= 0) THEN
      WRITE (*,*) "Error code of ",err,"in reading file."
      STOP
    END IF

    READ (10,'(I2)') temp
    temp         = 1

    READ(10,*) node_minus_one,destination_minus_one,cost
    DO WHILE (err == 0)
      ! We could do something crazier here, but luckily file is ordered
      DO temp = 1,node_counts(node_minus_one+1)
        nodes(node_minus_one+1)%Neighbors(temp)%Dest = destination_minus_one + 1
        nodes(node_minus_one+1)%Neighbors(temp)%Cost = cost   
        READ(10,*,IOSTAT=err) node_minus_one,destination_minus_one,cost
        IF (err /= 0) THEN 
          EXIT 
        END IF
      END DO

    ENDDO


  End Subroutine add_routes_to_neighbors
  
  Subroutine populate_neighbors(nodes,length,node_counts)
    Implicit none
    Integer, Intent(in) :: length
    Type (Node), Intent(inout), Dimension(length) :: nodes 
    Integer :: I=0,err,node_minus_one,neighbor,cost
    Integer, Dimension(1:length), Intent(out) :: node_counts
    ! Initialize all of node_counts to zero
    node_counts = 0
    OPEN(UNIT=10,FILE='agraph',STATUS='OLD',ACTION='READ',IOSTAT=err)
    IF (err /= 0) THEN
      WRITE (*,*) "Err of ",err," in reading the file."
      STOP
    END IF

    READ (10,'(I2)') I

    READ(10,*,IOSTAT=err) node_minus_one,neighbor,cost


    DO WHILE (err == 0)
      node_counts(node_minus_one+1) = node_counts(node_minus_one+1)+1
      READ(10,*,IOSTAT=err) node_minus_one,neighbor,cost
    END DO

    AllocateNeighbors: DO I=1,length
      Allocate(nodes(I)%Neighbors(node_counts(I)))
      nodes(I)%Neighbors%Dest = 0
      nodes(I)%Neighbors%Cost = 0
    END DO AllocateNeighbors
    CLOSE(10)
  End Subroutine populate_neighbors

  Subroutine populate_node_array(nodes,total_node_count)
    Implicit none
    Type (Node), Intent(out), Dimension(:), Allocatable  :: nodes
    Integer,     Intent(out)                             :: total_node_count
    Call get_total_nodes(total_node_count)
    If (total_node_count == -1) Then
      WRITE (*,*) "Error reading file - recompile to get error message"
      STOP
    END IF
    ALLOCATE(nodes(total_node_count))
  End Subroutine populate_node_array

  Subroutine get_total_nodes(node_count)
    Implicit none
    Integer, intent(out) :: node_count
    Integer :: err
    OPEN(UNIT=10,FILE='agraph',STATUS='OLD',ACTION='READ',IOSTAT=err)
    READ(10,'(I2)') node_count
    IF (err /= 0) THEN
      node_count = -1
    END IF
    CLOSE(10)
  End Subroutine get_total_nodes
End Module RouteTypesAndMethods

PROGRAM LONGESTROUTE
  Use RouteTypesAndMethods
  Implicit None
  Integer :: temp_node,temp_neighbor,temp_cost,total_lines=0,total_node_count=0
  Integer, Dimension(:), Allocatable :: node_lengths
  Integer :: err=0,I=0,J=0
  Type (Node), Dimension(:), Allocatable :: Nodes

  ! Read first line and allocate nodes
  Call populate_node_array(Nodes,total_node_count)
  Allocate(node_lengths(total_node_count))
  node_lengths = 0
  ! Read rest of file and allocate lengths for each node's neighbor array
  Call populate_neighbors(Nodes,total_node_count,node_lengths)
  Call add_routes_to_neighbors(Nodes,total_node_count,node_lengths)
  ! Debugging
  DO I=1,total_node_count
    WRITE (*,*) "NODE: ",I,"has",node_lengths(I),"length."
    DO J=1,node_lengths(I)
      WRITE(*,*) "    ","Destination: ",Nodes(I)%Neighbors(J)%Dest
      WRITE(*,*) "    ","Cost:        ",Nodes(I)%Neighbors(J)%Cost
    END DO
  END DO

END PROGRAM LONGESTROUTE
