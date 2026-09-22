!===============================================================================
! external_func.f90
!
! An external function (not in a module) to demonstrate the use of
! interface blocks in the calling program.
!
! When a function is defined externally (not in a module or contains block),
! the calling program needs an explicit interface to know:
!   - The function's return type
!   - The number and types of arguments
!   - The intent of each argument
!
! Without an interface, the compiler cannot check for type mismatches,
! which can lead to subtle runtime bugs.
!
! Author: Student Name
! Course: Fortran Programming
!===============================================================================

!-------------------------------------------------------------------------------
! vector_sum: Calculate the sum of all elements in a vector
!
! This function uses explicit-shape array passing (the old Fortran 77 style)
! rather than assumed-shape arrays. This is to demonstrate that explicit-shape
! arrays do NOT require an interface (though having one is still recommended).
!
! Arguments:
!   v - the input vector
!   n - the number of elements in the vector
!
! Returns:
!   The sum of all elements as a real number
!
! Note: This style requires passing the size separately, unlike assumed-shape
!       arrays where you can use size(v) inside the function.
!-------------------------------------------------------------------------------
real function vector_sum(v, n)
    implicit none
    integer, intent(in) :: n        ! Size of the vector
    real, intent(in) :: v(n)        ! Explicit-shape array: size is n
    integer :: i
    real :: total
    
    ! Initialize the sum
    total = 0.0
    
    ! Sum all elements using a loop
    ! (Could also use: vector_sum = sum(v) since v is a proper array here)
    do i = 1, n
        total = total + v(i)
    end do
    
    vector_sum = total
    
end function vector_sum