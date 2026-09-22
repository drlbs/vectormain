!===============================================================================
! vector_main.f90
!
! Main program demonstrating the vector operations library.
!
! This program demonstrates:
!   - Using a module with 'use' statement
!   - Calling functions (result = function(args))
!   - Calling subroutines (call subroutine(args))
!   - Interface blocks for external functions
!   - Allocatable arrays and proper memory management
!   - Formatted output
!
! Author: Student Name
! Course: Fortran Programming
!===============================================================================

program vector_main
    !---------------------------------------------------------------------------
    ! Use the vector_ops module
    ! This gives us access to all public functions and subroutines
    ! The interface is automatic - no interface block needed!
    !---------------------------------------------------------------------------
    use vector_ops
    implicit none
    
    !---------------------------------------------------------------------------
    ! Interface block for external function
    !
    ! Since vector_sum is defined in a separate file (not in a module),
    ! we MUST provide an explicit interface so the compiler knows:
    !   - The return type (real)
    !   - The argument types (real array and integer)
    !   - The intent attributes
    !
    ! Without this interface, the compiler would have to guess, which
    ! could lead to incorrect code generation and runtime errors.
    !---------------------------------------------------------------------------
    interface
        real function vector_sum(v, n)
            integer, intent(in) :: n
            real, intent(in) :: v(n)
        end function vector_sum
    end interface
    
    !---------------------------------------------------------------------------
    ! Variable declarations
    ! Using allocatable arrays for flexibility
    !---------------------------------------------------------------------------
    real, allocatable :: vec_a(:), vec_b(:)     ! General vectors
    real, allocatable :: vec_c(:), vec_d(:)     ! For cross product demo
    real, allocatable :: result_vec(:)           ! To store results
    
    real :: magnitude_a, magnitude_b
    real :: dot_prod, angle
    real :: element_sum
    integer :: i
    
    !---------------------------------------------------------------------------
    ! Allocate 3D vectors for our demonstrations
    !---------------------------------------------------------------------------
    allocate(vec_a(3))
    allocate(vec_b(3))
    allocate(vec_c(3))
    allocate(vec_d(3))
    allocate(result_vec(3))
    
    !---------------------------------------------------------------------------
    ! Initialize test vectors
    ! vec_a = (3, 4, 0) - a classic 3-4-5 right triangle in 2D (z=0)
    ! vec_b = (1, 0, 0) - unit vector along x-axis
    !---------------------------------------------------------------------------
    vec_a = [3.0, 4.0, 0.0]
    vec_b = [1.0, 0.0, 0.0]
    
    ! Vectors for cross product demo (standard basis vectors)
    vec_c = [1.0, 0.0, 0.0]   ! i-hat (x-axis unit vector)
    vec_d = [0.0, 1.0, 0.0]   ! j-hat (y-axis unit vector)
    
    !---------------------------------------------------------------------------
    ! Print header and input vectors
    !---------------------------------------------------------------------------
    print *, ""
    print *, "=== Vector Operations Demo ==="
    print *, ""
    
    call print_vector("Vector A", vec_a)
    call print_vector("Vector B", vec_b)
    print *, ""
    
    !---------------------------------------------------------------------------
    ! DEMONSTRATE FUNCTIONS
    ! Functions return values and can be used in expressions
    !---------------------------------------------------------------------------
    print *, "FUNCTION RESULTS:"
    print *, "-----------------"
    
    ! vector_magnitude returns a real value
    magnitude_a = vector_magnitude(vec_a)
    magnitude_b = vector_magnitude(vec_b)
    print '(A, F10.4)', "  Magnitude of A:    ", magnitude_a
    print '(A, F10.4)', "  Magnitude of B:    ", magnitude_b
    
    ! dot_product_custom returns a real value
    dot_prod = dot_product_custom(vec_a, vec_b)
    print '(A, F10.4)', "  Dot product A·B:   ", dot_prod
    
    ! vector_angle returns a real value (in degrees)
    angle = vector_angle(vec_a, vec_b)
    print '(A, F10.4, A)', "  Angle between A and B: ", angle, " degrees"
    print *, ""
    
    !---------------------------------------------------------------------------
    ! DEMONSTRATE SUBROUTINES
    ! Subroutines are called with 'call' and modify their arguments
    !---------------------------------------------------------------------------
    print *, "SUBROUTINE RESULTS:"
    print *, "-------------------"
    
    ! vector_add: stores result in third argument
    call vector_add(vec_a, vec_b, result_vec)
    call print_vector("  A + B", result_vec)
    
    ! vector_scale: multiply by scalar
    call vector_scale(vec_a, 2.0, result_vec)
    call print_vector("  A × 2", result_vec)
    
    ! vector_normalize: create unit vector
    call vector_normalize(vec_a, result_vec)
    call print_vector("  Normalized A", result_vec)
    
    ! Verify the normalized vector has magnitude 1
    print '(A, F10.4)', "  (Magnitude of normalized A: ", vector_magnitude(result_vec), ")"
    print *, ""
    
    !---------------------------------------------------------------------------
    ! DEMONSTRATE CROSS PRODUCT (3D only)
    !---------------------------------------------------------------------------
    print *, "3D Cross Product Demo:"
    print *, "----------------------"
    call print_vector("  Vector C", vec_c)
    call print_vector("  Vector D", vec_d)
    
    call vector_cross_product(vec_c, vec_d, result_vec)
    call print_vector("  C × D", result_vec)
    
    ! Note: i × j = k (the z-axis unit vector)
    print *, "  (i-hat × j-hat = k-hat, as expected)"
    print *, ""
    
    !---------------------------------------------------------------------------
    ! DEMONSTRATE EXTERNAL FUNCTION WITH INTERFACE
    !---------------------------------------------------------------------------
    print *, "EXTERNAL FUNCTION (with interface):"
    print *, "------------------------------------"
    
    ! Call the external function vector_sum
    ! Note: We must pass the size explicitly because it uses explicit-shape arrays
    element_sum = vector_sum(vec_a, size(vec_a))
    print '(A, F10.4)', "  Sum of A elements: ", element_sum
    
    ! Verify: 3.0 + 4.0 + 0.0 = 7.0
    print '(A, F10.4)', "  (Expected: 3.0 + 4.0 + 0.0 = ", sum(vec_a), ")"
    print *, ""
    
    !---------------------------------------------------------------------------
    ! Clean up: Deallocate all allocatable arrays
    !
    ! This is good practice, especially for:
    !   - Large arrays
    !   - Long-running programs
    !   - Subroutines that may be called many times
    !
    ! Modern Fortran automatically deallocates local allocatable arrays
    ! when they go out of scope, but explicit deallocation is clearer.
    !---------------------------------------------------------------------------
    deallocate(vec_a)
    deallocate(vec_b)
    deallocate(vec_c)
    deallocate(vec_d)
    deallocate(result_vec)
    
    print *, "Memory deallocated successfully."
    print *, ""
    
contains

    !---------------------------------------------------------------------------
    ! print_vector: Helper subroutine to print a vector nicely
    !
    ! This is an internal subprogram (inside 'contains'), so:
    !   - No interface block is needed
    !   - It can access variables from the host program (though we don't here)
    !
    ! Arguments:
    !   label - description to print before the vector
    !   v     - the vector to print
    !---------------------------------------------------------------------------
    subroutine print_vector(label, v)
        character(len=*), intent(in) :: label
        real, intent(in) :: v(:)
        integer :: i
        
        ! Print label
        write(*, '(A)', advance='no') label // ": ["
        
        ! Print each element with formatting
        do i = 1, size(v)
            if (i > 1) write(*, '(A)', advance='no') ", "
            write(*, '(F6.2)', advance='no') v(i)
        end do
        
        ! Close bracket and newline
        print *, "]"
        
    end subroutine print_vector

end program vector_main