!===============================================================================
! vector_module.f90
! 
! A module containing vector mathematics operations demonstrating the use of
! both functions (which return a single value) and subroutines (which modify
! arguments).
!
! This module demonstrates:
!   - Module structure with contains
!   - Public/private access control
!   - Functions that return values
!   - Subroutines that modify arguments via intent attributes
!   - Assumed-shape arrays for flexible array passing
!
! 
!===============================================================================

module vector_ops
    implicit none
    
    !---------------------------------------------------------------------------
    ! Module-level constants
    ! These are accessible to all procedures within the module
    !---------------------------------------------------------------------------
    real, parameter :: PI = 3.14159265358979
    
    !---------------------------------------------------------------------------
    ! Access control
    ! 'private' makes everything private by default
    ! 'public' explicitly exports only what users need
    !---------------------------------------------------------------------------
    private
    public :: vector_magnitude, dot_product_custom, vector_angle
    public :: vector_add, vector_scale, vector_normalize, vector_cross_product
    public :: PI  ! Export PI so main program can use it if needed
    
contains

    !===========================================================================
    ! FUNCTIONS - These return a single value and can be used in expressions
    !===========================================================================
    
    !---------------------------------------------------------------------------
    ! vector_magnitude: Calculate the magnitude (length) of a vector
    !
    ! Formula: |v| = sqrt(v1^2 + v2^2 + ... + vn^2)
    !
    ! Arguments:
    !   v - input vector (assumed-shape array, works with any size)
    !
    ! Returns:
    !   The magnitude as a real number
    !
    ! Note: Using assumed-shape array (:) requires an explicit interface,
    !       which modules provide automatically.
    !---------------------------------------------------------------------------
    real function vector_magnitude(v)
        real, intent(in) :: v(:)    ! Assumed-shape: size determined at runtime
        
        ! sum(v**2) squares each element and sums them
        ! This is a whole-array operation - very Fortran!
        vector_magnitude = sqrt(sum(v**2))
        
    end function vector_magnitude
    
    !---------------------------------------------------------------------------
    ! dot_product_custom: Calculate the dot product of two vectors
    !
    ! Formula: v1 · v2 = v1x*v2x + v1y*v2y + v1z*v2z + ...
    !
    ! Arguments:
    !   v1, v2 - input vectors (must be same size)
    !
    ! Returns:
    !   The dot product as a real number
    !
    ! Note: Named 'dot_product_custom' to avoid conflict with Fortran's
    !       intrinsic 'dot_product' function.
    !---------------------------------------------------------------------------
    real function dot_product_custom(v1, v2)
        real, intent(in) :: v1(:), v2(:)
        
        ! Verify vectors are the same size
        if (size(v1) /= size(v2)) then
            print *, "Error: Vectors must have the same size for dot product"
            dot_product_custom = 0.0
            return
        end if
        
        ! sum(v1 * v2) multiplies element-wise and sums
        ! This is equivalent to: v1(1)*v2(1) + v1(2)*v2(2) + ...
        dot_product_custom = sum(v1 * v2)
        
    end function dot_product_custom
    
    !---------------------------------------------------------------------------
    ! vector_angle: Calculate the angle between two vectors in degrees
    !
    ! Formula: theta = arccos((v1 · v2) / (|v1| * |v2|))
    !
    ! Arguments:
    !   v1, v2 - input vectors
    !
    ! Returns:
    !   The angle in degrees (0 to 180)
    !
    ! Note: Must handle the case where one or both vectors have zero magnitude
    !       to avoid division by zero.
    !---------------------------------------------------------------------------
    real function vector_angle(v1, v2)
        real, intent(in) :: v1(:), v2(:)
        real :: mag1, mag2, cos_angle
        
        ! Calculate magnitudes
        mag1 = vector_magnitude(v1)
        mag2 = vector_magnitude(v2)
        
        ! Check for zero-magnitude vectors (would cause division by zero)
        if (mag1 < 1.0e-10 .or. mag2 < 1.0e-10) then
            print *, "Warning: Cannot compute angle with zero-magnitude vector"
            vector_angle = 0.0
            return
        end if
        
        ! Calculate cosine of angle
        cos_angle = dot_product_custom(v1, v2) / (mag1 * mag2)
        
        ! Clamp to [-1, 1] to handle numerical precision issues
        ! (Sometimes floating point gives values slightly outside this range)
        if (cos_angle > 1.0) cos_angle = 1.0
        if (cos_angle < -1.0) cos_angle = -1.0
        
        ! acos returns radians, convert to degrees
        vector_angle = acos(cos_angle) * 180.0 / PI
        
    end function vector_angle
    
    !===========================================================================
    ! SUBROUTINES - These modify arguments and are called with 'call'
    !===========================================================================
    
    !---------------------------------------------------------------------------
    ! vector_add: Add two vectors element-wise
    !
    ! Formula: result = v1 + v2 (element-wise)
    !
    ! Arguments:
    !   v1, v2  - input vectors (intent(in) - read only)
    !   result  - output vector (intent(out) - will be assigned)
    !
    ! Note: This demonstrates intent(in) for inputs and intent(out) for output.
    !       The result array must already be allocated by the caller.
    !---------------------------------------------------------------------------
    subroutine vector_add(v1, v2, result)
        real, intent(in) :: v1(:), v2(:)
        real, intent(out) :: result(:)
        
        ! Verify all vectors are the same size
        if (size(v1) /= size(v2) .or. size(v1) /= size(result)) then
            print *, "Error: All vectors must have the same size for addition"
            result = 0.0
            return
        end if
        
        ! Whole-array addition - adds element by element
        result = v1 + v2
        
    end subroutine vector_add
    
    !---------------------------------------------------------------------------
    ! vector_scale: Multiply a vector by a scalar
    !
    ! Formula: result = v * scalar
    !
    ! Arguments:
    !   v       - input vector (intent(in))
    !   scalar  - multiplication factor (intent(in))
    !   result  - output vector (intent(out))
    !---------------------------------------------------------------------------
    subroutine vector_scale(v, scalar, result)
        real, intent(in) :: v(:)
        real, intent(in) :: scalar
        real, intent(out) :: result(:)
        
        if (size(v) /= size(result)) then
            print *, "Error: Input and result vectors must have the same size"
            result = 0.0
            return
        end if
        
        ! Whole-array operation: multiplies each element by scalar
        result = v * scalar
        
    end subroutine vector_scale
    
    !---------------------------------------------------------------------------
    ! vector_normalize: Create a unit vector in the same direction
    !
    ! Formula: result = v / |v|
    !
    ! A unit vector has magnitude 1.0 but points in the same direction
    ! as the original vector.
    !
    ! Arguments:
    !   v      - input vector (intent(in))
    !   result - output unit vector (intent(out))
    !---------------------------------------------------------------------------
    subroutine vector_normalize(v, result)
        real, intent(in) :: v(:)
        real, intent(out) :: result(:)
        real :: mag
        
        if (size(v) /= size(result)) then
            print *, "Error: Input and result vectors must have the same size"
            result = 0.0
            return
        end if
        
        mag = vector_magnitude(v)
        
        ! Check for zero-magnitude vector (can't normalize)
        if (mag < 1.0e-10) then
            print *, "Warning: Cannot normalize zero-magnitude vector"
            result = 0.0
            return
        end if
        
        ! Divide each component by the magnitude
        result = v / mag
        
    end subroutine vector_normalize
    
    !---------------------------------------------------------------------------
    ! vector_cross_product: Compute the cross product of two 3D vectors
    !
    ! Formula: v1 × v2 = (v1y*v2z - v1z*v2y,
    !                     v1z*v2x - v1x*v2z,
    !                     v1x*v2y - v1y*v2x)
    !
    ! Arguments:
    !   v1, v2  - input vectors (MUST be 3D)
    !   result  - output vector (MUST be 3D)
    !
    ! Note: The cross product is only defined for 3D vectors.
    !       The result is perpendicular to both input vectors.
    !---------------------------------------------------------------------------
    subroutine vector_cross_product(v1, v2, result)
        real, intent(in) :: v1(:), v2(:)
        real, intent(out) :: result(:)
        
        ! Cross product only works for 3D vectors
        if (size(v1) /= 3 .or. size(v2) /= 3 .or. size(result) /= 3) then
            print *, "Error: Cross product requires exactly 3D vectors"
            result = 0.0
            return
        end if
        
        ! Apply the cross product formula
        ! result_x = v1_y * v2_z - v1_z * v2_y
        result(1) = v1(2) * v2(3) - v1(3) * v2(2)
        
        ! result_y = v1_z * v2_x - v1_x * v2_z
        result(2) = v1(3) * v2(1) - v1(1) * v2(3)
        
        ! result_z = v1_x * v2_y - v1_y * v2_x
        result(3) = v1(1) * v2(2) - v1(2) * v2(1)
        
    end subroutine vector_cross_product

end module vector_ops
