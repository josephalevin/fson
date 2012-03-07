!     
! File:   fjson.f95
! Author: josephalevin
!
! Created on March 6, 2012, 7:48 PM
!

MODULE fjson

    IMPLICIT NONE
    
    PRIVATE
    
    PUBLIC :: json_read
    
    TYPE json
        
        
    END TYPE json

    CONTAINS
    
    SUBROUTINE json_read ()
        IMPLICIT NONE
        write (*,*) "Hello, World"
    END SUBROUTINE json_read
    

END MODULE fjson

PROGRAM main
    USE fjson
    
    IMPLICIT NONE
    
    call json_read
    
    
END PROGRAM main
    
    