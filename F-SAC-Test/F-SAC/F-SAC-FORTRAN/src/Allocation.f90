! Copyright (c) 2011-2012, Federal University of Rio Grande do Sul
! All rights reserved.
!
! This software is subject to a BSD License, please see the License.txt
! file for more information.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED.
!
! Authors: Luiz Felipe Kusler Possani
!          Rafael de Pelegrini Soares
!
! This is a demonstration code, for more efficient implementations
! please contact rafael@enq.ufrgs.br.
!
! PLEASE CITE AS Soares and Gerber (2013), Ind. Eng. Chem. Res. DOI:10.1021/ie400170a.
!

module variables
! This module contains the variables that are shared by the subroutines used to compute
! the activity coefficient according to the F-SAC model.
!
    REAL,PARAMETER:: EO = 2.395*10.0**(-4), RGAS = 0.001987, rav = 1.07, p = 0.75
    REAL,PARAMETER:: R = 66.69, Q = 50, PI = 3.14159265359

    REAL :: FPOL, ALPHA, ALPHAPRIME, SYSTEMP
    REAL :: SUMMATION
    REAL :: AEFFPRIME, DENOM

    INTEGER :: I, J, K, L, COMPSEG, COMP, n, ii, mm, nn ! Auxiliary variables

    REAL, DIMENSION(:), ALLOCATABLE :: RCOSMO, QCOSMO, RNORM, QNORM, LNGAMMACOMB
    REAL, DIMENSION(:), ALLOCATABLE :: FRAC, NUMER
    REAL, DIMENSION(:,:), ALLOCATABLE :: CONVERG, CONPR
    REAL, DIMENSION(:,:), ALLOCATABLE :: SEGGAMMAPR, SEGGAMMAOLDPR, SEGGAMMA, SEGGAMMAOLD, PROFILEMATRIX
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: SigmaComp, SIGMA
    REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: DELTAW, deltaW_HB

    CHARACTER(25), DIMENSION(:), ALLOCATABLE :: COMPOUNDS

end module variables

subroutine allocation(SYSCOMP,compSize)
! This routine allocates all the vectors and matrices and sets the variables contained within module "variables".
! It must be called only once in the program and before the routine "activity".
!
! DO NOT FORGET to deallocate all vectors and matrices in the end of the main program using the command "STOP".
!
    use variables
    implicit none

    character(25), dimension(compSize), intent(in) :: SYSCOMP
    integer, intent(in) :: compSize

    ! Sets constants
    COMPSEG = 51 ! NUMBER OF INTERVALS FOR THE SIGMA PROFILE
    COMP = compSize ! Number os components
    FPOL = 1

    AEFFPRIME = PI*rav**2
    ALPHA = (0.3*AEFFPRIME**(1.5))/(EO)
    ALPHAPRIME = FPOL*ALPHA

    ALLOCATE(FRAC(COMP),RCOSMO(COMP), QCOSMO(COMP), RNORM(COMP), QNORM(COMP), LNGAMMACOMB(COMP))
    ALLOCATE(PROFILEMATRIX(21,COMP), SEGGAMMA(21,COMP), SEGGAMMAOLD(21,COMP), SigmaComp(21,4,COMP))
    ALLOCATE(SIGMA(COMPSEG,4,COMP), NUMER(COMPSEG),CONVERG(21, COMP), SEGGAMMAPR(21,COMP), SEGGAMMAOLDPR(21,COMP), CONPR(21,COMP))
    ALLOCATE(DELTAW(COMP, COMP, 21, 21), deltaW_HB(COMP, COMP, 21, 21))
    ALLOCATE(COMPOUNDS(compSize))

    COMPOUNDS = SYSCOMP

    SigmaComp = 0 ! Starts SigmaComp

    !READS INDIVIDUAL SIGMA PROFILES
    DO K = 1, COMP
        CALL Comps(SYSCOMP(K), SIGMA(:,:,K), RCOSMO(K), QCOSMO(K))
    END DO

    call setSigmaComp
    call calcDeltaW

end subroutine allocation

subroutine setSigmaComp
! This routine sets the sigmaComp matrix
!
    use variables
    implicit none

    DO I = 1, COMP
        mm = 0
        DO K = 1,COMPSEG
            IF(SIGMA(K,2,I) /= 0) THEN
                mm = mm + 1
                IF(SIGMA(K,3,I) == 0) THEN
                    SigmaComp(mm,1,I) = SIGMA(K,1,I)
                    SigmaComp(mm,2,I) = SIGMA(K,2,I)
                    SigmaComp(mm,3,I) = 0
                    SigmaComp(mm,4,I) = SIGMA(K,4,I)
                    ELSE
                    SigmaComp(mm,1,I) = SIGMA(K,1,I)
                    SigmaComp(mm,2,I) = SIGMA(K,2,I) - AEFFPRIME*SIGMA(K,3,I)
                    SigmaComp(mm,3,I) = 0
                    SigmaComp(mm,4,I) = SIGMA(K,4,I)
                    mm = mm + 1
                    SigmaComp(mm,1,I) = SIGMA(K,1,I)
                    SigmaComp(mm,2,I) = AEFFPRIME*SIGMA(K,3,I)
                    SigmaComp(mm,4,I) = SIGMA(K,4,I)
                    IF(SigmaComp(mm,1,I) < 0) THEN
                        SigmaComp(mm,3,I) = 1
                        ELSE
                        SigmaComp(mm,3,I) = 2
                    ENDIF
                ENDIF
            ENDIF
        ENDDO
    ENDDO

end subroutine setSigmaComp

subroutine calcDeltaW
! This routine computes DeltaW

    use variables
    use deltaW_HB_data
    implicit none

    DELTAW = 0
    deltaW_HB = 0

    DO I = 1, COMP
        DO J = 1, COMP
            DO mm = 1, 21
                DO nn = 1, 21
                    if ((SigmaComp(mm,3,I) == 0.0).or.(SigmaComp(nn,3,J) == 0.0).or.(SigmaComp(mm,3,I) == SigmaComp(nn,3,J))) then
                        DELTAW(I,J,mm,nn) = (ALPHAPRIME/2.0)*(SigmaComp(mm,1,I) + SigmaComp(nn,1,J))**2.0
                        else
                        write(*,*) "Missing HB interaction"
                        stop
                    endif
                ENDDO
            ENDDO
        ENDDO
    ENDDO

end subroutine calcDeltaW
