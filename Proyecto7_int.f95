! Realizar un programa que resuelva integrales por el método de Romberg y aproximando la funcion por series de potencias 
Program Int_taylor 
implicit none

real*8, dimension(20,20) :: R
real*8 :: pi, A, B
integer :: N

! Limites de integracion
pi = 4*atan(1.0)
A= 0.5
B = 1
N=20

CALL ROMBERG(A,B,N,R) 

!write(*,*) taylor(1.0d0)
!Do i=1,6
!    n_derivada = 0
!    call deriv(i,0.1d0, n_derivada)
!    write(*,*) n_derivada
!End do 

End program 

! Definir la función a integrar 

function F(x)
    implicit none
    real*8 :: x, F

    if ((x.lt.0.0001).and.(x.gt.-0.0001)) then 
        F = 1.0
    else
        f = cos((x)**(1.0/2))
    end if 
    

end function

! Función de factorial

FUNCTION Factorial(X) 
    implicit none
    integer :: X, factorial, i
    factorial = 1
    Do i = 1, X 
        factorial = factorial*i
    End do 

  RETURN
END FUNCTION 

! Función que devuelve el resultado de la función por serie de potencias (taylor) 

function taylor(b)
    implicit none
    real*8 :: a, b, taylor, n_derivada, f
    integer :: i, factorial
    
    a = b-0.1 
    n_derivada = 0
    taylor = F(a) 


    Do i=1,4

        call deriv(i,a, n_derivada)
        !write(*,*) 'La derivada del termino ', i, 'es ', n_derivada
        taylor = taylor + ((n_derivada*((b-a)**i))/(factorial(i)))
        !write(*,*) 'polinomio de taylor en ', taylor
  
    End do
    Return

end function

! Subrutina de integral de Romberg 

SUBROUTINE ROMBERG(A,B,N,R)
    implicit none 
    REAL*8 :: R(20,20)
    real*8 :: H, A, B, taylor, S
    integer :: i,j,n, k

    WRITE(*,*)' INTEGRACION DE ROMBERG ' 
    H=B-A
    R(1,1)=(H/2.0)*(taylor(A)+taylor(B))
    !write(*,*) taylor(A)
    DO I=2,N
          S=0
          DO K=1,2**(I-2)
                S=S+taylor(A+((K-0.5)*H))
          END DO
          R(I,1)=0.5*(R(I-1,1)+(H*S))
          H=H/2.0
    END DO
    
    ! Extrapolación de Richardson
    DO J=2,N
          DO I=J,N
                R(I,J)=R(I,J-1)+((R(I,J-1)-R(I-1,J-1))/((4**(J-1)-1)))
                !write(*,*) I, J, R(I,3)
          END DO
    END DO
    
    !DO I=1,N 
    I = 6
          WRITE(*,*)(R(I,1))
    !20  FORMAT(6(2X,F12.6))
    !END DO
END SUBROUTINE

! Subrutina para derivar 

Subroutine deriv(n,a, n_derivada)
        implicit none 
  
        integer :: i, j
        integer :: factorial, n !Función factorial, orden de derivada y número de puntos
        real*8 :: h, a, z, n_derivada, f
  
        n_derivada = 0
        h = 0.1 
        j = n 
  
        Do i=0,n 
          z = (1/((2*h)**n))*((-1)**i)*(factorial(n)/(factorial(i)*factorial(n-i)))*F(a+j*h)
          n_derivada = n_derivada + z 
          j = j-2 
        end do

        If (ABS(n_derivada).lt.(2.0E-002)) then
            n_derivada = 0
        End if

End subroutine
