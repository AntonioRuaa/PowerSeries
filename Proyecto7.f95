! Realizar un menu donde se pueda elegir entre 10 funciones y su aproximación con su polinomio de Taylor al grado 7, mostrando la gráfica del polinomio.
Program proyecto_taylor 

    implicit none 
    real*8 :: h, x, taylor
    integer :: number , i


    100 Write(*,*) 'Escriba el numero de la funcion a graficar' 

    Write(*,*) '(1) e^x cos(x)'

    Write(*,*) '        1'
    Write(*,*) '(2) ---------'
    Write(*,*) '     1 + 9x^2'

    Write(*,*) '       x'
    Write(*,*) '(3) --------'
    Write(*,*) '     e^x -1'

    Write(*,*) '(4) e^sin(x)'

    Write(*,*) '(5) e^-x^2'

    Write(*,*) '(6) log10(1+e^x)'

    Write(*,*) '(7) ln(cosx)'

    Write(*,*) '      1'
    Write(*,*) '(8) ----- (e^x-e^-x)'
    Write(*,*) '      2'

    Write(*,*) '(9) cos^2(x)'

    Write(*,*) '(10) ln(sqrt(1+x))'

    Read(*,*) number 

    h = 0.03
    x = -3.0

    Select case (number)
        case (1)
            write(*,*) '1'
           
            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (2)
            write(*,*) '2' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (3)
            write(*,*) '3' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (4)
            write(*,*) '4' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (5)
            write(*,*) '5' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (6)
            write(*,*) '6' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (7)
            write(*,*) '7' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (8)
            write(*,*) '8' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (9)
            write(*,*) '9' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

        case (10)
            write(*,*) '10' 

            open(999, file='Funcion.txt')

            Do i=1,200

                write(999, *) x, taylor(x,number) 
                x = x+h

            End do 

            close(999) 

            call graph() 

            call system('start gnuplot -p grafica.gpl')

            print*, 'espere a que se genere la grafica y de enter para abrirla'
            read(*,*)

            call system('Curva.png')

    End select

    GO TO 100


End program 

!Definir como funciones las 10 series análiticas de Taylor 

function F(x, number)
    implicit none
    real*8 :: x, F 
    integer :: number

    Select case (number)
    case (1)
        if ((x.lt.0.0001).and.(x.gt.-0.0001)) then 
            F = 1.0
        else
            F = exp(x)*cos(x)
        end if 
        

    case (2)
        F = 1/(1+9*x**2)

    case (3)
        if ((x.lt.0.0001).and.(x.gt.-0.0001)) then 
            F = 1.0
        else
            F = x/(exp(x)-1)
        end if 
        
    case (4)
        
        if ((x.lt.0.0001).and.(x.gt.-0.0001)) then 
            F = 1.0
        else
            F = exp(sin(x))
        end if 

    case (5)
        
        if ((x.lt.0.0001).and.(x.gt.-0.0001)) then 
            F = 1.0
        else
            F = exp(-x**2)
        end if 

    case (6)
        F = log10(1+exp(x))

    case (7)
        F = log(cos(x))

    case (8)
        F = (1.0/2)*(exp(x)-exp(-x))

    case (9)
        F = (cos(x))**2

    case (10)
        F = log((1+x)**(0.5))

    End select

    
    !f = exp(-x**3)

end function

FUNCTION Factorial(X) 
    implicit none
    integer :: X, factorial, i
    factorial = 1
    Do i = 1, X 
        factorial = factorial*i
    End do 

  RETURN
END FUNCTION 

function taylor(b, number)
    implicit none
    real*8 :: a, b, taylor, n_derivada, f
    integer :: i, factorial, number
    
    a = b-0.1 
    n_derivada = 0
    taylor = F(a, number) 


    Do i=1,6

        call deriv(i,a, n_derivada, number)
        !write(*,*) 'La derivada del termino ', i, 'es ', n_derivada
        taylor = taylor + ((n_derivada*((b-a)**i))/(factorial(i)))
        !write(*,*) 'polinomio de taylor en ', taylor
  
    End do
    Return

end function 

Subroutine deriv(n,a, n_derivada, number)
    implicit none 

    integer :: i, j
    integer :: factorial, n, number !Función factorial, orden de derivada y número de puntos
    real*8 :: h, a, z, n_derivada, f

    n_derivada = 0
    h = 0.1 
    j = n 

    Do i=0,n 
      z = (1/((2*h)**n))*((-1)**i)*(factorial(n)/(factorial(i)*factorial(n-i)))*F(a+j*h, number)
      n_derivada = n_derivada + z 
      j = j-2 
    end do

    If (ABS(n_derivada).lt.(2.0E-002)) then
        n_derivada = 0
    End if

End subroutine

! Subrutina para gráficar 

subroutine Graph() 
    open (439, file='grafica.gpl')
    write(439,*) "set terminal pngcairo enhanced font 'Verdana,12'"
    write(439,*) "set output 'Curva.png'"
    write(439,*) "set style line 1 lt 1 lw 2 pt 7 ps 1.5 lc rgb 'blue'"
    write(439,*) "set title 'Función seleccionada'"
    write(439,*) "set xlabel 'x'"
    write(439,*) "set ylabel 'Y'"
    !write(439,*) "set yrange [-1.2:1.2]"
    write(439,*) "set xrange [-3:3]"
    write(439,*) "set border linewidth 1.5"
    write(439,*) 'set xzeroaxis linetype 1 linewidth 2.5'
    write(439,*) "set yzeroaxis linetype 1 linewidth 2.5"
    write(439,*) "set grid"
    write(439,*) "plot 'Funcion.txt' \" 
    write(439,*) "with lines ls 1 lw 3 lc 'black' \"
    !write(439,*) " , 'datos2.txt' with lines ls 1 lw 3 lc 'red' "
    close(439) 
    RETURN
end subroutine Graph 