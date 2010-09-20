program readchem

  implicit none

  type element
     character(len=3) :: name
  end  type element


  type formula
     integer :: length
     type(element), dimension(1000) :: element
     integer, dimension(1000) :: quantity
  end type formula


  character (len=26), parameter :: ABC_upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character (len=26), parameter :: abc_lower = "abcdefghijklmnopqrstuvwxyz"
  character (len=10), parameter :: numbers = "0123456789"


  ! True = The Programm should be very verbous;
  ! False = normal mode
  logical, parameter :: debug = .true.   


  character(len=50) :: chemeq = "H23Cl23Cer89HO23JPIS24GZ"

  character, dimension(50,3) :: eqsymbols
  integer, dimension(50) :: eqquantity

  !  real, dimension(10) :: Faktor
  integer :: position
  integer :: numberofsymbols = 0 
  integer :: lengthofeqstring = 0

  integer :: apos, bpos

  integer :: thisquantity
  character(len=3) :: thissymbol

  integer :: step

  write(*,*) "enter chemical equation:"
  ! read(*,*) chemeq

  write(*,*) chemeq
  write(*,*) numbers(2:)

  lengthofeqstring = len(chemeq)


  ! ============================
  ! parse the equation string
  ! ============================



  position = 1 
  step = 1

  do while (position < lengthofeqstring)

     ! walk over the equation 
     if (scan(chemeq(position+1:), ABC_upper) == 0) then
        bpos = lengthofeqstring
     else
        bpos = scan(chemeq(position+1:), ABC_upper) + position
     endif

     apos = scan(chemeq(position:), ABC_upper) + position - 1


     if (debug) then
        write(*,*) " "
        write(*,*) "=====(next sequenze)=================================="
        write(*,*) "parsing this part:",  chemeq(apos:bpos-1)
     endif


     ! ===============================
     ! Search for chemical symbol
     ! ===============================
     if (scan(chemeq(apos:bpos-1), abc_lower, .true.) >0) then
        thissymbol = chemeq(apos:apos-1+(scan(chemeq(apos:bpos-1), abc_lower, .true.)))
     else 
        thissymbol = chemeq(apos:apos)
     endif


     if (debug) then
        write(*,*) "chemical symbol:", thissymbol
     endif


     ! ===============================
     ! Search for quantifier
     ! ===============================

     if (scan(chemeq(apos:bpos-1), numbers, .true.) >0) then
        read(chemeq(apos-1+(scan(chemeq(apos:bpos-1), numbers)):bpos-1 ), '(I3)' ) thisquantity
     else
        thisquantity = 1
     endif

     if (debug) then
        write(*,'(A,I4)') "quantity:", thisquantity
     endif


     eqquantity(step) = thisquantity

     position = bpos
     step = step + 1 
  end do

  write(*,*)  chemeq(1:)
  write(*,*)  eqquantity


  !  write(*,*) eqsymbols
  !  write(*,*) eqquantity(1:10)




contains



! ===============================
! FUNCTION part
! ===============================

!!$function getformula result formulastring
!!$  implicit none
!!$  character (len=79) :: formulastring
!!$
!!$  getformula = "H23Cl23Cer89HO23JPIS24GZ"
!!$
!!$end function getformula

end program readchem
