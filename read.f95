module globalknowledgeofletters
  character (len=26), parameter :: ABC_upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character (len=26), parameter :: abc_lower = "abcdefghijklmnopqrstuvwxyz"
  character (len=10), parameter :: numbers = "0123456789"
end module globalknowledgeofletters



module elements
  save

  type element
     integer :: atomnumber
     character(len=3) :: name
  end type element


  type formula
     integer :: length
     type(element), dimension(1000) :: element
     integer, dimension(1000) :: quantity
  end type formula
 
  
end module elements


module mydebug
save
  logical :: debug = .false. ! .true.   
  ! True = The Programm should be very verbous;
  ! False = normal mode


contains
  subroutine debugmode(myswitch)
    logical :: myswitch
    debug = myswitch
  end subroutine debugmode

  subroutine debugme(anyvar)
    character(len=80) :: anyvar
    write(*,*) anyvar
  end subroutine debugme

end module mydebug



module pse
save 

contains


  subroutine initdb

    integer :: filelength_as_lines = -1
    integer :: io_error
    integer, parameter :: ChemDB = 900
    integer :: ordnungszahl
    character (len =70) :: zeichenkette
    character (len =3) :: findelement = "Ge"
    character (len =3) :: elementname

    character (len =30) ::  ChemFMT = '(I3, 2X, A3, A10)'
    open(unit=ChemDB,file='elements.dat',status='old',action='read', &
         iostat=io_error)


    write(*,*) filelength_as_lines

    if (io_error == 0) then


       do 
          read(ChemDB,ChemFMT) ordnungszahl, elementname, zeichenkette 
!          write(*,*) ordnungszahl
          if (ordnungszahl == 999) then 
             exit 
          end if

          if (elementname == findelement) then  
             write(*,*) ordnungszahl, elementname, zeichenkette
          end if
       end do

    else
       write(*,*) 'Could not open database error', &
            io_error,' will exit now.'
    end if

    close(unit=ChemDB)
    return
  end subroutine initdb

end module pse







program readchem
  use mydebug
  use elements
  use globalknowledgeofletters
  use pse


  implicit none




  type(formula), dimension(50) :: myform



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

  character(len=80) :: debugstring

  write(*,*) "enter chemical equation:"
  ! read(*,*) chemeq


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

  write(debugstring,*) step 
  call debugme(debugstring)

call initdb

end program readchem
