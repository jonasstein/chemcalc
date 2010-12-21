module globalknowledgeofletters
  character(len=26), parameter :: ABC_upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=26), parameter :: abc_lower="abcdefghijklmnopqrstuvwxyz"
  character(len=10), parameter :: numbers="0123456789"
end module globalknowledgeofletters


module elements
  save

  type element
     integer :: atomnumber
     character(len=3) :: name
  end type element



  type formula
     integer :: length = 0
     character (len=3), dimension(1000) :: element
     integer,  dimension(1000) :: quantity
  end type formula


  type(formula) :: UsersFormula

contains

  subroutine PrintUsersFormula
    write(*,*) "oooo" !UsersFormula%length
  end subroutine PrintUsersFormula

  subroutine InsertToUsersFormula(symbol, quantity)

  end subroutine InsertToUsersFormula


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

  subroutine logme(message)
    character(len=80) :: message
    write(*,*) message
  end subroutine logme

end module mydebug



module pse
save 
contains


  integer function getdblength()
    integer :: io_error
    integer, parameter :: ChemDB = 900
    integer :: linenumber 

    character (len =80) :: datarow

    open(unit=ChemDB,file='elements.dat',status='old',action='read', &
         iostat=io_error)

    if (io_error == 0) then
       write(*,*) "II: file elements.dat found and accessible."
       linenumber = 0

       do 
          read(ChemDB, '(A)', iostat=io_error) datarow
          if (io_error /= 0) exit
          linenumber = linenumber + 1
          write(*,*) "found dataset No.", linenumber
          write(*,*) datarow
       end do

    else
       write(*,*) "EE: can not open file elements.dat program will exit now."
       linenumber = -1
    end if

    close(unit=ChemDB)
    getdblength = linenumber
    return
  end function getdblength

  real function getmassfromelement(searchsymbol)
    integer :: io_error
    integer, parameter :: ChemDB = 900

    integer :: maxlines
    integer :: linenumber=0

    character (len =3) :: searchsymbol


    character (len =80) :: datarow
    character (len =30) ::  ChemFMT = '(I3, 1X, A3, 1X A20, F1.6, F1.4)'   ! Ordnungszahl, Symbol, Name, Molmasse, FooWert

!! EXAMPLE:
!!1   H   Wasserstoff          1.007947       3.0079
!!2   He  Helium               4.002602       4.0026
!!3   Li  Lithium              6.941223       8.2941
!!4   Be  Beryllium            9.012182       1.0122

    integer :: dbord
    character (len =3) :: dbsym
    character (len =10) :: dbname
    real :: dbatommass
    real :: dbelektrons

    maxlines = getdblength()

    open(unit=ChemDB,file='elements.dat',status='old',action='read', &
         iostat=io_error)


    do while (linenumber <= maxlines)
       linenumber = linenumber + 1
       read(unit=ChemDB, fmt=ChemFMT, iostat=io_error) datarow

       if (io_error /= 0) exit
       write(*,*) datarow
    end do

    close(unit=ChemDB)
    getmassfromelement = 0.1
    return
  end function getmassfromelement





end module pse



! =============================================
! main programm starts here
! =============================================



program readchem
  use mydebug
  use elements
  use globalknowledgeofletters
  use pse


  implicit none


!  call PrintUsersFormula

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

! call initdb

! write(*,*) "Datenbank ist so lang: " getdblength()
write(*,*) getmassfromelement("Ge")

end program readchem


! print database
! print parsed formula
! add item to formula
! objects
! 



! TODO
! Inent in/out
! parameter
! Kommentare
