program readchem
  implicit none

  character(len=50) :: chemeq = "HCl23HO23JPIS24G"
  character(len=1) :: this_char

  character, dimension(50,3) :: eqsymbols
  integer, dimension(50) :: eqquantity

  !  real, dimension(10) :: Faktor
  integer :: position
  integer :: numberofsymbols = 0 
  integer :: lengthofeqstring = 0

  ! functions
  logical :: isuppercase
  logical :: islowercase
  logical :: isbetween
  logical :: isdigit


  write(*,*) "enter chemical equation:"
  ! read(*,*) chemeq


  ! ============================
  ! count chemical symbols
  ! ============================
  write(*,*) len(chemeq)
  do position = 1, len(chemeq)
     this_char = chemeq(position:position)  
     if (this_char == "") then
        ! no more data in equation string
        exit
     elseif (isuppercase(this_char)) then
        numberofsymbols = numberofsymbols + 1
     end if
       lengthofeqstring = position
  end do
  write(*,*) numberofsymbols, "chemical symbols detected"


  ! ============================
  ! parse the equation string
  ! ============================

  do position = 1, lengthofeqstring
     this_char = chemeq(position:position)  
     write(*,*) position, this_char, ichar(this_char)

     if (isuppercase(this_char)) then
        write(*,*) "das war gross"
     
     elseif(islowercase(this_char)) then 
        write(*,*) "das war klein"
     elseif(isdigit(this_char)) then 
        write(*,*) "das war zahl"
        eqquantity(position) = strtoint(this_char)
     else
        write(*,*) "EE: could not parse equation"
        exit
     end if


!!$      CHARACTER*8 DATE
!!$           INTEGER MONTH, DAY, YEAR
!!$
!!$           INTEGER MONTH2, DAY2, YEAR2
!!$           MONTH = 7
!!$           DAY = 4
!!$           YEAR = 93
!!$
!!$           WRITE (DATE,10) MONTH, DAY, YEAR
!!$        10 FORMAT (I2,'/',I2,'/',I2)
!!$           READ (DATE,20) MONTH2, DAY2, YEAR2
!!$       20  FORMAT (I2,1X,I2,1X,I2)




  end do

  write(*,*) eqsymbols
  write(*,*) eqquantity(1:10)

  !  logi =  isuppercase( "A" )
  if (lle("A", "D") .and. lle("D", "Z") )  then 
     write(*,*) "13"
  else 
     write(*,*) "12"
  end if


  if (.true.) then 
     write(*,*) "GGG"
  end if

  write(*,*) isuppercase("A") 

end program readchem



logical function isuppercase(mychar)
  implicit none
  character, intent(in) :: mychar
  logical :: isbetween

  isuppercase = isbetween(mychar,"A","Z")
end function isuppercase


logical function islowercase(mychar)
  implicit none
  character, intent(in) :: mychar
  logical :: isbetween

  islowercase = isbetween(mychar,"a","z")
end function islowercase


logical function isdigit(mychar)
  implicit none
  character, intent(in) :: mychar
  logical :: isbetween

  isdigit = isbetween(mychar,"0","9")
end function isdigit

logical function isbetween(mychar,firstchar,lastchar)
  implicit none
  character, intent(in) :: mychar, firstchar, lastchar

  if (lle(firstchar, mychar) .and. lle(mychar, lastchar) )  then 
     isbetween = .true.
  else 
     isbetween = .false.
  end if
end function isbetween



