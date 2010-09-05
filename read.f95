program readchem
  implicit none

  character(len=50) :: chemeq = "HCl23HO23JPIS24"
  character(len=1) :: this_char

  character, dimension(50,3) :: eqsymbols
  integer, dimension(50) :: eqquantity

  !  real, dimension(10) :: Faktor
  integer :: position = 1


  ! functions
  logical :: isuppercase
  logical :: islowercase
  logical :: isbetween
  logical :: isdigit


  write(*,*) "enter chemical equation:"
  ! read(*,*) Formel


  write(*,*) len(chemeq)


  do while (position < 10)
     this_char = chemeq(position:position)  
     write(*,*) position, this_char, ichar(this_char)
     position = position + 1 

     if (isuppercase(this_char)) then
        write(*,*) "das war gross"
     elseif(islowercase(this_char)) then 
        write(*,*) "das war klein"
     elseif(isdigit(this_char)) then 
        write(*,*) "das war zahl"

     else
        write(*,*) "EE: could not parse equation"
        exit
     end if


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



