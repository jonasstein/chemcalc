program readchem
  implicit none

  character(len=50) :: Formel = "HCl"
  character(len=1) :: Zeichen

  real, dimension(10) :: Faktor
  integer :: Position = 1


  write(*,*) "Chemische Formel:"



  do while (Position < 50)
     Zeichen = Formel(Position:Position)  
     write(*,*) Position, Zeichen, ichar(Zeichen)
     Position = Position + 1 

     select case(Zeichen)
     case ("H") 
        write(*,*) "das war Wasserstoff"
        !     case()
     end select

  end do

  ! read(*,*) Formel




  ! write(*,*) 

end program readchem


function isuppercase(mychar)

select case(mychar)
case("A" <= mychar .and. mychar <= "Z" ) 
   write(*,*) "Klein"
case("A" <= mychar .and. mychar <= "Z" ) 
   write(*,*) "Klein"

end select

result 1
end function
