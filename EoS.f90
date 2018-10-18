program EoS
  implicit none
  integer, parameter ::dp = selected_real_kind(15,300)
  real(kind=dp), parameter :: mp=938.27204621, mn=939.56537921, c=2.99792458*(10**(8))
  real(kind=dp) :: WN, E, B
  integer :: A,Z,N
  A=72
  Z=46
  N=A-Z

  print*, 'Garvey model'
  B=59.9117+15.5842*Z+6.31823*N+0.321516*N*Z-0.303061*(Z**2)-0.105635*(N**2)
  E=B/A
  WN=Nuclear_E(A,Z,E)
  print*, 'binding energy per nucleon = ', E, 'MeV'
  print*, 'Nuclear energy', WN, 'MeV'

  print*, ''

  print*, 'from AME 2016'
  E=8.405222  
  WN=Nuclear_E(A,Z,E)
  print*, 'binding energy per nucleon = ', E, 'MeV'
  print*, WN, 'MeV'


  contains
  function Nuclear_E(A,Z,b)
  integer, intent(in) :: A,Z
  real(kind=dp), intent(in) :: b
  real(kind=dp) :: Nuclear_E
  Nuclear_E=mn*(A-Z)+mp*Z-b*A

  end function
end program EoS
