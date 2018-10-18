program EoS
  use constants
  implicit none
  real(kind=dp) :: WN, b, Btot, P, ne
  integer :: A,Z,N
  N=56
  Z=26
  A=N+Z
  ne=0.0_dp
  P=10.0_dp**4.0_dp

  print*, 'Garvey model'
  Btot=59.9117+15.5842*Z+6.31823*N+0.321516*N*Z-0.303061*(Z**2)-0.105635*(N**2)
  b=Btot/A
  WN=Nuclear_E(A,Z,b)
  print*, 'binding energy per nucleon = ', b, 'MeV'
  print*, 'Nuclear energy', WN, 'MeV'

  print*, ''

  print*, 'from AME 2016'
  b=8.790354  
  WN=Nuclear_E(A,Z,b)
  print*, 'binding energy per nucleon = ', b, 'MeV'
  print*, WN, 'MeV'

  call electron_density(P,Z,ne)  
  




  contains
  function Nuclear_E(A,Z,b)
  !-----------------------------------------------------------------------------------------!
  ! function takes the binding energy, number of nucleons and number of protons and returns !
  ! the energy of the nucleus, one of the three componants of the baryon chemical potnetial !
  !-----------------------------------------------------------------------------------------!
    implicit none
    integer, intent(in) :: A,Z
    real(kind=dp), intent(in) :: b
    real(kind=dp) :: Nuclear_E
    Nuclear_E=mn*(A-Z)+mp*Z-b*A
  end function
  
  subroutine electron_density(P,Z,ne)
  !-----------------------------------------------------------------------------------------!
  ! subroutine takes the pressure and number of protons and finds the electron density that !
  ! , when used in the equation for pressure, gives the input pressure. DOES NOT WORK YET!  !
  !-----------------------------------------------------------------------------------------!
    implicit none
    integer, intent(in) :: Z
    real(kind=dp), intent(in) :: P
    real(kind=dp), intent(out) :: ne
    real(kind=dp) :: netest, Ptest, Pe, PL, third
    integer :: i
    
    third=1.0_dp/3.0_dp  
    
    
    do i=1,1000  
      netest=real(i,dp)/1000000.0_dp
  
      Pe=0.25_dp*hbarc*netest*(hbar*(3*(pi**2.0_dp)*netest)**third)
      PL=third*(-1.819620_dp*(real(Z,dp)**2.0_dp)*(e**2.0_dp)*(netest/(2.0_dp*real(Z,dp)))**third)*(netest/real(Z,dp))
      Ptest=Pe+PL
      print*, Ptest
      if (abs(P-Ptest) .lt. 0.00001_dp) then
        ne=Ptest
        exit
      end if
    end do
  end subroutine electron_density
end program EoS
