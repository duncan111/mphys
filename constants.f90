module constants
  implicit none
  integer, parameter ::dp = selected_real_kind(15,300)
  real(kind=dp), parameter :: mp=938.27204621, mn=939.56537921, c=2.99792458*(10**8) !c needs to be in fm
  real(kind=dp), parameter :: hbarc=197.32697, hbar=hbarc/c, pi=3.141592653589793, e=4.253890 ! not sure about e!!!
end module constants
