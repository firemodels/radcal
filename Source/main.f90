!==============================================================================
program driver
!==============================================================================
!
!
 use radcal
#ifdef openmp
 use omp_lib 
#endif

implicit none

 integer, parameter :: exb = selected_real_kind(16)

 real(eb), allocatable, dimension(:) :: transmissivity

 real(eb) :: amean, planck_mean_absorption, flux, total_length_m,             &
             total_length_cm, total_transmissivity

 integer :: io
 character(len=255) :: filename

! for performance check
 real(exb) :: time_init
 real(exb) :: time_end

 call cpu_time(time_init)

!------------------------------------------------------------------------------
! Define output file. RADCAL.out (Important: Capitalized name)
 io       = 1
 filename = 'RADCAL.out'
 open(unit=io,file=trim(filename),status='unknown')  

 call read_input(io)
 call init_radcal(io)

 call sub_radcal(amean,planck_mean_absorption,flux,total_transmissivity,io)

 total_length_m  = sum(segment_length_m)
 total_length_cm = m_to_cm*total_length_m

!------------------------------------------------------------------------------
! Write output file RADCAL.out

 write(io,'(A)') 'CASEID: '//trim(chid)//' TITLE: '//trim(title)
 write(io,'(A)') '-------------------------------------------------------------'
 write(io,*) 'Calculation completed.'
 write(io,*) 'Total path length (m):',char(9),  total_length_m

 write(io,*) 'Amean (cm-1):', char(9),char(9),char(9),  amean
 write(io,*) 'Planck mean absorption (cm-1):', char(9), planck_mean_absorption
 write(io,*) 'Total Emissivity:', char(9),char(9), 1.0_eb-dexp(-amean*total_length_cm)
 write(io,*) 'Received Flux (W/m2/str):', char(9), Flux
 write(io,*) 'Total Transmissivity:', char(9), char(9),   Total_transmissivity
 write(io,'(A)') '-------------------------------------------------------------'
 write(io,3) trim(Radcalrev)
 write(io,4) trim(Radcaldate)

#ifdef date
  write(io,5) trim(date_compile)  
#endif

! If compiled with openmp, indicate in output file
#ifdef openmp
    write(io,'(A)') 'Compiled with OpenMP'
    write(io,6) n_threads
#endif

!------------------------------------------------------------------------------
! Print spectral transmissivity and incident radiance

 allocate(transmissivity(nom))
 transmissivity = ttau(npt,:)

 call tau_print(chid,pfuel,total_length_m,transmissivity,lambda(1:nom))

 call rcdealloc

 call cpu_time(time_end)
 
!------------------------------------------------------------------------------
! Print below characteristics only when compiled in debug mode

#ifdef debug
    write(*,1) 1000.0_exb*(time_end-time_init)
    write(*,3) trim(radcalrev)
    write(*,4) trim(radcaldate)
#ifdef date
    write(*,5) trim(date_compile)  
#endif

#endif

!------------------------------------------------------------------------------
! write performance measurements and close radcal.out

 write(io,1) 1000.0_exb*(time_end-time_init)
 close(io)

 1 format('Execution time (ms): ',1pe12.5)
 2 format(3(1pe12.5,2X))
 3 format('Version: ',(A))
 4 format('Version created on: ',(A))
 5 format('Version built on: ',(A))

#ifdef openmp
 6 format('Maximum number of threads used :',(i4))
#endif
!------------------------------------------------------------------------------
 contains 

!============================================================================== 
 subroutine tau_print(case_id, pressure, path_length, transmissivity, wave_length)
!==============================================================================
! This function prints the wavenumber in cm-1, the transmissivity in %, 
! the incident radiance, and the mean spectral absorption coefficient
!
 use radcal, only : eb, incident_radiance
 implicit none

! Variables passed in 
 real(eb), dimension(:), intent(in) :: wave_length    ! Wave length in micron
 real(eb), dimension(:), intent(in) :: transmissivity ! Transmissivity
 
 real(eb), intent(in) :: pressure
 real(eb), intent(in) :: path_length

 character(len=255), intent(in) :: case_id

! Local variables
 character(len=15)  :: pressure_atm
 character(len=15)  :: path_length_m
 character(len=255) :: filename
 character(len=50)  :: format_output

 integer :: n_max
 integer :: i_tecfile, i

 n_max = size(transmissivity(:))

! Define format output for the tecplot file
! Special consideration for the double precision must be taken into account

 format_output = "(4(1es23.16E3,2x))"

 write(pressure_atm,'(1e12.5)') pressure
 write(path_length_m,'(1pe12.5)') path_length


 filename = 'TRANS_'//trim(adjustl(case_id)) // '.tec'

 i_tecfile = 40

 open(unit=i_tecfile,file=trim(filename),status='unknown')  

 write(i_tecfile,*) 'VARIABLES='
 write(i_tecfile,*) '"<greek>w</greek> (cm<sup>-1</sup>)"'
 write(i_tecfile,*) '"Trans (%), Pa='// trim(pressure_atm) // ' (atm), L= '// trim(path_length_m)  // ' (m)"'
 write(i_tecfile,*) '"Radiance, Pa='// trim(pressure_atm) // ' (atm), L= '// trim(path_length_M)  // ' (m)"'
 write(i_tecfile,*) 'ZONE T = "Radcal '//trim(case_id)//'", F=POINT '

 do i = 1, n_max
     write(i_tecfile,format_output) 1.0e+4_eb/wave_length(i), 100._eb*transmissivity(i), incident_radiance(i)
 enddo

 close(i_tecfile)

!------------------------------------------------------------------------------

 return
 end subroutine tau_print

!------------------------------------------------------------------------------

!==============================================================================
subroutine termination(ierr,io)
!==============================================================================
! Subroutine called when exceptions are raised. 
! terminates the program and write error messages depending on the context.
! variables passed in
! ierr :: error message indice
! io   :: file unit number
! variables passed out:: null
!------------------------------------------------------------------------------

 integer, intent(in) :: ierr, io
 character(len=2056) :: message

 message = 'Error! radcal did not end correctly.'//char(10)//'See message below.'

 write(io,'(A)') trim(message)

 select case (ierr)
    case(1) 
       write(io,'(A)') 'Error 1: ommax should be greater than ommin.' 
    case(2)
       write(io,'(A)') 'Error 2: (internal) vector x and y for integration should have same size.' 
       ! deallocate memory
       call rcdealloc
    case(3)
       write(io,'(A)') 'Error 3: (internal) not enough points for integration.' 
       ! deallocate memory
       call rcdealloc
    case(4)
       write(io,'(A)') 'Error 4: lambdmax should be greater than lambdamin.' 
    case(5)
       write(io,'(A)') 'Error 5: No &Path_Segment defined. Program stopped.' 
 end select
 close(io)

! End execuation program
 stop
 
!------------------------------------------------------------------------------   
end subroutine termination

!========================================================================================
subroutine write_input(io)
!========================================================================================
! This subroutine write a default radcal.in in the case that no radcal.in is provided
! in particular, it writes the species availables.
! this subroutine should only be called when radcal.in does not exist
!----------------------------------------------------------------------------------------

! variables passed in

 integer, intent(in) :: io

! local variables

 integer, parameter  :: i_input = 10
 integer             :: ierr 
 character(len=30)   :: filename

 character(len=2048) :: header
 character(len=2048) :: line_text
 character           :: character_line(80)

 logical             :: file_exist

 integer :: i_species, i

! Test if RADCAL.in exists. If yes write message in io file.
 filename = 'RADCAL.in'

 inquire(FILE=filename, EXIST=file_exist)

 IF (file_exist) THEN
   write(io,'(A)') 'ERROR in WRITE_INPUT!'
   write(io,'(A)') 'RADCAL.in exists, hence WRITE_INPUT should not be called.'
   return
 ENDIF

!----------------------------------------------------------------------------------------
! Create header first

 character_line(:) = '-'

 write(HEADER,'(A)')    '# Generic RADCAL input file'//char(10)                   &
                     // '# Created automatically'//char(10)                       &
                     // '# List of species currently available:'
                     
!----------------------------------------------------------------------------------------
! Write header

 open(unit=i_input,file=trim(filename),action='write',status='new',IOSTAT=ierr)  

 if (ierr/=0) then
    write(io,'(A)') 'ERROR when attempting to create input file RADCAL.in!'
    close(io)
    stop
 endif

 write(line_text,'(A1,80A1)') '#', (character_line(i), i = 1,80)
 write(i_input,'(A)') trim(line_text)
 write(i_input,'(A)') trim(HEADER)
 write(i_input,'(A)') trim(line_text)
!----------------------------------------------------------------------------------------
! Write the list of available species

 write(line_text,'(A)') '# <species name>' // char(9) //'! <phase> <comments>'
 write(i_input  ,'(A)') trim(line_text)  

 DO i_species = 1, N_SPECIES
   write(line_text,'(A)') '# '//trim(SPECIES(i_species)%ID)// char(9) // trim(SPECIES(i_species)%COMMENTS)
   write(i_input  ,'(A)') trim(line_text)  
 ENDDO

 write(line_text,'(A1,80A1)') '#', (character_line(i), i = 1,80)
 write(i_input,'(A)') trim(line_text)

 write(line_text,'(A)') '#'//char(10) // '# How to use:'//char(10) // '#' // char(9) //  &
       '1) Discretize the line of sight into isothermal, homogeneous segments'//char(10) // '#' // char(9) //&
       '2) Define each segment temperature (variable "T", in Kelvin) and length'//         &
       ' (variable "LENGTH", in meters)'//char(10) // '#' // char(9) //                     &
       '3) Enter the pressure of each segment (variable "PRESSURE", in atmosphere)'//char(10) // '#' // char(9) //&
       '4) Enter the composition of the mixture, in mole fraction for gas phase '//      &
       'species (variable "X<name of species>")' // char(10) // '#' // char(9) // char(9)//&
       'Important: make sure the sum of species mole fraction is equal to 1'//char(10)//'#'//char(9)//&  
       '5) Define bounds of the spectrum OMMIN/OMMAX in wavenumber (1/cm)'//char(10)//'#'//char(9)//char(9)//&
       '6) Do not forget to enter the temperature of the surrounding, which is'//char(10)//'#'//char(9)//char(9)//&
       'represented by a wall at an infinite distance at its blackbody temperature'//char(10)//'#'//char(9)//char(9)//&
       '(variable "TWALL" in Kelvin)'

 write(i_input,'(A)') trim(line_text)

 write(line_text,'(A1,80A1)') '#', (character_line(i), i = 1,80)
 write(i_input,'(A)') trim(line_text)

 write(line_text,'(A)') 'Example:'//char(10)//'&HEADER TITLE="Example" CHID="Example" /'//char(10)

 write(line_text,'(A)') trim(line_text)//char(10)//'&BAND'
 write(line_text,'(A)') trim(line_text)//char(10) //char(9)//char(9)//'OMMIN = 50.0'
 write(line_text,'(A)') trim(line_text)//char(10) //char(9)//char(9)//'OMMAX = 10000.0 /'

 write(line_text,'(A)') trim(line_text)//char(10)//'&WALL TWALL = 500.0 /'//char(10)

 write(line_text,'(A)') trim(line_text)//char(10)//'&Path_Segment ! Define a homogeneous segment'

 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'T        = 300.0  ! Temperature in Kelvin'
 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'LENGTH   = 0.3175 ! Length of the segment in meters'
 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'PRESSURE = 1.0    ! Pressure in atm'
 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'XC2H4    = 0.01   ! Mole fraction of Ethylene'
 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'XCO2     = 0.0033 ! Mole fraction of CO2'
 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'XH2O     = 0.01   ! Mole fraction of H2O'
 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'XO2      = 0.21   ! Mole fraction of O2'
 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'XN2      = 0.7667 ! Mole fraction of N2'
 write(line_text,'(A)') trim(line_text)//char(10)//char(9)//'Fv       = 1.0e-7/! Soot volume fraction'

 write(i_input,'(A)') trim(line_text)

 write(line_text,'(A1,80A1)') '#', (character_line(i), i = 1,80)
 write(i_input,'(A)') trim(line_text)

!----------------------------------------------------------------------------------------
 return
END SUBROUTINE WRITE_INPUT

!========================================================================================
subroutine read_input(io)
!========================================================================================
! this subroutine reads the input file containing definition of the problem to be studied
!
 implicit none

! variables passed in

 integer, intent(in) :: io

! local variables

 integer           :: i_input, ierr 
 character(len=30) :: filename

 logical           :: file_exist

 call populate_species(io)

 filename = 'RADCAL.in'
 i_input  = 2

 inquire(file=filename, exist=file_exist)

 if (.not.(file_exist)) then
    write(io,'(A)') 'WARNING ! RADCAL.in was NOT provided.'
    write(io,'(A)') 'Creating Default RADCAL.in for illustration purposes.'
    call write_input(io)

    close(io)
    stop
 endif

 open(unit=i_input,file=trim(filename),action='read',status='old',iostat=ierr)  

 if (ierr/=0) then
    write(io,'(A)') 'Error when attempting to read input file!'
    write(io,'(A)') 'Maybe RADCAL.in does not exist.'

    close(io)
    stop
 endif

!------------------------------------------------------------------------------
! Loops over lines
! read line. if line starts with pt{ then read lines until symbol } is found
! if ommin is found, assign ommin
! if ommax is found, assign omax
! if twall is found, assign twall
! syntax: &pt length=xxx (meters) t=xxx (k) <species_name>=xxx (partial pressure in atm) /
! npt = number of heterogeneous points along the path length

 call read_header(i_input,io)
 call read_band(i_input,io)
 call read_wall(i_input,io)
 call read_point(i_input,io)

 close(i_input)

 return
!------------------------------------------------------------------------------
end subroutine read_input

!==============================================================================
subroutine read_point(i_input,i_output)
!==============================================================================
! This subroutine reads the file (unit io) and search for keyword path_segment
! counts number of homogeneous segments along a unique pathline, and assign 
! values of species mole fractions or volume fraction (only for soot), 
! temperature (assumed to be uniform along a given segment of the pathline; each
! segment has a length = length, and pressure (in atm)
!
! units:
! temperature: (kelvin)
! species data in mole fraction
! pathlength: (meters)
! pressure: (atm)
!------------------------------------------------------------------------------

 implicit none

! Variables in
 integer, intent(in) :: i_input  ! unit of the input file. alreay opened
 integer, intent(in) :: i_output ! unit of the output file. alreay opened

! local
! Pointer Gas_phase species
 type gas_phase
    real(kind=eb), pointer :: obj
    character(len=30)      :: name
 end type gas_phase

 type(gas_phase), allocatable, dimension(:) ::  Mole_Fraction

 real(kind=eb) :: sum_mole_fraction

 real(kind=eb) :: t, length, pressure, fv

 real(kind=eb), target :: xco2, xh2o, xco, xch4, xc2h4, xc2h6, xc3h8, &
                         xc3h6, xc7h8, xc7h16, xch3oh, xmma, xn2, xch4_old, xo2

 character(len=255) :: line

 integer :: n_segment, i_segment, i_species, i_species_gas, status, i_fv

 namelist /path_segment/ T, length, pressure, xco2, xh2o, xco, xch4, xc2h4, xc2h6, &
              xc3h8, xc3h6, xc7h8, xc7h16, xch3oh, xmma, fv, xn2, xch4_old, xo2
 

!------------------------------------------------------------------------------
! Initialize NAMELIST variable: set to 0

 n_segment = 0
 T         = 0.0_eb 
 length    = 0.0_eb
 pressure  = 0.0_eb
 xco2      = 0.0_eb
 xh2o      = 0.0_eb
 xco       = 0.0_eb
 xch4      = 0.0_eb
 xc2h4     = 0.0_eb
 xc2h6     = 0.0_eb
 xc3h8     = 0.0_eb
 xc3h6     = 0.0_eb
 xc7h8     = 0.0_eb
 xc7h16    = 0.0_eb
 xch3oh    = 0.0_eb
 xmma      = 0.0_eb
 fv        = 0.0_eb
 xn2       = 0.0_eb
 xch4_old  = 0.0_eb
 xo2       = 0.0_eb

!------------------------------------------------------------------------------
! Count the number of segment that comprises the pathline of interest
! A segment starts with the namelist name '&Path_Segment'
 
 rewind(i_input)
 status = 0
 do while (status == 0)
    read(i_input,'(A)',iostat=status,advance = 'yes') line
    if (index(trim(line),'&Path_Segment',BACK = .false.)>0) n_segment = n_segment + 1
 enddo

 rewind(i_input)

! Test whether there is at least one &Path_Segment. If not then write error message
! and call termination

 if (n_segment == 0) then 
   write(i_output,'(A)') 'Warning, there is no &Path_Segment defined. Namelist contains: '
   write(i_output,path_segment) 
   call termination(5,i_output)
 endif

!------------------------------------------------------------------------------
! Allocate memory for Partial pressure, Length, Temperature

 npt = n_segment

 if(allocated(Partial_Pressures_atm)) deallocate(Partial_Pressures_atm)
 allocate(Partial_Pressures_atm(N_SPECIES,n_segment))
 Partial_Pressures_atm = 0.0_EB

 if(allocated(Temp_Gas))              deallocate(Temp_Gas)
 allocate(Temp_Gas(n_segment))
 Temp_Gas = 0.0_EB

 if(allocated(Segment_Length_m))      deallocate(Segment_Length_m)
 allocate(Segment_Length_m(n_segment))
 Segment_Length_m = 0.0_EB

 if(allocated(Total_Pressure_atm))    deallocate(Total_Pressure_atm)
 allocate(Total_Pressure_atm(n_segment))
 Total_Pressure_atm = 0.0_EB

!------------------------------------------------------------------------------
! Allocate pointer to gas phase species. Pointer used for ease of variable initialization
! and tests.

  if (allocated(Mole_Fraction)) deallocate(Mole_Fraction)
  allocate(Mole_Fraction(1:n_species_gas))

  Mole_Fraction(1)%obj   => xco
  Mole_Fraction(1)%name  = 'CO'
  Mole_Fraction(2)%obj   => xh2o
  Mole_Fraction(2)%name  = 'H2O'
  Mole_Fraction(3)%obj   => xco2
  Mole_Fraction(3)%name  = 'CO2'
  Mole_Fraction(4)%obj   => xch4
  Mole_Fraction(4)%name  = 'CH4'
  Mole_Fraction(5)%obj   => xc2h4
  Mole_Fraction(5)%name  = 'C2H4'
  Mole_Fraction(6)%obj   => xc2h6
  Mole_Fraction(6)%name  = 'C2H6'
  Mole_Fraction(7)%obj   => xc3h8
  Mole_Fraction(7)%name  = 'C3H8'
  Mole_Fraction(8)%obj   => xc3h6
  Mole_Fraction(8)%name  = 'C3H6'
  Mole_Fraction(9)%obj   => xc7h8
  Mole_Fraction(9)%name  = 'C7H8'
  Mole_Fraction(10)%obj  => xc7h16
  Mole_Fraction(10)%name = 'C7H16'
  Mole_Fraction(11)%obj  => xch3oh
  Mole_Fraction(11)%name = 'CH3OH'
  Mole_Fraction(12)%obj  => xmma
  Mole_Fraction(12)%name = 'MMA'
  Mole_Fraction(13)%obj  => xn2
  Mole_Fraction(13)%name = 'N2'
  Mole_Fraction(14)%obj  => xch4_old
  Mole_Fraction(14)%name = 'CH4_OLD'
  Mole_Fraction(15)%obj  => xo2
  Mole_Fraction(15)%name = 'O2'

!------------------------------------------------------------------------------
! Get index of fv

 i_fv = index_species('Fv')

!------------------------------------------------------------------------------
! Read io file and allocate values of Temp_Gas, Segment_Length_m, and 
! Partial_Pressures_atm.
! loop over the number of points along the path length

 do i_segment = 1, n_segment

! Initialize elements of path_segment namelist
     T        = 0.0_EB
     Length   = 0.0_EB
     Pressure = 0.0_EB
     Fv       = 0.0_EB
! Initialize the mole fraction of species to zero on each new segment
     do i_species_gas = 1, n_species_gas
        Mole_Fraction(i_species_gas)%obj = 0.0_EB
     enddo

! Read name list path_segment
     read(i_input,Path_Segment) 

!------------------------------------------------------------------------------
! Test consistency of the mole fraction. Consider only the absolute values of
! input.  
! It is required to have: sum(Mole_Fraction()%obj = 1).
! Be careful to the machine floating point precision.
! Issues can arise for some numbers. perform test: abs(1.0-sum) > epsilon
! instead of sum = 1
! If not: print error message, put segment values to 0 and cycle loop

     sum_mole_fraction = 0.0_EB

     do i_species_gas = 1, n_species_gas
        sum_mole_fraction = sum_mole_fraction + abs(Mole_Fraction(i_species_gas)%obj)
     enddo


     if (dabs(sum_mole_fraction-1.0_EB)>epsilon(sum_mole_fraction)) then
! Perform test. If not successful, print informations
        write(i_output,'(A,A,I2)')   &
        'Error. Sum of mole fraction not equal to 1 for segment #', char(9), i_segment

#ifdef debug
        write(*,'(A)') '-- DEBUG -- DEBUG -- DEBUG -- DEBUG --'
        write(*,*) 'Error. Sum of mole fraction not equal to 1: ', sum_mole_fraction

        do i_species_gas = 1, n_species_gas
           write(*,*) trim(Mole_Fraction(i_species_gas)%name), char(9),           & 
                      Mole_Fraction(i_species_gas)%obj 
        enddo
#endif 
        Temp_Gas(i_segment)                = 0.0_EB
        Segment_Length_m(i_segment)        = 0.0_EB
        Total_Pressure_atm(i_segment)      = 0.0_EB
        Partial_Pressures_atm(:,i_segment) = 0.0_EB
        cycle
     end if

!------------------------------------------------------------------------------
! Below is executed only if Sum(Mole_Fraction()%obj==1)
! Enforce positive values

     Temp_Gas(i_segment)                    = max(T,        0.0_EB)
     Segment_Length_m(i_segment)            = max(Length,   0.0_EB)
     Total_Pressure_atm(i_segment)          = max(Pressure, 0.0_EB)
     Partial_Pressures_atm(i_fv,i_segment)  = max(Fv,       0.0_EB)

! Populate Partial_Pressure with gas_phase species using pointer Mole_Fraction
! Recall: Partial_Pressures_atm(i,j) = Mole_Fraction(i)*Total_Pressure_atm(j)
! Assume ideal gas mixture

     do i_species_gas = 1, n_species_gas

         i_species = index_species(trim(Mole_Fraction(i_species_gas)%name))

         Partial_Pressures_atm(i_species,i_segment) =  &
            abs(Mole_Fraction(i_species_gas)%obj)*Total_Pressure_atm(i_segment)
     enddo

 enddo

#ifdef debug
 do i_segment = 1, n_segment
     write(*,'(A)') '-- DEBUG -- DEBUG -- DEBUG -- DEBUG --'
     write(*,'(2A,I2)') 'Point number: ', char(9), i_segment
     write(*,'(3A,1f8.4)') 'Total pressure in atm: ', char(9), char(9),       &
                             Total_Pressure_atm(i_segment)
     write(*,'(2A,1f8.4)') 'Segment length in meters: ', char(9),             &
                             Segment_Length_m(i_segment)
     write(*,'(2A,1f12.8)')  'Segment temperature in Kelvin: ', char(9),      &
                             Temp_gas(i_segment)

     write(*,'(A)') 'Partial Pressure Array in subroutine READ_POINT. Values in atm. '

     do i_species = 1, n_species 
        write(*,'(2A,1pe12.5)') trim(species(i_species)%id), char(9),           & 
                   Partial_Pressures_atm(i_species,i_segment) 
     enddo

 enddo
#endif 

 rewind(i_input)

 return
!------------------------------------------------------------------------------
end subroutine read_point

!==============================================================================
subroutine read_band(i_input,i_output)
!==============================================================================
! This subroutine reads the file (unit io) and searches for keyword %band
! &bands defines the lower and upper bound of the spectrum to be computed,
! ommin and ommax, respectively. Both are given in cm-1
!------------------------------------------------------------------------------

 implicit none

! Variables in
 integer, intent(in) :: i_input  ! unit of the input file. alreay opened
 integer, intent(in) :: i_output ! unit of the output file. alreay opened

! Local variables

 integer :: io_err ! error condition number

 namelist /band/ ommin, ommax, lambdamin, lambdamax

 ommin =  500.00_eb
 ommax = 5000.00_eb

 lambdamin = -1.1e+4_eb
 lambdamax = -1.0e+4_eb

! read from the beginning. perform rewind as precaution
 rewind(i_input)
 read(i_input,band,iostat=io_err)

! Checked for end of file error. Needed to avoid program stop in case band is 
! not present

 if (io_err < 0) then 
    write(i_output,'(A)') 'Warning! No &band was defined. Using:'
    write(i_output,band)
 endif

! test to insure constitency of ommin and ommax
 if (ommax <= ommin)         call termination(1,i_output)
 if (lambdamax <= lambdamin) call termination(4,i_output)

! Condition: User has entered values for lambdamin, lambdamax. 
! They must be positive
 
 if ((0.0_eb<=lambdamax ).and.(0.0_eb<=lambdamin).and. & 
     (ommin == 500.00_eb).and.(ommax == 5000.00_eb)) then
    ommin     = 1.0e+4_eb/lambdamax
    ommax     = 1.0e+4_eb/lambdamin
 else
    lambdamin = 1.0e+4_eb/ommax
    lambdamax = 1.0e+4_eb/ommin
 endif 

 rewind(i_input)

 return
!------------------------------------------------------------------------------
end subroutine read_band

!==============================================================================
subroutine read_wall(i_input,i_output)
!==============================================================================
! This subroutine reads the file (unit io) and searches for keyword %wall
! &wall defines the wall (or infinity) temperature: twall
! the wall acts as black body
!------------------------------------------------------------------------------

 implicit none

! Variables in
 integer, intent(in) :: i_input  ! unit of the input file. alreay opened
 integer, intent(in) :: i_output ! unit of the output file. alreay opened

! Local variables
 integer :: io_err ! catch the value of the error raised by output subroutine

 namelist /wall/ twall

 twall = 0.0_eb

! read from the beginning. perform rewind as precaution
 rewind(i_input)
 read(i_input,wall,iostat=io_err)

 if (io_err < 0) then 
    write(i_output,'(A)') 'Warning! No &wall was defined. Using:'
    write(i_output,wall)
 endif


! test to insure constitency of ommin and ommax
 if (twall<0.0_eb) then
    write(i_output,'(A)') 'Caution!! twall you have specified is less than 0.  '
    write(i_output,'(A)') 'Twall set automatically to 0.  '
    twall = max(0.0_eb,twall)
 end if

 rewind(i_input)

 return
!------------------------------------------------------------------------------
end subroutine read_wall

!==============================================================================
subroutine read_header(i_input,i_output)
!==============================================================================
! This subroutine reads the file (unit i_input) and search for keyword &header
! &header defines the case id (chid) that will be used to generate to output and the
! case title (title) which will be printed in the output file
!------------------------------------------------------------------------------

 implicit none

! Variables in
 integer, intent(in) :: i_input  ! unit of the input file. alreay opened
 integer, intent(in) :: i_output ! unit of the output file. alreay opened

! Local
 integer :: io_err ! catch the value of the error raised by output subroutine

 namelist /header/ chid, title

 title = 'RADCAL Simulation '
 chid  = 'RADCAL' 

! Read from the beginning. perform rewind as precaution
 rewind(i_input)
 read(i_input,header,iostat=io_err)

 if (io_err < 0) then 
    write(i_output,'(A)') 'Warning! No &header was defined. Using:'
    write(i_output,header)
 endif

 rewind(i_input)

 return
!------------------------------------------------------------------------------
end subroutine read_header

end PROGRAM

