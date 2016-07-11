#include "fintrf.h"

!======================================================================
!     timestwo.f
!     Computational function that takes a scalar and doubles it.
!     This is a MEX file for MATLAB.
!======================================================================

! Gateway routine
subroutine mexFunction(nlhs, plhs, nrhs, prhs)

! Declarations
implicit none

! mexFunction arguments:
mwPointer plhs(*), prhs(*)
integer nlhs, nrhs

! Function declarations:
mwPointer mxGetPr
mwPointer mxCreateDoubleMatrix
integer mxIsNumeric
mwPointer mxGetM, mxGetN

! Pointers to input/output mxArrays:
mwPointer x_ptr, y_ptr

! Array information:
mwPointer mrows, ncols
mwSize size

! Arguments for computational routine:
real(selected_real_kind(12))  x_input, y_output

! Statements

! Check for proper number of arguments. 
if(nrhs .ne. 1) then
   call mexErrMsgIdAndTxt ('MATLAB:timestwo:nInput','One input required.')
elseif(nlhs .gt. 1) then
   call mexErrMsgIdAndTxt ('MATLAB:timestwo:nOutput','Too many output arguments.')
endif

! Check that the input is a number.
if(mxIsNumeric(prhs(1)) .eq. 0) then
   call mexErrMsgIdAndTxt ('MATLAB:timestwo:NonNumeric','Input must be a number.')
endif

x_ptr = mxGetPr(prhs(1))

! Get the size of the input array.
mrows = mxGetM(prhs(1))
ncols = mxGetN(prhs(1))
size = mrows*ncols

! Create Fortran array from the input argument.
call mxCopyPtrToReal8(x_ptr,x_input,size)

! Create matrix for the return argument.
plhs(1) = mxCreateDoubleMatrix(mrows,ncols,0)

y_ptr = mxGetPr(plhs(1))

! Call the computational subroutine.
call timestwo(y_output, x_input)

! Load the data into y_ptr, which is the output to MATLAB.
call mxCopyReal8ToPtr(y_output,y_ptr,size)

return
end


! Computational routine

subroutine timestwo(y_output, x_input)
real(selected_real_kind(12)) x_input, y_output

y_output = 2.0 * x_input
return
end