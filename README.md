# radcal
A library of subroutines for calculating radiative properties of gaseous species and soot.

RADCAL computes the spectral properties of the radiation participating species at discrete values of the spectrum (expressed either in wavenumber or in wavelength) and temperature, and returns two alternative mean absorption coefficients for each spectral band. The first coefficient is the Planck mean coefficient.  The second coefficient is the so-called path mean or effective absorption coefficient.

RADCAL was originally developed by William Grosshandler at the National Institute of Standards and Technology.  Recent improvements to include additional species were implemented by Vivien Lecoustre at the University of Maryland, College Park.

### Installation and running the code

Go to the [Releases]() page and download the appropriate executable for your platform (Windows, OSX, or Linux).

### Running the code

Copy the executable and example input file to your working directory.  From within the working directory type the name of the exe followed by the name of the input file.  For example, on Linux

```
$ cd Examples
$ ./radcal_intel_lunix_64 RADCAL.IN
```

The output is written to the same working directory.
