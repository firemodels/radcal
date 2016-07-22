# radcal
A library of subroutines for calculating radiative properties of gaseous species and soot.

RADCAL computes the spectral properties of the radiation participating species at discrete values of the spectrum (expressed either in wavenumber or in wavelength) and temperature, and returns two alternative mean absorption coefficients for each spectral band. The first coefficient is the Planck mean coefficient.  The second coefficient is the so-called path mean or effective absorption coefficient.

RADCAL was originally developed by William Grosshandler at the National Institute of Standards and Technology.  Recent improvements to include additional species were implemented by Vivien Lecoustre at the University of Maryland, College Park.

### Installation and running the code

Go to the [Releases](https://github.com/firemodels/radcal/releases) page and download the appropriate executable for your platform (Windows, OSX, or Linux).  Copy the executable to where ever you store program files.  Make sure the program is "executable".  On OSX or Linux

```
$ chmod +x radcal_intel_osx_64
```

Add the appropriate environment variables (Windows) or create necessary aliases (OSX, Linux).  For example, on OSX after copying the executable to `/Applications/RADCAL/radcal_intel_osx_64` set up the following alias in your `.bash_profile`:

```
alias radcal="/Applications/RADCAL/radcal_intel_osx_64"
```

Then exit and relaunch your terminal or source your `.bash_profile`.

### Running the code

To test your installation, copy the [example input file](https://github.com/firemodels/radcal/blob/master/Examples/RADCAL.IN) to a working directory.  From within the working directory type the name of the exe followed by the name of the input file.  For example, on OSX

```
$ radcal RADCAL.IN
```

The output is written to the same working directory.
