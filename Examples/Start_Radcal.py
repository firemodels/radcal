#!/usr/bin/env python

"""
------------------------------------------------------------------------------
This is Python file/script.
Test to plot transmissity on a nice plot using Python
This is a prototype. Could be used a fully functional post-processing interface for RADCAL.

Uses:
-----
@Article{Hunter:2007,
  Author    = {Hunter, J. D.},
  Title     = {Matplotlib: A 2D graphics environment},
  Journal   = {Computing In Science \& Engineering},
  Volume    = {9},
  Number    = {3},
  Pages     = {90--95},
  abstract  = {Matplotlib is a 2D graphics package used for Python
  for application development, interactive scripting, and
  publication-quality image generation across user
  interfaces and operating systems.},
  publisher = {IEEE COMPUTER SOC},
  year      = 2007
}

SVN Info. Commented for now
$Id: $'
$Revision: $'
$Date: $'
------------------------------------------------------------------------------
"""

import numpy as np
import matplotlib.pyplot as plt
import subprocess
import sys

# Is the plot visible?
Visible=True

# Define bounds for the wavenumber
wave_min = 0.0
wave_max = 10000.0

#==============================================================================
# Run Radcal


# Python Version 2.6 and above

# 
Process="./RADCAL.x"

try:
    retcode = subprocess.call(Process, shell=True)
    if retcode < 0:
        print >>sys.stderr, "Program was terminated by signal", -retcode
    else:
        print >>sys.stderr, "Program ended correctly"
except OSError as e:
    print >>sys.stderr, "Execution failed:", e

#==============================================================================
# Read tecplot output created by RADCAL.
# file_2_read = TRANS_<CASEID>.tec
# CASEID can be read from the first line of RADCAL.out Case ID: <CASEID>

# Get CASE ID
f          = file("RADCAL.out","r")
lines      = f.readlines()
first_line = lines[0]

split_first = str.split(first_line)
CASEID      = split_first[1]

# Get effective_absorption, mean_planck, total_emissivity, Received_flux
for i in range(4,9):
	line       = lines[i]
	split_line = str.split(line)

	if i == 4:
		effective_absorption = np.float(split_line[2])
	elif i == 5:
		mean_planck          = np.float(split_line[4])
	elif i == 6:
		total_emissivity     = np.float(split_line[2])
	elif i == 7:
		Received_flux        = np.float(split_line[3])
	elif i == 8:
		Total_trans          = np.float(split_line[2])

# Open Tecplot file and read the data
column_wavenumber     = 0 
column_transmissivity = 1
column_radiance       = 2

tecplot_file = "TRANS_" + CASEID + ".tec"

wavenumber   = []
transmissity = []
radiance     = []

with open(tecplot_file, 'r') as f:
	for i,line in enumerate(f):
		if i>= 6:
			line         = line.strip()
			sline        = line.split()
			wavenumber.append(   sline[column_wavenumber    ] )
			transmissity.append( sline[column_transmissivity] )
			radiance.append(     sline[column_radiance      ] )

#==============================================================================
# Plot the graph

# Remove any underscript in CASEID
CASEID = CASEID.replace('_',' ')

# Set up properties for class figure
plt.rc('font', family='serif')
plt.rc('xtick', labelsize='x-small')
plt.rc('ytick', labelsize='x-small')

# Create Figure 
fig = plt.figure(figsize=(12, 9))

#------------------------------------------------------------------------------
# Plot the transmissity

ax  = fig.add_subplot(2, 1, 1)

#plot the two lines on same graph
ax.plot(wavenumber, transmissity, label = r'$\tau_{\omega}$', linewidth=2.0, color='k',    ls='solid' )
#plt.plot(wavenumber, y_series_2, label = r'$x^3$', linewidth=2.0, color='0.50', ls='solid' )

ax.set_xlabel(r'Wavenumber in  cm$^{-1}$')
ax.set_ylabel('Transmissivity')
ax.set_title(CASEID,fontsize='large')

# Print the Planck mean absorption coefficient
string_planck = " %3.2e" % (mean_planck)
plt.figtext(0.15,0.65,r'$\alpha_{Planck}$  =' + string_planck)

# Print the effective mean absorption coefficient
string_effective = " %3.2e" % (effective_absorption)
plt.figtext(0.15,0.62,r'$\alpha_{Effective}$=' + string_effective)

# Print the effective mean emissivity coefficient
string_emissivity = " %3.2e" % (total_emissivity)
plt.figtext(0.15,0.59,r'$\epsilon_{Effective}$ =' + string_emissivity)

# Print the total transmissity coefficient
string_Total_trans = " %3.2e" % (Total_trans)
plt.figtext(0.15,0.56,r'$\tau$' + '\t' + '\t' + '=' + string_Total_trans)

# Plot the wavenumbers with the highest values on the left
ax.set_xlim(wave_min,wave_max)
ax.set_ylim(0,100.1)
ax.invert_xaxis()

#------------------------------------------------------------------------------
# Plot the incident spectral radiance

ax2  = fig.add_subplot(2, 1, 2)

ax2.plot(wavenumber, radiance, label = r'Incident Radiance', linewidth=2.0, color='k',    ls='solid' )
ax2.set_ylabel(r'Incident Radiance W/m$^2$/str/cm$^{-1}$')
ax2.set_xlabel(r'Wavenumber in  cm$^{-1}$')
ax2.set_xlim(wave_min,wave_max)
ax2.invert_xaxis()
#plt.legend(loc="best",frameon=False)

# Print the incident radiant flux 
string_radiant = "Incident Flux:\n%8.4e" % (Received_flux)
plt.figtext(0.15,0.37,string_radiant + r' W/m$^2$')

#------------------------------------------------------------------------------
# Save Figure
plt.savefig(CASEID.replace(' ','_') + ".pdf")

# Show plot if Visible
if Visible:
	plt.show()

exit()
