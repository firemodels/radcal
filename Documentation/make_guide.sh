#!/bin/bash

# Add LaTeX search path; Paths are ':' separated
export TEXINPUTS=".:LaTeX_Style_Files:"

clean_build=1

gitrevision=`git describe --long --dirty`
echo "\\newcommand{\\gitrevision}{$gitrevision}" > gitrevision.tex

pdflatex -interaction nonstopmode RadCal_User_Guide &> RadCal_User_Guide.err
bibtex RadCal_User_Guide &> RadCal_User_Guide.err
pdflatex -interaction nonstopmode RadCal_User_Guide &> RadCal_User_Guide.err
pdflatex -interaction nonstopmode RadCal_User_Guide &> RadCal_User_Guide.err

# make sure the guide exists
if [ ! -e RadCal_User_Guide.pdf ]; then
  clean_build=0
  echo "***error: the RadCal Users Guide failed to build!"
fi

# Scan and report any errors in the LaTeX build process
if [[ `grep -E "Too many|Undefined control sequence|Error:|Fatal error|! LaTeX Error:|Paragraph ended before|Missing \\\$ inserted|Misplaced" -I RadCal_User_Guide.err | grep -v "xpdf supports version 1.5"` == "" ]]
   then
      # Continue along
      :
   else
      echo "LaTeX errors detected:"
      grep -A 1 -E "Too many|Undefined control sequence|Error:|Fatal error|! LaTeX Error:|Paragraph ended before|Missing \\\$ inserted|Misplaced" -I RadCal_User_Guide.err | grep -v "xpdf supports version 1.5"
      clean_build=0
fi

# Check for LaTeX warnings (undefined references or duplicate labels)
if [[ `grep -E "undefined|multiply defined|multiply-defined" -I RadCal_User_Guide.err` == "" ]]
   then
      # Continue along
      :
   else
      echo "LaTeX warnings detected:"
      grep -E "undefined|multiply defined|multiply-defined" -I RadCal_User_Guide.err
      clean_build=0
fi

if [[ $clean_build == 0 ]]
   then
      :
   else
      echo "RadCal User Guide built successfully!"
fi

