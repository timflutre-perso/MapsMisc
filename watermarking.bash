#!/usr/bin/env bash

# Aim: watermark a photo
# Copyright (C) 2018 Timothée Flutre
# License: AGPL-3+
# Persons: Timothée Flutre [cre,aut]
# Versioning: https://github.com/timflutre-perso

progVersion="1.0.0" # http://semver.org/

# Display the help on stdout.
# The format complies with help2man (http://www.gnu.org/s/help2man)
function help () {
    msg="\`${0##*/}' watermarks a photo.\n"
    msg+="\n"
    msg+="Usage: ${0##*/} [OPTIONS] ...\n"
    msg+="\n"
    msg+="Options:\n"
    msg+="  -h, --help\tdisplay the help and exit\n"
    msg+="  -V, --version\toutput version information and exit\n"
    msg+="  -v, --verbose\tverbosity level (default=0/1/2/3)\n"
    msg+="      --photo\tpath to the input file containing the photo (in JPEG format)\n"
    msg+="\t\toptional\n"
    msg+="      --wtm\tpath to the input file containing the watermark (in PNG format)\n"
    msg+="\t\tcompulsory\n"
    msg+="\t\tif file doesn't exist, will be created\n"
    msg+="      --aut\tauthor of the photo\n"
    msg+="\t\tcompulsory only if watermark file doesn't exist\n"
    msg+="      --year\tyear the photo was taken\n"
    msg+="\t\tcompulsory only if watermark file doesn't exist\n"
    msg+="      --font\tfont for the watermarking (default='Liberation-Sans')\n"
    msg+="\t\tcompulsory only if watermark file doesn't exist\n"
    msg+="      --width\twidth in pixels for the watermarking (default='1500')\n"
    msg+="\t\tcompulsory only if watermark file doesn't exist\n"
    msg+="      --height\theight in pixels for the watermarking (default='250')\n"
    msg+="\t\tcompulsory only if watermark file doesn't exist\n"
    msg+="      --ptsize\tpointsize for the watermarking (default='120')\n"
    msg+="\t\tcompulsory only if watermark file doesn't exist\n"
    msg+="\n"
    msg+="Examples:\n"
    msg+="  ${0##*/} --wtm watermark.png --aut \"R. Capa\" --year 1944\n"
    msg+="  ${0##*/} --photo DSC03611.JPG --wtm watermark.png\n"
    msg+="\n"
    msg+="Report bugs to timflutre@gmail.com."
    echo -e "$msg"
}

# Display version and license information on stdout.
# The person roles comply with R's guidelines (The R Journal Vol. 4/1, June 2012).
function version () {
    msg="${0##*/} ${progVersion}\n"
    msg+="\n"
    msg+="Copyright (C) 2018 Timothée Flutre.\n"
    msg+="License AGPLv3+: GNU AGPL version 3 or later <http://gnu.org/licenses/agpl.html>\n"
    msg+="\n"
    msg+="Written by Timothée Flutre [cre,aut]."
    echo -e "$msg"
}

# http://www.linuxjournal.com/content/use-date-command-measure-elapsed-time
function timer () {
    if [[ $# -eq 0 ]]; then
        echo $(date '+%s')
    else
        local startRawTime=$1
        endRawTime=$(date '+%s')
        if [[ -z "$startRawTime" ]]; then startRawTime=$endRawTime; fi
        elapsed=$((endRawTime - startRawTime)) # in sec
        nbDays=$((elapsed / 86400))
        nbHours=$(((elapsed / 3600) % 24))
        nbMins=$(((elapsed / 60) % 60))
        nbSecs=$((elapsed % 60))
        printf "%01dd %01dh %01dm %01ds" $nbDays $nbHours $nbMins $nbSecs
    fi
}

# Parse the command-line arguments.
# http://stackoverflow.com/a/4300224/597069
function parseCmdLine () {
    getopt -T > /dev/null # portability check (say, Linux or Mac OS?)
    if [ $? -eq 4 ]; then # GNU enhanced getopt is available
	TEMP=`getopt -o hVv: -l help,version,verbose:,photo:,wtm:,aut:,year:,font:,width:,height:,ptsize: \
        -n "$0" -- "$@"`
    else # original getopt is available (no long options, whitespace, sorting)
	TEMP=`getopt hVv:i: "$@"`
    fi
    if [ $? -ne 0 ]; then
	echo "ERROR: "$(which getopt)" failed" 1>&2
	getopt -T > /dev/null
	if [ $? -ne 4 ]; then
	    echo "did you use long options? they are not handled \
on your system, use -h for help"
	fi
	exit 2
    fi
    eval set -- "$TEMP"
    while [ $# -gt 0 ]; do
        case "$1" in
            -h | --help) help; exit 0; shift;;
            -V | --version) version; exit 0; shift;;
            -v | --verbose) verbose=$2; shift 2;;
            --photo) inPhoto=$2; shift 2;;
            --wtm) inWtm=$2; shift 2;;
            --aut) author=$2; shift 2;;
            --year) year=$2; shift 2;;
            --font) font=$2; shift 2;;
            --width) width=$2; shift 2;;
            --height) height=$2; shift 2;;
            --ptsize) ptsize=$2; shift 2;;
            --) shift; break;;
            *) echo "ERROR: options parsing failed, use -h for help" 1>&2; exit 1;;
        esac
    done

    hash composite 2>/dev/null || \
      { echo >&2 "ERROR: composite (from ImageMagick) is not in your PATH"; exit 1; }

    if [ ! -z "${inPhoto}" ]; then
      if [ ! -f "${inPhoto}" ]; then
        echo -e "ERROR: can't find file ${inPhoto}" 1>&2
        exit 1
      fi
    fi

    if [ -z "${inWtm}" ]; then
      echo -e "ERROR: missing compulsory option --wtm" 1>&2
      exit 1
    else
      if [ ! -f "${inWtm}" ]; then
        if [ -z "${author}" ]; then
          echo -e "ERROR: missing compulsory option --aut" 1>&2
          exit 1
        fi
        if [ -z "${year}" ]; then
          echo -e "ERROR: missing compulsory option --year" 1>&2
          exit 1
        fi
      fi
    fi
}

function run () {

  # make the watermark file if it doesn't exist
  # https://www.imagemagick.org/Usage/annotating/#wmark_text
  if [[ ! -f "${inWtm}" ]]; then
    txt="Copyright "${author}", "${year}"."
    if [ $verbose -gt "0" ]; then
      echo ${txt}
    fi

    if [ -f stamp_fgnd.png ]; then
      rm stamp_fgnd.png
    fi
    if [ -f stamp_mask.png ]; then
      rm stamp_mask.png
    fi
    convert -size ${width}x${height} xc:white -font ${font} -pointsize ${ptsize} -gravity center \
            -draw "fill white  text 0,0  '${txt}'" \
            stamp_fgnd.png
    convert -size ${width}x${height} xc:black -font ${font} -pointsize ${ptsize} -gravity center \
            -draw "fill white  text  2,2  '${txt}'  \
                   text  0,0  '${txt}'  \
                   fill black  text -2,-2 '${txt}'" \
            +matte stamp_mask.png
    composite -compose CopyOpacity stamp_mask.png stamp_fgnd.png ${inWtm}
    # mogrify -trim +repage ${inWtm}
    rm stamp_fgnd.png stamp_mask.png
  fi

  # add the watermark image to the photo
  if [ ! -z "${inPhoto}" ]; then
    composite -gravity SouthEast -geometry +5+5 ${inWtm} ${inPhoto} "${inPhoto%.*}"_wm.${inPhoto##*.}
  fi
}

verbose=0
inPhoto=""
inWtm=""
author=""
year=""
font="Liberation-Sans"
width="1500"
height="250"
ptsize="120"
parseCmdLine "$@"

if [ $verbose -gt "0" ]; then
    startTime=$(timer)
    msg="START ${0##*/} ${progVersion} $(date +"%Y-%m-%d") $(date +"%H:%M:%S")"
    msg+="\ncmd-line: $0 "$@ # comment if an option takes a glob as argument
    msg+="\ncwd: $(pwd)"
    echo -e $msg
fi

run inPhoto inWtm author year font width height ptsize verbose

if [ $verbose -gt "0" ]; then
    msg="END ${0##*/} ${progVersion} $(date +"%Y-%m-%d") $(date +"%H:%M:%S")"
    msg+=" ($(timer startTime))"
    echo $msg
fi
