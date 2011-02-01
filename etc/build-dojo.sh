#!/bin/bash

# http://docs.dojocampus.org/build/buildScript
# example usage: $DWIM_WORKSPACE/hu.dwim.web-server/etc/build-dojo.sh --dojo $DWIM_WORKSPACE/dojotoolkit-v1.5/ --dojo-release-dir $DWIM_WORKSPACE/hu.dwim.web-server/www/libraries/ --profile $DWIM_WORKSPACE/hu.dwim.web-server/etc/dojo-build-profile.js --locales "en-us,hu"

absolutize ()
{
  if [ ! -d "$1" ]; then
    echo
    echo "ERROR: '$1' doesn't exist or not a directory!"
    exit -1
  fi

  cd "$1"
  echo `pwd`
  cd - >/dev/null
}

LOCALE_LIST="en-us"

TEMP=`getopt -o h --long help,dojo:,dojo-release-name:,dojo-release-dir:,hdws:,profile:,locales: -n "$0" -- "$@"`

# echo $TEMP

if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

# Note the quotes around `$TEMP': they are essential!
eval set -- "$TEMP"

while true ; do
	case "$1" in
        	-h|--help)	echo TODO usage...
        		        exit 0
        		        ;;
        	--hdws) HDWS_HOME=$2 ; shift 2
        	     ;;
        	--dojo) DOJO_HOME=$2 ; shift 2
        	      ;;
        	--dojo-release-name) DOJO_RELEASE_NAME=$2 ; shift 2
        	      ;;
        	--dojo-release-dir) DOJO_RELEASE_DIR=$2 ; shift 2
        	      ;;
        	--locales) LOCALE_LIST=$2 ; shift 2
        	         ;;
        	--profile) DOJO_PROFILE=$2 ; shift 2
	                 ;;
		--) shift ; break ;;
		*) echo "Internal error at $1!" ; exit 1 ;;
	esac
done

if [ -z "${HDWS_HOME}" ]; then
  HDWS_HOME="`dirname $0`/.."
fi

if [ -z "${DOJO_RELEASE_DIR}" ]; then
  DOJO_RELEASE_DIR="${HDWS_HOME}/www/libraries/"
fi

if [ -z "${DOJO_HOME}" ]; then
  DOJO_HOME="`dirname $0`/../../dojotoolkit"
fi

HDWS_HOME=`absolutize "$HDWS_HOME"`
DOJO_HOME=`absolutize "$DOJO_HOME"`
DOJO_RELEASE_DIR=`absolutize "$DOJO_RELEASE_DIR"`

if [ -z "${DOJO_PROFILE}" ]; then
  DOJO_PROFILE="${HDWS_HOME}/etc/dojo-build-profile.js"
fi

if [ -z "${DOJO_RELEASE_NAME}" ]; then
  DOJO_RELEASE_NAME=`cd $DOJO_HOME; svn info | grep URL: | awk -F '/' '{print $NF}'`
  DOJO_RELEASE_NAME=${DOJO_RELEASE_NAME}-`cd $DOJO_HOME; svn info | grep Revision: | awk '{print $2}'`
fi

#echo "Remaining arguments:"
#for arg do echo '--> '"\`$arg'" ; done

echo "Will build dojo into ${DOJO_RELEASE_DIR} now..."
echo "Assuming the following parameters:"
echo "profile             - $DOJO_PROFILE"
echo "locales             - $LOCALE_LIST"
echo "hu.dwim.web-server  - $HDWS_HOME"
echo "dojo                - $DOJO_HOME"
echo "release dir         - $DOJO_RELEASE_DIR"

if [ ! -d "$DOJO_HOME" -o ! -d "$HDWS_HOME" ]; then
    echo Some of the paths are not correct!
    echo Hint:
    echo svn co http://svn.dojotoolkit.org/src/tags/release-1.5/ dojotoolkit-v1.5/
    echo or
    echo svn co http://svn.dojotoolkit.org/src/trunk/ dojotoolkit/
    exit -1
fi

echo Starting the dojo build script now...
echo

cd "${DOJO_HOME}/util/buildscripts"
sh ./build.sh action="clean,release" version="${DOJO_RELEASE_NAME}" profileFile="$DOJO_PROFILE" releaseDir="${DOJO_RELEASE_DIR}" releaseName="dojotoolkit-${DOJO_RELEASE_NAME}" copyTests=false layerOptimize=shrinksafe.keepLines localeList="${LOCALE_LIST}"
