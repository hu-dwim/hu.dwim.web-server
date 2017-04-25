#!/bin/bash

# https://dojotoolkit.org/documentation/tutorials/1.10/build/index.html
# example usage: ~/common-lisp/hu.dwim.web-server/etc/build-dojo.sh --dojo ~/workspace/dojotoolkit-v1.12/ --dojo-release-dir ~/common-lisp/hu.dwim.web-server/www/libraries/ --profile ~/common-lisp/hu.dwim.web-server/etc/dojo-build-profile.js --locales "en-us,hu"

absolutize ()
{
    echo `readlink -f ${1}`
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
    #DOJO_HOME="`dirname $0`/../../dojotoolkit"
    echo Please provide the checked out dojo dir using the --dojo arg!
    exit 1
fi

if [ -z "${DOJO_PROFILE}" ]; then
  DOJO_PROFILE="${HDWS_HOME}/etc/dojo-build-profile.js"
fi

if [ -z "${DOJO_RELEASE_NAME}" ]; then
    DOJO_RELEASE_NAME=`cd ${DOJO_HOME}/dojo; git describe --tags HEAD`
    #DOJO_RELEASE_NAME=`cd ${DOJO_HOME}; svn info | grep URL: | awk -F '/' '{print $NF}'`
    #DOJO_RELEASE_NAME=${DOJO_RELEASE_NAME}-`cd ${DOJO_HOME}; svn info | grep Revision: | awk '{print $2}'`
fi

HDWS_HOME=`absolutize "$HDWS_HOME"`
DOJO_HOME=`absolutize "$DOJO_HOME"`
DOJO_RELEASE_DIR=`absolutize "$DOJO_RELEASE_DIR"`
DOJO_PROFILE=`absolutize "$DOJO_PROFILE"`

#echo "Remaining arguments:"
#for arg do echo '--> '"\`$arg'" ; done

echo "Will build dojo into '${DOJO_RELEASE_DIR}' now, with name '${DOJO_RELEASE_NAME}'..."
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
#careful... rm -r ${DOJO_RELEASE_DIR}/dojotoolkit-${DOJO_RELEASE_NAME}
sh ./build.sh --profile "$DOJO_PROFILE" --version "${DOJO_RELEASE_NAME}" --releaseDir "${DOJO_RELEASE_DIR}" --releaseName "dojotoolkit-${DOJO_RELEASE_NAME}" $@
#sh ./build.sh action="clean,release" version="${DOJO_RELEASE_NAME}" profileFile="$DOJO_PROFILE" releaseDir="${DOJO_RELEASE_DIR}" releaseName="dojotoolkit-${DOJO_RELEASE_NAME}" copyTests=false layerOptimize=shrinksafe.keepLines localeList="${LOCALE_LIST}"
