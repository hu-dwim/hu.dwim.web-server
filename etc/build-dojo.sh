#!/bin/bash

# https://dojotoolkit.org/documentation/tutorials/1.10/build/index.html
# example usage: ~/common-lisp/hu.dwim.web-server/etc/build-dojo.sh --dojo ~/workspace/dojotoolkit/ --dojo-release-dir ~/common-lisp/hu.dwim.web-server/www/libraries/ --profile ~/common-lisp/hu.dwim.web-server/etc/dojo-build-profile.js

absolutize ()
{
    echo `readlink -f ${1}`
}

DOJO_GIT_TAG=1.12.2

TEMP=`getopt -o h --long help,dojo:,dojo-release-name:,dojo-release-dir:,hdws:,profile: -n "$0" -- "$@"`

# echo $TEMP

if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

# Note the quotes around $TEMP: they are essential!
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
        	--profile) DOJO_PROFILE=$2 ; shift 2
	                 ;;
		--) shift ; break ;;
		*) echo "Internal error at $1!" ; exit 1 ;;
	esac
done

#echo "Remaining arguments:"
#for arg do echo '--> '"\`$arg'" ; done

if [ -z "${HDWS_HOME}" ]; then
  HDWS_HOME="`dirname $0`/.."
fi

if [ -z "${DOJO_RELEASE_DIR}" ]; then
  DOJO_RELEASE_DIR="${HDWS_HOME}/www/libraries/"
fi

if [ -z "${DOJO_PROFILE}" ]; then
  DOJO_PROFILE="${HDWS_HOME}/etc/dojo-build-profile.js"
fi

if [ -z "${DOJO_HOME}" ]; then
    DOJO_HOME=`dirname ${DOJO_PROFILE}`"/../../../workspace/dojotoolkit/"
fi

HDWS_HOME=`absolutize "$HDWS_HOME"`
DOJO_HOME=`absolutize "$DOJO_HOME"`
DOJO_RELEASE_DIR=`absolutize "$DOJO_RELEASE_DIR"`
DOJO_PROFILE=`absolutize "$DOJO_PROFILE"`

if [ ! -d "$DOJO_HOME" ]; then
    echo Checking out dojotoolkit into "$DOJO_HOME"
    mkdir --parents "$DOJO_HOME"

    for subdir in dojo dijit dojox util; do
        if [ ! -d "${DOJO_HOME}/${subdir}" ]; then
            git clone https://github.com/dojo/${subdir}.git "${DOJO_HOME}/${subdir}"
        fi
    done
fi

for subdir in dojo dijit dojox util; do
    echo "git checkout ${DOJO_GIT_TAG} for ${subdir}"
    git -C "${DOJO_HOME}/${subdir}" checkout -q ${DOJO_GIT_TAG}
done

if [ -z "${DOJO_RELEASE_NAME}" ]; then
    DOJO_RELEASE_NAME=`cd ${DOJO_HOME}/dojo; git describe --tags HEAD`
    #DOJO_RELEASE_NAME=`cd ${DOJO_HOME}; svn info | grep URL: | awk -F '/' '{print $NF}'`
    #DOJO_RELEASE_NAME=${DOJO_RELEASE_NAME}-`cd ${DOJO_HOME}; svn info | grep Revision: | awk '{print $2}'`
fi

echo "Will build dojo into '${DOJO_RELEASE_DIR}' now, with name '${DOJO_RELEASE_NAME}'..."
echo "Assuming the following parameters:"
echo "profile             - $DOJO_PROFILE"
echo "hu.dwim.web-server  - $HDWS_HOME"
echo "dojo                - $DOJO_HOME"
echo "release dir         - $DOJO_RELEASE_DIR"

if [ ! -d "$DOJO_HOME" -o ! -d "$HDWS_HOME" ]; then
    echo Some of the paths are not correct!
    exit -1
fi

echo Starting the dojo build script now...
echo

cd "${DOJO_HOME}/util/buildscripts"
#careful... rm -r ${DOJO_RELEASE_DIR}/dojotoolkit-${DOJO_RELEASE_NAME}
sh ./build.sh --profile "$DOJO_PROFILE" --basePath "${DOJO_HOME}" --version "${DOJO_RELEASE_NAME}" --releaseDir "${DOJO_RELEASE_DIR}" --releaseName "dojotoolkit-${DOJO_RELEASE_NAME}" $@
