#!/bin/bash

# example usage: $DWIM_WORKSPACE/hu.dwim.web-server/etc/build-sencha.sh $DWIM_WORKSPACE/ext-core-3.3.x/ --output-dir $DWIM_WORKSPACE/hu.dwim.web-server/www/libraries/

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

TEMP=`getopt -o h --long help,output-dir:,hdws: -n "$0" -- "$@"`

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
        	--output-dir) OUTPUT_DIR=$2 ; shift 2
        	      ;;
		--) shift ; break ;;
		*) echo "Internal error at $1!" ; exit 1 ;;
	esac
done

######
### fetch the JSBuild2.jar

if [ ! -f "/tmp/JSBuilder2.jar" ]; then
    echo Downloading JSBuilder2
    cd /tmp
    wget -c http://www.sencha.com/deploy/JSBuilder2.zip
    unzip -o JSBuilder2.zip
    cd -
fi

EXT_HOME=$1

if [ -z "${HDWS_HOME}" ]; then
  HDWS_HOME="`dirname $0`/.."
fi

if [ -z "${OUTPUT_DIR}" ]; then
  OUTPUT_DIR="${HDWS_HOME}/www/libraries/"
fi

EXT_HOME=`absolutize "$EXT_HOME"`
HDWS_HOME=`absolutize "$HDWS_HOME"`
OUTPUT_DIR=`absolutize "$OUTPUT_DIR"`

#if [ -z "${RELEASE_NAME}" ]; then
#  RELEASE_NAME=`cd ${EXT_HOME}; svn info | grep URL: | awk -F '/' '{print $NF}'`
#  RELEASE_NAME=${RELEASE_NAME}-`cd ${EXT_HOME}; svn info | grep Revision: | awk '{print $2}'`
#fi

#echo "Remaining arguments:"
#for arg do echo '--> '"\`$arg'" ; done

echo "Will build ext-core into ${OUTPUT_DIR} now..."
echo "Assuming the following parameters:"
echo "hu.dwim.web-server  - ${HDWS_HOME}"
echo "ext-core            - ${EXT_HOME}"
echo "output dir          - ${OUTPUT_DIR}"
#echo "release name        - ${RELEASE_NAME}"

if [ ! -d "$EXT_HOME" -o ! -d "$HDWS_HOME" ]; then
    echo Some of the paths are not correct!
    echo Hint:
    echo svn co http://svn.extjs.com/svn/ext-core/branches/ext-3.3.x/ ext-core-3.3.x
    exit -1
fi

echo Starting the build now...
echo

# KLUDGE because of weird paths in ext-core.jsb2...
ln --no-dereference --symbolic --force ../ "${EXT_HOME}/src/ext-core"

java -jar /tmp/JSBuilder2.jar --projectFile "${EXT_HOME}/ext-core.jsb2" --homeDir "${OUTPUT_DIR}/sencha/"
