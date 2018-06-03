#!/bin/bash

PROGRAM_DIR=`dirname "$0"`
PROGRAM_DIR=`cd "$PROGRAM_DIR"; pwd`

echo "Cleaning all Compiled .*fas*, .lib, and .lx64fsl files in: " $PROGRAM_DIR

echo -e "Shall I proceed? [y/n]: \c"
read answer
if [ "$answer" != "y" ] && [ "$answer" != "Y" ]
then
	echo "Aborting"
	exit
fi

find $PROGRAM_DIR -name "*.*fas*" | xargs rm -v
find $PROGRAM_DIR -name "*.lib" | xargs rm -v
find $PROGRAM_DIR -name "*.lx*fsl" | xargs rm -v

#echo "Cleaning all Backup Files in: " $PROGRAM_DIR
#
#echo -e "Shall I proceed? [y/n]: \c"
#read answer
#if [ "$answer" != "y" ] && [ "$answer" != "Y" ]
#then
#	echo "Aborting"
#	exit
#fi
#
#find $PROGRAM_DIR -name "*.*~" | xargs rm -v
#find $PROGRAM_DIR -name "*.bak" | xargs rm -v
