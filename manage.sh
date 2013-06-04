#/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/../../../manageUtils.sh

githubProject sun-simulator

BASE=$HGROOT/programs/system/monitorcontrol

case "$1" in
mirror)
  syncHg
  
;;
esac
