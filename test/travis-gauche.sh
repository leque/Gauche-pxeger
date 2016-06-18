#!/bin/sh
set -e

sudo apt-get install -qq -y zlib1g-dev libgdbm-dev slib

name="Gauche-${GAUCHE_VERSION}"
source="$name.tgz"
wget -O "$source" "http://prdownloads.sourceforge.net/gauche/$source"
tar xzvf "$source"
(cd $name; ./configure; make; sudo make install)
