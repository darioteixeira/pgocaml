#!/bin/bash

DIR=`[ -f /var/run/postgresql ] && echo "/var/run/postgresql" || echo "/tmp"`
DIR=`echo $DIR | sed "s~/~\\\\\\/~g"`

DEFAULT_PASSWORD=

cat PGOCaml_config.ml.in | sed 's/DEFAULT_PORT/5432/' > tmp.$$
cat tmp.$$ | sed -e 's/DEFAULT_USER/"postgres"/' > tmp2.$$
cat tmp2.$$ | sed -e "s/DEFAULT_PASSWORD/\"$DEFAULT_PASSWORD\"/" > tmp3.$$
cat tmp3.$$ | sed -e 's/DEFAULT_COMMENT_SRC_LOC/false/' > tmp4.$$
cat tmp4.$$ | sed -e "s/DEFAULT_UDSD/\"$DIR\"/"
rm *.$$
