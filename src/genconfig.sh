#!/bin/bash

DIR=`[ -f /var/run/postgresql ] && echo "/var/run/postgresql" || echo "/tmp"`
DIR=`echo $DIR | sed -s "s~/~\\\\\\/~g"`
DEFAULT_PASSWORD=

sed PGOCaml_config.ml.in \
		-e 's/DEFAULT_PORT/5432/' \
		-e 's/DEFAULT_USER/"postgres"/' \
		-e "s/DEFAULT_PASSWORD/\"$DEFAULT_PASSWORD\"/" \
		-e 's/DEFAULT_COMMENT_SRC_LOC/false/' \
		-e "s/DEFAULT_UDSD/\"$DIR\"/"
