# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

DIR=$(if $(wildcard /var/run/postgresql),"/var/run/postgresql","/tmp")

setup.ml: _oasis
	@oasis setup

setup.data: setup.ml

completeclean: setup.ml distclean
	@find ./ -name \*mllib -delete
	@find ./ -name \*mldylib -delete
	@rm -f setup.ml myocamlbuild.ml $(tmpfiles)
	@cat _tags | awk '/OASIS_START$$/{print;skip=1}/OASIS_STOP/{skip=0}{if(skip == 1) next; else print; }' > _tags.tmp
	@mv _tags.tmp _tags

config:
	echo "let default_unix_domain_socket_dir = \"$(DIR)\"" > src/PGOCaml_config.ml

