##############################################################################
##                                                                          ##
##                             GPR UNIT PROVIDER                            ##
##                                                                          ##
##          Copyright (C) 2021-2022, Free Software Foundation, Inc.         ##
##                                                                          ##
## This library is free software;  you can redistribute it and/or modify it ##
## under terms of the  GNU General Public License  as published by the Free ##
## Software  Foundation;  either version 3,  or (at your  option) any later ##
## version. This library is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN# ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            ##
##                                                                          ##
## As a special exception under Section 7 of GPL version 3, you are granted ##
## additional permissions described in the GCC Runtime Library Exception,   ##
## version 3.1, as published by the Free Software Foundation.               ##
##                                                                          ##
## You should have received a copy of the GNU General Public License and    ##
## a copy of the GCC Runtime Library Exception along with this program;     ##
## see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    ##
## <http://www.gnu.org/licenses/>.                                          ##
##                                                                          ##
##############################################################################

# Makefile targets
# ----------------
#
# Build gpr_unit_provider:           make
# Install gpr_unit_provider:         make install

# Variables which can be set:
#
#   prefix                  : root install directory
#   ENABLE_SHARED           : yes / no (or empty)
#   GPR_UNIT_PROVIDER_BUILD : debug release
#   PROCESSORS              : nb parallel compilations (0 to use all cores)
#   TARGET                  : target triplet for cross-compilation

HOST    = $(shell gcc -dumpmachine)
TARGET := $(shell gcc -dumpmachine)

prefix	      := $(dir $(shell which gnatls))..
GPR_UNIT_PROVIDER_BUILD = release
PROCESSORS    = 0
BUILD_DIR     =
ENABLE_SHARED := $(shell gprbuild $(GTARGET) -c -q -p \
	-P$(MAKEPREFIX)config/test_shared 2>/dev/null && echo "yes")
GPRINSTALL    = gprinstall

# target options for cross-build
ifeq ($(HOST),$(TARGET))
GTARGET=
else
GTARGET=--target=$(TARGET)
endif

ifeq ($(ENABLE_SHARED), yes)
   LIB_TYPES=static relocatable static-pic
else
   LIB_TYPES=static
endif

# Used to pass extra options to GPRBUILD, like -d for instance
GPRBUILD_OPTIONS=


GPR_OPTIONS=$(GTARGET) -XGPR_UNIT_PROVIDER_BUILD=${GPR_UNIT_PROVIDER_BUILD}

BUILDER=gprbuild -p -m -j${PROCESSORS} ${GPR_OPTIONS} ${GPRBUILD_OPTIONS}
INSTALLER=${GPRINSTALL} -p -f ${GPR_OPTIONS} --prefix=${prefix}
CLEANER=gprclean -eL -p ${GPR_OPTIONS}
UNINSTALLER=$(INSTALLER) -p -f --uninstall ${GPR_OPTIONS}

.PHONY: force

#########
# build #
#########

all: ${LIB_TYPES:%=build-%}


build-%:
	$(BUILDER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* -P gpr_unit_provider.gpr


###########
# Install #
###########

uninstall:
ifneq (,$(wildcard $(prefix)/share/gpr/manifests/gpr_unit_provider))
	$(UNINSTALLER) gpr_unit_provider.gpr
endif


install: uninstall ${LIB_TYPES:%=install-%}

install-%:
	$(INSTALLER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* \
		--build-name=$* --build-var=LIBRARY_TYPE \
		--build-var=GPR_UNIT_PROVIDER_LIBRARY_TYPE -P gpr_unit_provider.gpr


###########
# Cleanup #
###########

distclean: clean
	rm -rf .build

clean: ${LIB_TYPES:%=clean-%}

clean-%:
	-$(CLEANER) -XLIBRARY_TYPE=$* -XXMLADA_BUILD=$* -P gpr_unit_provider.gpr
