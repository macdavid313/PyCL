##
# PyCL
# Python interoperability for Allegro CL
#
# @file
# @version 0.1

LISP=

DOCKER=podman
PYTHON_VERSION=3.7
Py_LIMITED_API=0x03070000
LLVM_VERSION=14
BINDING_BUILDER_IMAGE=pycl.binding.builder

pycl.fasl: find_libpython.py
ifndef LISP
	$(error LISP is not set)
endif
	@if [ ! -f "sys/capi.cl" ]; then echo "Please run 'make sys/capi.cl'" && exit 1; fi;
	$(LISP) -W -L pkg.cl -e '(pycl.pkg:build-pycl)' --kill

find_libpython.py:
	cp pycl/find_libpython.py ./

sys/capi.cl: sys/libpython.binding.sexp
ifndef LISP
	$(error LISP is not set)
endif
	$(LISP) -W -L sys/gen.cl --kill >sys/capi.cl

sys/libpython.binding.sexp:
	$(DOCKER) build --build-arg python_version=$(PYTHON_VERSION) \
		--build-arg llvm_version=$(LLVM_VERSION) \
		--tag $(BINDING_BUILDER_IMAGE):py$(PYTHON_VERSION) \
		--file sys/Dockerfile \
		sys/
	$(DOCKER) run --rm --env Py_LIMITED_API=$(Py_LIMITED_API) \
		-v $(shell pwd)/sys:/opt/workspace \
		$(BINDING_BUILDER_IMAGE):py$(PYTHON_VERSION)

.PHONY: clean
clean:
	find . -type f -name "*.fasl" -delete
	rm -f sys/*.sexp find_libpython.py

.DEFAULT_GOAL := pycl.fasl

# end
