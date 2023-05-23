##
# PyCL
# Python interoperability for Allegro CL
#
# @file
# @version 0.1


LISP=

DOCKER=podman
PYTHON_VERSION=3.6
Py_LIMITED_API=0x03060000
LLVM_VERSION=14
BINDING_BUILDER_IMAGE=pycl.binding.builder

pycl.fasl: lisp_path sys/capi.cl find_libpython.py
	$(LISP) -W -L pkg.cl -e '(build-pycl)' --kill

find_libpython.py:
	cp pycl/find_libpython.py ./

sys/capi.cl: sys/libpython.binding.sexp lisp_path
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

.PHONY: lisp_path
lisp_path:
	@[ "${LISP}" ] && echo "LISP=${LISP}" || ( echo "LISP is not set"; exit 1 )

.PHONY: clean
clean:
	find . -type f -name "*.fasl" -delete
	rm -f sys/*.sexp find_libpython.py

# end
