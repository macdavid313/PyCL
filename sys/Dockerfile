FROM debian:bookworm-slim as binding_generator

ARG python_version
ARG llvm_version

ENV PYENV_ROOT /usr/local/pyenv
ENV PATH ${PYENV_ROOT}/shims:${PYENV_ROOT}/bin:$PATH

RUN echo "Install system dependencies ..."; \
    set -eux; \
    \
    apt-get update; \
    apt-get install --yes \
        git \
        curl \
        bzip2 \
        cmake \
        llvm-${llvm_version} \
        clang-${llvm_version} \
        libclang-${llvm_version}-dev \
        libclang-cpp${llvm_version}-dev

RUN echo "Building Python ${python_version} ..."; \
    set -eux; \
    \
    git clone https://github.com/pyenv/pyenv.git ${PYENV_ROOT}; \
    apt-get install --yes \
        build-essential \
        libssl-dev \
        zlib1g-dev \
        libbz2-dev \
        libreadline-dev \
        libsqlite3-dev \
        curl \
        libncursesw5-dev \
        xz-utils \
        tk-dev \
        libxml2-dev \
        libxmlsec1-dev \
        libffi-dev \
        liblzma-dev; \
    env CC=clang-${llvm_version} pyenv install ${python_version}; \
    pyenv global ${python_version}; \
    pyenv rehash; \
    python --version; \
    python -m pip install --no-cache-dir --quiet -U pip setuptools

RUN echo "Installing c2ffi ..." \
    set -eux; \
    \
    cd /usr/local/src; \
    git clone https://github.com/rpav/c2ffi.git /usr/local/src/c2ffi -b llvm-${llvm_version}.0.0; \
    cd c2ffi; \
    mkdir -p build; \
    cd build; \
    cmake -DBUILD_CONFIG=Release -DCMAKE_CXX_COMPILER=clang++-${llvm_version} ../; \
    cmake --build . --parallel $(nproc); \
    cmake --build . --target install; \
    cd $HOME; \
    rm -rf /usr/local/src/c2ffi

# Generate bindings manifest
VOLUME /opt/workspace
WORKDIR /opt/workspace
CMD set -eux; \
    \
    export PKG_CONFIG_PATH=$(python -c "import sys; print(sys.prefix)")/lib/pkgconfig; \
    printf "#define PY_SSIZE_T_CLEAN\n#define Py_LIMITED_API ${Py_LIMITED_API}\n#include <Python.h>\n" >wrapper.h; \
    cat wrapper.h; \
    c2ffi wrapper.h \
        --sys-include $(pkg-config --cflags python3 | cut -c3-) \
        --driver sexp \
        --warn-as-error \
        --fail-on-error >libpython.binding.sexp; \
    rm wrapper.h
