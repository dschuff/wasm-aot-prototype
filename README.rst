WebAssembly AOT Prototype (aka WAOT)
==========

Ahead-of-time compiler and runtime for WebAssembly

Overview
--------

WAOT compiles WebAssembly s-expression files of the style used by the
WebAssembly `spec <https://github.com/WebAssembly/spec>`_ into native executables.

It uses `sexpr-wasm-prototype <https://github.com/WebAssembly/sexpr-wasm-prototype>`_
to parse s-expressions to an AST IR, and generates LLVM IR from that.
It then translates the LLVM to a native object file and links it with a provided
runtime library and the platform's standard C library. It is developed on Linux
but should (most likely?) work on Mac OS as well.

WAOT is intended to be an experimental platform for non-browser, non-Javascript
uses of WebAssembly and inform its design.

Components
----------

``sexpr_dump``
  S-expression dumper: parses an s-expression file into WAOT's AST and dumps it
  out again. Currently no desugaring is done, so files should round-trip.

``wat``
  WebAssembly Translator: reads an s-expression file and outputs an LLVM
  assembly file.

``libwart``
  WebAssembly runtime library: a target library against which compiled
  WebAssembly modules are linked. Includes definitions of functions defined in
  the spec interpreter's 'host' section (e.g. ``stdio.print``) and libgcc-style
  runtime support functions called by generated code (e.g. ``__assert_fail``)

``wac.py``
  WebAssembly Compiler: an end-to-end compiler which runs ``wat`` and LLVM's
  ``llc`` tool to create an object file, and links it against ``libwart.a``



Building and Running
--------------------

Clone as normal, but don't forget to update/init submodules as well::

  $ git clone https://github.com/WebAssembly/wasm-aot-prototype
  $ git submodule update --init

Build or download LLVM. I use a locally-built copy of LLVM 3.7, configured using
CMake's ``BUILD_SHARED_LIBS`` option because it makes for fast link times. The
Makefile's default linker flags are set up for that use case.
Set LLVM_PATH in your environment to a directory containing the ``llvm-config``
program. If you want to run the tests, it must also contain ``llvm-lit`` and
``FileCheck``
(which means it probably needs to be a build directory rather than an install
directory).

Run ``make``. By default output goes into the ``out/`` directory, and current
options for build targets include the above-mentioned components, plus the
``sexpr-wasm`` tool from
`sexpr-wasm-prototype <https://github.com/WebAssembly/sexpr-wasm-prototype>`_,
(which is used for testing) and a ``test`` target.

To run tests, run ``make test``. This will run the tests from the ``test/``
directory using ``llvm-lit`` and ``FileCheck``, which are described in the
`LLVM command guide <http://llvm.org/docs/CommandGuide/index.html>`_


License and Contributing
------------------------

Contributions are welcome, and should follow the same guidelines as the
`WebAssembly design <https://github.com/WebAssembly/design/blob/master/Contributing.md>`_.
The license can be found in the `<LICENSE>`_ file.
