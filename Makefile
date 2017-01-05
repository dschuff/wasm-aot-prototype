.SUFFIXES:

ALL = sexpr_dump wat libwart.a
CC ?= gcc
CXX ?= g++

CFLAGS ?= -Wall -Werror -g -O0
# -fno-exceptions and -fno-rtti are required to link with LLVM (which uses these
# flags by default) but we don't want ALL of LLVM's cflags (e.g. warning flags,
# -fPIC)
CXXFLAGS = -std=c++11 -fno-exceptions -fno-rtti
LDFLAGS = -L$(OUT_DIR)
LIBS = -lwabt

OUT_DIR = out

VPATH = src:$(OUT_DIR)

WABT = $(PWD)/third_party/wabt
LIBWABT = $(OUT_DIR)/libwabt.a
WABT_OUT = $(OUT_DIR)/wabt

WASM_CPP_HEADERS = wasm_parser_cxx.h wasm_ast.h ast_visitor.h ast_dumper.h
WASM_CPP_SRCS = wasm_parser_cxx.cc wasm_ast.cc ast_dumper.cc
WASM_CPP_OBJS = $(patsubst %.cc, $(OUT_DIR)/%.o, $(WASM_CPP_SRCS))

WAOT_HEADERS = waot_visitor.h
WAT_SRCS = waot_visitor.cc wat.cc
WAT_OBJS = $(patsubst %.cc, $(OUT_DIR)/%.o, $(WAT_SRCS))


LLVM_PATH ?= /s/llvm-upstream/release_37/install
LLVM_BUILD_PATH ?= $(LLVM_PATH)/../build
LLVM_CONFIG = $(LLVM_PATH)/bin/llvm-config

LLVM_CPPFLAGS := $(shell $(LLVM_CONFIG) --cppflags)
LLVM_LIBS := -lLLVM
# was $(shell $(LLVM_CONFIG) --libs) but this seems broken on mac for dylib
# TODO: Fix for BUILD_SHARED_LIBS
LLVM_LIBDIR := $(shell $(LLVM_CONFIG) --libdir)
LLVM_SYSTEMLIBS := $(shell  $(LLVM_CONFIG) --system-libs)

# When we support Windows, that would probably be the time to turn this makefile
# into something a bit more robust.
OS := $(shell uname)
ifeq ($(OS), Linux)
# Support -DBUILD_SHARED_LIBS on Linux
OS_LDFLAGS := -Wl,-rpath=$(LLVM_LIBDIR) -Wl,--as-needed
else
OS_LDFLAGS := -Wl,-rpath $(LLVM_LIBDIR)
endif

LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags) $(OS_LDFLAGS)

SANITIZE ?=
ifneq ($(SANITIZE),)
	CFLAGS += -fsanitize=$(SANITIZE)
	LDFLAGS += -fsanitize=$(SANITIZE)
endif


.PHONY: all
all: $(OUT_DIR) $(WABT_OUT) $(addprefix $(OUT_DIR)/,$(ALL))

$(OUT_DIR)/:
	mkdir $@

$(WABT_OUT)/:
	mkdir $@

$(OUT_DIR)/%.o: %.c $(LIBWABT)
	$(CC) $(CFLAGS) -I$(WABT)/src -I$(WABT_OUT) -c -Wno-unused-function -Wno-return-type -o $@ $<
$(OUT_DIR)/%.o: %.cc $(LIBWABT) $(WASM_CPP_HEADERS) $(WAOT_HEADERS)
	$(CXX) $(LLVM_CPPFLAGS) $(CXXFLAGS) -I$(WABT)/src -I$(WABT_OUT) -Wno-format $(CFLAGS) -c -o $@ $<


$(LIBWABT): $(WABT)
	(cd $(WABT_OUT) && cmake $(WABT_OUT) $(WABT) -DBUILD_TESTS=OFF -DCMAKE_BUILD_TYPE=Debug)
	make -C $(WABT_OUT) VERBOSE=1
	cp -a $(WABT_OUT)/libwasm.a $@
	cp -a $(WABT_OUT)/wast2wasm $(OUT_DIR)

$(OUT_DIR)/sexpr_dump: out/sexpr_dump.o $(LIBWABT) $(WASM_CPP_OBJS)
	$(CXX) -o $@ out/sexpr_dump.o $(WASM_CPP_OBJS) $(LDFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS) $(LLVM_SYSTEMLIBS) $(LIBS)

$(OUT_DIR)/wat: $(WAT_OBJS) $(WASM_CPP_OBJS)
	$(CXX) -o $@ $(WAT_OBJS) $(WASM_CPP_OBJS) $(LDFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS) $(LLVM_SYSTEMLIBS) $(LIBS)


#### RUNTIME ###
RUNTIME_CC = $(CC)
RUNTIME_CFLAGS = $(CFLAGS) -Wno-unused-function

RUNTIME_SRCS = stdio.c wart_main.c assert.c trap.c float_minmax.c memory.c
RUNTIME_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(RUNTIME_SRCS)) $(OUT_DIR)/membase.o

$(OUT_DIR)/%.o: host/%.c
	$(RUNTIME_CC) $(RUNTIME_CFLAGS) -Isrc -c -o $@ $<
$(OUT_DIR)/%.o: host/%.S
	$(CC) $(RUNTIME_CFLAGS)  -c -o $@ $<

$(OUT_DIR)/libwart.a: $(RUNTIME_OBJS)
	ar rcs $@ $(RUNTIME_OBJS)

.PHONY: runtime
TEST_CC = $(OUT_DIR)/wac.py

$(TEST_CC): src/wac.py
	cp $< $(OUT_DIR)
runtime: $(OUT_DIR)/libwart.a $(TEST_CC)


#### TESTS ####
.PHONY: test
test: $(OUT_DIR) $(OUT_DIR)/sexpr_dump $(OUT_DIR)/wat runtime
	PATH=$(PATH):$(LLVM_PATH)/bin $(LLVM_BUILD_PATH)/bin/llvm-lit -sv test/
#### CLEAN ####
.PHONY: clean
clean:
	rm -rf $(OUT_DIR)
	rm -rf test/*/Output
