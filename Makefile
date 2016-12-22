.SUFFIXES:

ALL = sexpr_dump wat libwart.a
CC ?= gcc
CXX ?= g++

CFLAGS ?= -Wall -Werror -g -O0
# -fno-exceptions and -fno-rtti are required to link with LLVM (which uses these
# flags by default) but we don't want ALL of LLVM's cflags (e.g. warning flags,
# -fPIC)
CXXFLAGS = -std=c++11 -fno-exceptions -fno-rtti
LDFLAGS =

PARSER_SRC = third_party/sexpr-wasm-prototype/src
OUT_DIR = out

VPATH = $(PARSER_SRC):src:$(OUT_DIR)

PARSER_SRCS = wasm-check.c wasm-lexer.c wasm-parser.c wasm-vector.c wasm.c
PARSER_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(PARSER_SRCS))
WASMGEN_SRCS = sexpr-wasm.c wasm-binary-writer.c wasm-writer.c
WASMGEN_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(WASMGEN_SRCS))

PARSER_HEADERS = $(PARSER_SRC)/wasm.h $(PARSER_SRC)/wasm-parser.h $(PARSER_SRC)/wasm-common.h

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
OS_LDFLAGS :=
endif

LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags) $(OS_LDFLAGS)

SANITIZE ?=
ifneq ($(SANITIZE),)
	CFLAGS += -fsanitize=$(SANITIZE)
	LDFLAGS += -fsanitize=$(SANITIZE)
endif


.PHONY: all
all: $(OUT_DIR) $(addprefix $(OUT_DIR)/,$(ALL))

$(OUT_DIR)/:
	mkdir $@

$(OUT_DIR)/%.o: %.c $(PARSER_HEADERS)
	$(CC) $(CFLAGS) -I$(PARSER_SRC) -c -Wno-unused-function -Wno-return-type -o $@ $<
$(OUT_DIR)/%.o: %.cc $(PARSER_HEADERS) $(WASM_CPP_HEADERS) $(WAOT_HEADERS)
	$(CXX) $(LLVM_CPPFLAGS) $(CXXFLAGS) -I$(PARSER_SRC) -Wno-format $(CFLAGS) -c -o $@ $<

$(OUT_DIR)/sexpr-wasm: out/sexpr-wasm.o $(PARSER_OBJS) $(WASMGEN_OBJS)
	$(CC) -o $@ $(PARSER_OBJS) $(WASMGEN_OBJS) $(LDFLAGS)

$(OUT_DIR)/sexpr_dump: out/sexpr_dump.o $(PARSER_OBJS) $(WASM_CPP_OBJS) 
	$(CXX) -o $@ out/sexpr_dump.o $(PARSER_OBJS) $(WASM_CPP_OBJS) $(LDFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS) $(LLVM_SYSTEMLIBS)

$(OUT_DIR)/wat: $(WAT_OBJS) $(PARSER_OBJS) $(WASM_CPP_OBJS)
	$(CXX) -o $@ $(WAT_OBJS) $(PARSER_OBJS) $(WASM_CPP_OBJS) $(LDFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS) $(LLVM_SYSTEMLIBS)

$(PARSER_SRC)/wasm-keywords.h: $(PARSER_SRC)/wasm-keywords.gperf
	gperf --compare-strncmp --readonly-tables --struct-type $< --output-file $@

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
test: $(OUT_DIR) $(OUT_DIR)/sexpr_dump $(OUT_DIR)/sexpr-wasm $(OUT_DIR)/wat runtime
	PATH=$(PATH):$(LLVM_PATH)/bin $(LLVM_BUILD_PATH)/bin/llvm-lit -sv test/
#### CLEAN ####
.PHONY: clean
clean:
	rm -rf $(OUT_DIR)
	rm -rf test/*/Output
