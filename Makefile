.SUFFIXES:

ALL = sexpr-cpp
CC ?= gcc
CXX ?= g++
CFLAGS ?= -Wall -Werror -g -O0
# -fno-exceptions and -fno-rtti are required to link with LLVM (which uses these
# flags by default) but we don't want ALL of LLVM's cflags (e.g. warning flags,
# -fPIC)
CXXFLAGS = -std=c++11 -fno-exceptions -fno-rtti

PARSER_SRC = third_party/sexpr-wasm-prototype/src
OUT_DIR = out

VPATH = $(PARSER_SRC):src:$(OUT_DIR)

PARSER_SRCS = wasm-parse.c
PARSER_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(PARSER_SRCS))
WASMGEN_SRCS = sexpr-wasm.c wasm-gen.c
WASMGEN_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(WASMGEN_SRCS))

PARSER_HEADERS = $(PARSER_SRC)/wasm.h $(PARSER_SRC)/wasm-parse.h $(PARSER_SRC)/hash.h

SEXPR_CPP_HEADERS = wasm-cpp.h wasm-ast.h
SEXPR_CPP_SRCS = wasm-cpp.c sexpr-cpp.c
SEXPR_CPP_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(SEXPR_CPP_SRCS))

LLVM_PATH ?= /s/llvm-upstream/release_37/install
LLVM_CONFIG = $(LLVM_PATH)/bin/llvm-config

LLVM_CPPFLAGS := $(shell $(LLVM_CONFIG) --cppflags)
LLVM_LIBS := $(shell $(LLVM_CONFIG) --libs)
LLVM_LIBDIR := $(shell $(LLVM_CONFIG) --libdir)
LLVM_SYSTEMLIBS := $(shell  $(LLVM_CONFIG) --system-libs)
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags) -Wl,-rpath=$(LLVM_LIBDIR)


.PHONY: all
all: $(addprefix $(OUT_DIR)/,$(ALL))

$(OUT_DIR)/:
	mkdir $@

$(OUT_DIR)/%.o: %.c $(PARSER_HEADERS) $(OUT_DIR)
	$(CC) $(CFLAGS) -I$(PARSER_SRC) -c -o $@ $<
$(OUT_DIR)/%.o: %.cc $(PARSER_HEADERS) $(OUT_DIR)
	$(CXX) $(LLVM_CPPFLAGS) $(CXXFLAGS) -I$(PARSER_SRC) -Wno-format $(CFLAGS) -c -o $@ $<

$(OUT_DIR)/sexpr-wasm: out/sexpr-wasm.o $(PARSER_OBJS) $(WASMGEN_OBJS) $(HEADERS)
	$(CC) -o $@ $(PARSER_OBJS) $(WASMGEN_OBJS)

$(OUT_DIR)/sexpr-cpp: $(PARSER_OBJS) $(SEXPR_CPP_OBJS) $(HEADERS) $(PARSER_OBJS) $(SEXPR_CPP_OJBS)
	$(CXX) -o $@ $(PARSER_OBJS) $(SEXPR_CPP_OBJS) $(LLVM_LDFLAGS) $(LLVM_LIBS)

$(PARSER_SRC)/hash.h: $(PARSER_SRC)/hash.txt
	gperf --compare-strncmp --readonly-tables --struct-type $< --output-file $@

#### TESTS ####
TEST_EXES=$(shell python test/run-tests.py --list-exes)


#### CLEAN ####
.PHONY: clean
clean:
	rm -rf $(OUT_DIR)
