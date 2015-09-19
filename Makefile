.SUFFIXES:

ALL = sexpr_dump
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

WASM_CPP_HEADERS = wasm_parser_cxx.h wasm_ast.h
SEXPR_DUMP_SRCS = wasm_parser_cxx.cc wasm_ast.cc sexpr_dump.cc
SEXPR_DUMP_OBJS = $(patsubst %.cc, $(OUT_DIR)/%.o, $(SEXPR_DUMP_SRCS))

LLVM_PATH ?= /s/llvm-upstream/release_37/install
LLVM_BUILD_PATH ?= /s/llvm-upstream/release_37/build
LLVM_CONFIG = $(LLVM_PATH)/bin/llvm-config

LLVM_CPPFLAGS := $(shell $(LLVM_CONFIG) --cppflags)
LLVM_LIBS := $(shell $(LLVM_CONFIG) --libs)
LLVM_LIBDIR := $(shell $(LLVM_CONFIG) --libdir)
LLVM_SYSTEMLIBS := $(shell  $(LLVM_CONFIG) --system-libs)
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags) -Wl,-rpath=$(LLVM_LIBDIR) -Wl,--as-needed


.PHONY: all
all: $(OUT_DIR) $(addprefix $(OUT_DIR)/,$(ALL))

$(OUT_DIR)/:
	mkdir $@

$(OUT_DIR)/%.o: %.c $(PARSER_HEADERS)
	$(CC) $(CFLAGS) -I$(PARSER_SRC) -c -o $@ $<
$(OUT_DIR)/%.o: %.cc $(PARSER_HEADERS) $(WASM_CPP_HEADERS)
	$(CXX) $(LLVM_CPPFLAGS) $(CXXFLAGS) -I$(PARSER_SRC) -Wno-format $(CFLAGS) -c -o $@ $<

$(OUT_DIR)/sexpr-wasm: out/sexpr-wasm.o $(PARSER_OBJS) $(WASMGEN_OBJS)
	$(CC) -o $@ $(PARSER_OBJS) $(WASMGEN_OBJS)

$(OUT_DIR)/sexpr_dump: $(PARSER_OBJS) $(SEXPR_DUMP_OBJS)
	$(CXX) -o $@ $(PARSER_OBJS) $(SEXPR_DUMP_OBJS) $(LLVM_LDFLAGS) $(LLVM_LIBS)

$(PARSER_SRC)/hash.h: $(PARSER_SRC)/hash.txt
	gperf --compare-strncmp --readonly-tables --struct-type $< --output-file $@

#### TESTS ####
.PHONY: test
test: $(OUT_DIR)/sexpr_dump $(OUT_DIR)/sexpr-wasm
	PATH=$(PATH):$(LLVM_PATH)/bin:`pwd`/$(OUT_DIR) $(LLVM_BUILD_PATH)/bin/llvm-lit -sv test/
#### CLEAN ####
.PHONY: clean
clean:
	rm -rf $(OUT_DIR)
	rm -rf test/Output
