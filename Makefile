.SUFFIXES:

ALL = sexpr_dump waot
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

PARSER_SRCS = wasm-parse.c
PARSER_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(PARSER_SRCS))
WASMGEN_SRCS = sexpr-wasm.c wasm-gen.c
WASMGEN_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(WASMGEN_SRCS))

PARSER_HEADERS = $(PARSER_SRC)/wasm.h $(PARSER_SRC)/wasm-parse.h $(PARSER_SRC)/hash.h

WASM_CPP_HEADERS = wasm_parser_cxx.h wasm_ast.h ast_visitor.h ast_dumper.h
WASM_CPP_SRCS = wasm_parser_cxx.cc wasm_ast.cc ast_dumper.cc
WASM_CPP_OBJS = $(patsubst %.cc, $(OUT_DIR)/%.o, $(WASM_CPP_SRCS))

WAOT_HEADERS = waot_visitor.h
WAOT_SRCS = waot_visitor.cc waot.cc
WAOT_OBJS = $(patsubst %.cc, $(OUT_DIR)/%.o, $(WAOT_SRCS))

LLVM_PATH ?= /s/llvm-upstream/release_37/install
LLVM_BUILD_PATH ?= /s/llvm-upstream/release_37/build
LLVM_CONFIG = $(LLVM_PATH)/bin/llvm-config

LLVM_CPPFLAGS := $(shell $(LLVM_CONFIG) --cppflags)
LLVM_LIBS := $(shell $(LLVM_CONFIG) --libs)
LLVM_LIBDIR := $(shell $(LLVM_CONFIG) --libdir)
LLVM_SYSTEMLIBS := $(shell  $(LLVM_CONFIG) --system-libs)
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags) -Wl,-rpath=$(LLVM_LIBDIR) -Wl,--as-needed

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
	$(CC) $(CFLAGS) -I$(PARSER_SRC) -c -o $@ $<
$(OUT_DIR)/%.o: %.cc $(PARSER_HEADERS) $(WASM_CPP_HEADERS) $(WAOT_HEADERS)
	$(CXX) $(LLVM_CPPFLAGS) $(CXXFLAGS) -I$(PARSER_SRC) -Wno-format $(CFLAGS) -c -o $@ $<

$(OUT_DIR)/sexpr-wasm: out/sexpr-wasm.o $(PARSER_OBJS) $(WASMGEN_OBJS)
	$(CC) -o $@ $(PARSER_OBJS) $(WASMGEN_OBJS) $(LDFLAGS)

$(OUT_DIR)/sexpr_dump: out/sexpr_dump.o $(PARSER_OBJS) $(WASM_CPP_OBJS) 
	$(CXX) -o $@ out/sexpr_dump.o $(PARSER_OBJS) $(WASM_CPP_OBJS) $(LDFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS)

$(OUT_DIR)/waot: $(WAOT_OBJS) $(PARSER_OBJS) $(WASM_CPP_OBJS)
	$(CXX) -o $@ $(WAOT_OBJS) $(PARSER_OBJS) $(WASM_CPP_OBJS) $(LDFLAGS) $(LLVM_LDFLAGS) $(LLVM_LIBS)

$(PARSER_SRC)/hash.h: $(PARSER_SRC)/hash.txt
	gperf --compare-strncmp --readonly-tables --struct-type $< --output-file $@

#### RUNTIME ###
RUNTIME_CC = $(CC)
RUNTIME_CFLAGS = $(CFLAGS) -Wno-unused-function

RUNTIME_SRCS = stdio.c wasm_main.c
RUNTIME_OBJS = $(patsubst %.c, $(OUT_DIR)/%.o, $(RUNTIME_SRCS))

$(OUT_DIR)/%.o: host/%.c
	$(RUNTIME_CC) $(RUNTIME_CFLAGS) -c -o $@ $<

$(OUT_DIR)/libwaot_runtime.a: $(RUNTIME_OBJS)
	ar rcs $@ $(RUNTIME_OBJS)

.PHONY: runtime
TEST_CC = $(OUT_DIR)/waot_test_cc.py

$(TEST_CC): src/waot_test_cc.py
	cp $< $(OUT_DIR)
runtime: $(OUT_DIR)/libwaot_runtime.a $(TEST_CC)


#### TESTS ####
.PHONY: test
test: $(OUT_DIR) $(OUT_DIR)/sexpr_dump $(OUT_DIR)/sexpr-wasm $(OUT_DIR)/waot runtime
	PATH=$(PATH):$(LLVM_PATH)/bin:`pwd`/$(OUT_DIR) $(LLVM_BUILD_PATH)/bin/llvm-lit -sv test/
#### CLEAN ####
.PHONY: clean
clean:
	rm -rf $(OUT_DIR)
	rm -rf test/Output
