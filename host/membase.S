#if defined(__linux__)
	.type	.wasm_membase,@object # @.membase
	.section	.membase,"aw",@nobits
	.globl	".wasm_membase"
	.align	65536
.wasm_membase:
	.zero	4294967296
	.size	.wasm_membase, 4294967296
#elif defined(__APPLE__)
	.globl	"_.wasm_membase"
.zerofill __DATA,.membase,"_.wasm_membase",4294967296,16
#endif
