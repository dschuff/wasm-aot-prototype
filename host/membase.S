	.type	".wasm_membase",@object # @.membase
	.section	.membase,"aw",@nobits
	.globl	".wasm_membase"
	.align	65536
".wasm_membase":
	.zero	4294967296
	.size	".wasm_membase", 4294967296
