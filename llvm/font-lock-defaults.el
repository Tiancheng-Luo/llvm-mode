(defface llvm/font/accurate-garbage-collection-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for accurate garbage collection intrinsics.")

(defface llvm/font/aggregate-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for aggregate operations.")

(defface llvm/font/arithmetic-with-overflow-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for arithmetic with overflow intrinsics.")

(defface llvm/font/binary-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for binary operations.")

(defface llvm/font/boolean-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for boolean constants.")

(defface llvm/font/bit-manipulation-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for bit manipulation intrinsics.")

(defface llvm/font/bitwise-binary-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for bitwise binary operations.")

(defface llvm/font/calling-convention
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for calling conventions.")

(defface llvm/font/code-generator-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for code-generator-intrinsics.")

(defface llvm/font/conversion-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for conversion operations.")

(defface llvm/font/dll-storage-class
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for DLL storage classes.")

(defface llvm/font/floating-point-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for floating point constants.")

(defface llvm/font/floating-point-type
  '((t
      :inherit 'font-lock-type-face))
  "Font for floating point types.")

(defface llvm/font/function-define
  '((t
      :inherit 'font-lock-keyword-face))
  "Font...")

(defface llvm/font/function-attribute
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for function attributes.")

(defface llvm/font/general-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for general intrinsics.")

(defface llvm/font/global-variable
  '((t
      :inherit 'font-lock-variable-name-face))
  "Font for global variables.")

(defface llvm/font/half-precision-floating-point-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for half precision floating point intrinsics.")

(defface llvm/font/integer-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for integer constants.")

(defface llvm/font/integer-type
  '((t
      :inherit 'font-lock-type-face))
  "Font for integer types.")

(defface llvm/font/linkage-type
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for linkage types.")

(defface llvm/font/local-variable
  '((t
      :inherit 'font-lock-variable-name-face))
  "Font for local variables.")

(defface llvm/font/memory-access-and-addressing-operation
  '((t
      :foreground "brightblue"
      :inherit 'font-lock-variable-name-face))
  "Font for memory access and addressing operations.")

(defface llvm/font/memory-use-marker-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for memory use marker intrinsics.")

(defface llvm/font/null-pointer-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for null pointer constants.")

(defface llvm/font/other-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for other operations.")

(defface llvm/font/specialized-arithmetic-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for specialized arithmetic intrinsics.")

(defface llvm/font/standard-c-library-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for standard C library intrinsics.")

(defface llvm/font/structure-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for structure constants.")

(defface llvm/font/structure-type
  '((t
      :inherit 'font-lock-type-face))
  "Font for terminator instructions.")

(defface llvm/font/terminator-instruction
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for terminator instructions.")

(defface llvm/font/thread-local-storage-model
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for thread local storage models.")

(defface llvm/font/trampoline-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for trampoline intrinsics.")

(defface llvm/font/undefined-value
  '((t
      :inherit 'font-lock-constant-face))
  "Font for undefined value.")

(defface llvm/font/variable-argument-handling-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for variable argument handling intrinsics.")

(defface llvm/font/vector-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for vection operations.")

(defface llvm/font/visibility-style
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for visibility styles.")

(defface llvm/font/void-type
  '((t
      :inherit 'font-lock-type-face))
  "Font for the void type.")

(defconst llvm/font-lock-defaults
  (eval-when-compile
    (require 're)

    (let ()
      (list
        ;; This directly follows the language reference
        ;; @see http://llvm.org/docs/LangRef.html

        ;; ------------------- High Level Structure -------------------- ;;

        `(,(re/keyword
                "appending"
                "available_externally"
                "common"
                "external"
                "extern_weak"
                "internal"
                "linkonce"
                "linkonce_odr"
                "private"
                "weak"
                "weak_odr"
             )
           . 'llvm/font/linkage-type)

        `(,(re/keyword
                "ccc"
                "fastcc"
                "coldcc"
                ; cc 10
                ; cc 11
                "webkit_jscc"
                "anyregcc"
                "preserve_mostcc"
                "preserve_allcc"
                ; cc <n>
             )
           . 'llvm/font/calling-convention)

        `(,(re/keyword
                "default"
                "hidden"
                "protected"
             )
           . 'llvm/font/visibility-style)

        `(,(re/keyword
                "dllimport"
                "dllexport"
             )
           . 'llvm/font/dll-storage-class)

        `(,(re/keyword
                "localdynamic"
                "initialexec"
                "localexec"
             )
           . 'llvm/font/thread-local-storage-model)

        `(,(re/keyword "type")
           . 'llvm/font/structure-type)

        '("\\<@[-a-zA-Z$\._][-a-zA-Z$\._0-9]*\\>"
           . 'llvm/font/global-variable)

        '("\\<%[-a-zA-Z$\._][-a-zA-Z$\._0-9]*\\>"
           . 'llvm/font/local-variable)

        ; define [linkage] [visibility] [DLLStorageClass]
        ;        [cconv] [ret attrs]
        ;        <ResultType> @<FunctionName> ([argument list])
        ;        [unnamed_addr] [fn Attrs] [section "name"] [comdat $<ComdatName>]
        ;        [align N] [gc] [prefix Constant] [prologue Constant] { ... }

        `(,(re/keyword "define")
           . 'llvm/font/function-define)

        ; Aliases
        ; Comdats
        ; Named Metadata
        ; Parameter Attributes
        ; Garbage Collector Names
        ; Prefix Data
        ; Prologue Data
        ; Attribute Groups

        `(,(re/keyword
                "alignstack"
                "alwaysinline"
                "builtin"
                "cold"
                "inlinehint"
                "jumptable"
                "minsize"
                "naked"
                "nobuiltin"
                "noduplicate"
                "noimplicitfloat"
                "noinline"
                "nonlazybind"
                "noredzone"
                "noreturn"
                "nounwind"
                "optnone"
                "optsize"
                "readnone"
                "readonly"
                "returns_twice"
                "sanitize_address"
                "sanitize_memory"
                "sanitize_thread"
                "ssp"
                "sspreq"
                "sspstrong"
                "uwtable"
             )
           . 'llvm/font/function-attribute)

        ; Module-Level Inline Assembly
        ; Data Layout
        ; Target Triple

        ;; ------------------- Type System -------------------- ;;

        `(,(re/keyword "void")
           . 'llvm/font/void-type)

        ; Function Type

        '("\\<i[0-9]+\\>"
           . 'llvm/font/integer-type)

        `(,(re/keyword
                "half"
                "float"
                "double"
                "fp128"
                "x86_fp80"
                "ppc_fp128"
             )
           . 'llvm/font/floating-point-type)

        ;; ------------------- Constants -------------------- ;;

        ; X86_mmx Type
        ; Pointer Type
        ; Vector Type
        ; Label Type
        ; Metadata Type
        ; Aggregate Types
        ; Array Type
        ; Structure Type
        ; Opaque Structure Types

        `(,(re/keyword
             "true"
             "false")
           . 'llvm/font/boolean-constant)

        '("\\<[-]?[0-9]+\\>"
           . 'llvm/font/integer-constant)

        '("\\<[-]?[0-9]+\.[0-9]*\\([eE][-+]?[0-9]+\\)?\\>"
           . 'llvm/font/floating-point-constant)

        `(,(re/keyword "null")
           . 'llvm/font/null-pointer-constant)

        `(,(re/keyword "constant")
           . 'llvm/font/structure-constant)

        ; Array constants
        ; Vector constants
        ; Zero initialization
        ; Metadata node
        ; Global Variable and Function Addresses

        `(,(re/keyword "undef")
           . 'llvm/font/undefined-value)

        ; Inline Assembler Expressions
        ; Metadata

        `(,(re/keyword
             "ret"
             "br"
             "switch"
             "indirectbr"
             "invoke"
             "resume"
             "unreachable")
           . 'llvm/font/terminator-instruction)

        `(,(re/keyword
             "add"
             "fadd"
             "sub"
             "fsub"
             "mul"
             "fmul"
             "udiv"
             "sdiv"
             "fdiv"
             "urem"
             "srem"
             "frem")
           . 'llvm/font/binary-operation)

        `(,(re/keyword
             "shl"
             "lshr"
             "ashr"
             "and"
             "or"
             "xor")
           . 'llvm/font/bitwise-binary-operation)

        `(,(re/keyword
             "extractelement"
             "insertelement"
             "shufflevector")
           . 'llvm/font/vector-operation)

        `(,(re/keyword
             "extractvalue"
             "insertvalue")
           . 'llvm/font/aggregate-operation)

        `(,(re/keyword
             "alloca"
             "load"
             "store"
             "fence"
             "cmpxchg"
             "atomicrmw"
             "getelementptr")
           . 'llvm/font/memory-access-and-addressing-operation)

        `(,(re/keyword
             "trunc"         ; .. to
             "zext"          ; .. to
             "sext"          ; .. to
             "fptrunc"       ; .. to
             "fpext"         ; .. to
             "fptoui"        ; .. to
             "fptosi"        ; .. to
             "uitofp"        ; .. to
             "sitofp"        ; .. to
             "ptrtoint"      ; .. to
             "inttoptr"      ; .. to
             "bitcast"       ; .. to
             "addrspacecast"); .. to
           . 'llvm/font/conversion-operation)

        `(,(re/keyword
             "icmp"
             "fcmp"
             "phi"
             "select"
             "call"
             "va_arg"
             "landingpad")
           . 'llvm/font/other-operation)

        `(,(re/keyword
             "llvm.va_start"
             "llvm.va_end"
             "llvm.va_copy")
           . 'llvm/font/variable-argument-handling-intrinsic)

        `(,(re/keyword
             "llvm.gcroot"
             "llvm.gcread"
             "llvm.gcwrite")
           . 'llvm/font/accurate-garbage-collection-intrinsic)

        `(,(re/keyword
             "llvm.returnaddress"
             "llvm.frameaddress"
             "llvm.read_register"
             "llvm.write_register"
             "llvm.stacksave"
             "llvm.stackrestore"
             "llvm.prefetch"
             "llvm.pcmarker"
             "llvm.readcyclecounter"
             "llvm.clear_cache"
             "llvm.instrprof_increment")
           . 'llvm/font/code-generator-intrinsic)

        `(,(re/keyword
             "llvm.memcpy"
             "llvm.memmove"
             "llvm.memset."
             "llvm.sqrt."
             "llvm.powi."
             "llvm.sin."
             "llvm.cos."
             "llvm.pow."
             "llvm.exp."
             "llvm.exp2."
             "llvm.log."
             "llvm.log10."
             "llvm.log2."
             "llvm.fma."
             "llvm.fabs."
             "llvm.minnum."
             "llvm.maxnum."
             "llvm.copysign."
             "llvm.floor."
             "llvm.ceil."
             "llvm.trunc."
             "llvm.rint."
             "llvm.nearbyint."
             "llvm.round.")
           . 'llvm/font/standard-c-library-intrinsic)

        `(,(re/keyword
             "llvm.bswap."
             "llvm.ctpop."
             "llvm.ctlz."
             "llvm.cttz.")
           . 'llvm/font/bit-manipulation-intrinsic)

        `(,(re/keyword
             "llvm.sadd.with.overflow."
             "llvm.uadd.with.overflow."
             "llvm.usub.with.overflow."
             "llvm.smul.with.overflow."
             "llvm.umul.with.overflow.")
           . 'llvm/font/arithmetic-with-overflow-intrinsic)

        `(,(re/keyword
             "llvm.fmuladd.")
           . 'llvm/font/specialized-arithmetic-intrinsic)

        `(,(re/keyword
             "llvm.convert.to.fp16"
             "llvm.convert.from.fp16")
           . 'llvm/font/half-precision-floating-point-intrinsic)

        ; Debugger Intrinsics
        ; Exception Handling Intrinsics

        `(,(re/keyword
             "llvm.init.trampoline"
             "llvm.adjust.trampoline")
           . 'llvm/font/trampoline-intrinsic)

        `(,(re/keyword
             "llvm.lifetime.start"
             "llvm.lifetime.end"
             "llvm.invariant.start"
             "llvm.invariant.end")
           . 'llvm/font/memory-use-marker-intrinsic)

        `(,(re/keyword
             "llvm.var.annotation"
             "llvm.ptr.annotation."
             "llvm.annotation."
             "llvm.trap"
             "llvm.debugtrap"
             "llvm.stackprotector"
             "llvm.stackprotectorcheck"
             "llvm.objectsize"
             "llvm.expect"
             "llvm.assume"
             "llvm.donothing")
           . 'llvm/font/general-intrinsic)

        ; Stack Map Intrinsics
        )))
  "`font-lock-defaults' for `llvm-mode'.")

(provide 'llvm/font-lock-defaults)
