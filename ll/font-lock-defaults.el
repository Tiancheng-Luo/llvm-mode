(defface ll/font/accurate-garbage-collection-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for accurate garbage collection intrinsics.")

(defface ll/font/aggregate-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for aggregate operations.")

(defface ll/font/arithmetic-with-overflow-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for arithmetic with overflow intrinsics.")

(defface ll/font/binary-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for binary operations.")

(defface ll/font/boolean-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for boolean constants.")

(defface ll/font/bit-manipulation-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for bit manipulation intrinsics.")

(defface ll/font/bitwise-binary-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for bitwise binary operations.")

(defface ll/font/calling-convention
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for calling conventions.")

(defface ll/font/code-generator-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for code-generator-intrinsics.")

(defface ll/font/conversion-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for conversion operations.")

(defface ll/font/dll-storage-class
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for DLL storage classes.")

(defface ll/font/floating-point-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for floating point constants.")

(defface ll/font/floating-point-type
  '((t
      :inherit 'font-lock-type-face))
  "Font for floating point types.")

(defface ll/font/function-define
  '((t
      :inherit 'font-lock-keyword-face))
  "Font...")

(defface ll/font/function-attribute
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for function attributes.")

(defface ll/font/general-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for general intrinsics.")

(defface ll/font/global-variable
  '((t
      :inherit 'font-lock-variable-name-face))
  "Font for global variables.")

(defface ll/font/half-precision-floating-point-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for half precision floating point intrinsics.")

(defface ll/font/integer-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for integer constants.")

(defface ll/font/integer-type
  '((t
      :inherit 'font-lock-type-face))
  "Font for integer types.")

(defface ll/font/linkage-type
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for linkage types.")

(defface ll/font/local-variable
  '((t
      :inherit 'font-lock-variable-name-face))
  "Font for local variables.")

(defface ll/font/memory-access-and-addressing-operation
  '((t
      :foreground "brightblue"
      :inherit 'font-lock-variable-name-face))
  "Font for memory access and addressing operations.")

(defface ll/font/memory-use-marker-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for memory use marker intrinsics.")

(defface ll/font/null-pointer-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for null pointer constants.")

(defface ll/font/other-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for other operations.")

(defface ll/font/specialized-arithmetic-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for specialized arithmetic intrinsics.")

(defface ll/font/standard-c-library-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for standard C library intrinsics.")

(defface ll/font/structure-constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for structure constants.")

(defface ll/font/structure-type
  '((t
      :inherit 'font-lock-type-face))
  "Font for terminator instructions.")

(defface ll/font/terminator-instruction
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for terminator instructions.")

(defface ll/font/thread-local-storage-model
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for thread local storage models.")

(defface ll/font/trampoline-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for trampoline intrinsics.")

(defface ll/font/undefined-value
  '((t
      :inherit 'font-lock-constant-face))
  "Font for undefined value.")

(defface ll/font/variable-argument-handling-intrinsic
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for variable argument handling intrinsics.")

(defface ll/font/vector-operation
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for vection operations.")

(defface ll/font/visibility-style
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for visibility styles.")

(defface ll/font/void-type
  '((t
      :inherit 'font-lock-type-face))
  "Font for the void type.")

(defconst ll/font-lock-defaults
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
           . 'll/font/linkage-type)

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
           . 'll/font/calling-convention)

        `(,(re/keyword
                "default"
                "hidden"
                "protected"
             )
           . 'll/font/visibility-style)

        `(,(re/keyword
                "dllimport"
                "dllexport"
             )
           . 'll/font/dll-storage-class)

        `(,(re/keyword
                "localdynamic"
                "initialexec"
                "localexec"
             )
           . 'll/font/thread-local-storage-model)

        `(,(re/keyword "type")
           . 'll/font/structure-type)

        '("\\<@[-a-zA-Z$\._][-a-zA-Z$\._0-9]*\\>"
           . 'll/font/global-variable)

        '("\\<%[-a-zA-Z$\._][-a-zA-Z$\._0-9]*\\>"
           . 'll/font/local-variable)

        ; define [linkage] [visibility] [DLLStorageClass]
        ;        [cconv] [ret attrs]
        ;        <ResultType> @<FunctionName> ([argument list])
        ;        [unnamed_addr] [fn Attrs] [section "name"] [comdat $<ComdatName>]
        ;        [align N] [gc] [prefix Constant] [prologue Constant] { ... }

        `(,(re/keyword "define")
           . 'll/font/function-define)

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
           . 'll/font/function-attribute)

        ; Module-Level Inline Assembly
        ; Data Layout
        ; Target Triple

        ;; ------------------- Type System -------------------- ;;

        `(,(re/keyword "void")
           . 'll/font/void-type)

        ; Function Type

        '("\\<i[0-9]+\\>"
           . 'll/font/integer-type)

        `(,(re/keyword
                "half"
                "float"
                "double"
                "fp128"
                "x86_fp80"
                "ppc_fp128"
             )
           . 'll/font/floating-point-type)

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
           . 'll/font/boolean-constant)

        '("\\<[-]?[0-9]+\\>"
           . 'll/font/integer-constant)

        '("\\<[-]?[0-9]+\.[0-9]*\\([eE][-+]?[0-9]+\\)?\\>"
           . 'll/font/floating-point-constant)

        `(,(re/keyword "null")
           . 'll/font/null-pointer-constant)

        `(,(re/keyword "constant")
           . 'll/font/structure-constant)

        ; Array constants
        ; Vector constants
        ; Zero initialization
        ; Metadata node
        ; Global Variable and Function Addresses

        `(,(re/keyword "undef")
           . 'll/font/undefined-value)

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
           . 'll/font/terminator-instruction)

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
           . 'll/font/binary-operation)

        `(,(re/keyword
             "shl"
             "lshr"
             "ashr"
             "and"
             "or"
             "xor")
           . 'll/font/bitwise-binary-operation)

        `(,(re/keyword
             "extractelement"
             "insertelement"
             "shufflevector")
           . 'll/font/vector-operation)

        `(,(re/keyword
             "extractvalue"
             "insertvalue")
           . 'll/font/aggregate-operation)

        `(,(re/keyword
             "alloca"
             "load"
             "store"
             "fence"
             "cmpxchg"
             "atomicrmw"
             "getelementptr")
           . 'll/font/memory-access-and-addressing-operation)

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
           . 'll/font/conversion-operation)

        `(,(re/keyword
             "icmp"
             "fcmp"
             "phi"
             "select"
             "call"
             "va_arg"
             "landingpad")
           . 'll/font/other-operation)

        `(,(re/keyword
             "llvm.va_start"
             "llvm.va_end"
             "llvm.va_copy")
           . 'll/font/variable-argument-handling-intrinsic)

        `(,(re/keyword
             "llvm.gcroot"
             "llvm.gcread"
             "llvm.gcwrite")
           . 'll/font/accurate-garbage-collection-intrinsic)

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
           . 'll/font/code-generator-intrinsic)

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
           . 'll/font/standard-c-library-intrinsic)

        `(,(re/keyword
             "llvm.bswap."
             "llvm.ctpop."
             "llvm.ctlz."
             "llvm.cttz.")
           . 'll/font/bit-manipulation-intrinsic)

        `(,(re/keyword
             "llvm.sadd.with.overflow."
             "llvm.uadd.with.overflow."
             "llvm.usub.with.overflow."
             "llvm.smul.with.overflow."
             "llvm.umul.with.overflow.")
           . 'll/font/arithmetic-with-overflow-intrinsic)

        `(,(re/keyword
             "llvm.fmuladd.")
           . 'll/font/specialized-arithmetic-intrinsic)

        `(,(re/keyword
             "llvm.convert.to.fp16"
             "llvm.convert.from.fp16")
           . 'll/font/half-precision-floating-point-intrinsic)

        ; Debugger Intrinsics
        ; Exception Handling Intrinsics

        `(,(re/keyword
             "llvm.init.trampoline"
             "llvm.adjust.trampoline")
           . 'll/font/trampoline-intrinsic)

        `(,(re/keyword
             "llvm.lifetime.start"
             "llvm.lifetime.end"
             "llvm.invariant.start"
             "llvm.invariant.end")
           . 'll/font/memory-use-marker-intrinsic)

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
           . 'll/font/general-intrinsic)

        ; Stack Map Intrinsics
        )))
  "`font-lock-defaults' for LL mode.")

(provide 'll/font-lock-defaults)
