(defface llvm/font/atomic-ordering
  '((t
      :foreground "#D65C04"
      :inherit 'font-lock-preprocessor-face))
  "Font for atomic orderings.")

(defface llvm/font/attribute
  '((t
      :inherit 'font-lock-preprocessor-face))
  "Font for attributes.")

(defface llvm/font/attribute-group-id
  '((t
      :foreground "#DECF2C"
      :inherit 'font-lock-preprocessor-face))
  "Font for attribute group identifiers.")

(defface llvm/font/comdat
  '((t
      :foreground "#DE2CC9"
      :inherit 'font-lock-preprocessor-face))
  "Font for COMDAT selection kinds.")

(defface llvm/font/constant
  '((t
      :inherit 'font-lock-constant-face))
  "Font for constants.")

(defface llvm/font/fast-math-flag
  '((t
      :foreground "#D1C62E"
      :inherit 'font-lock-preprocessor-face))
  "Font for fast math flags.")

(defface llvm/font/global
  '((t
      :underline t
      :inherit 'font-lock-variable-name-face))
  "Font for global variables.")

(defface llvm/font/keyword
  '((t
      :inherit 'font-lock-keyword-face))
  "Font for keywords")

(defface llvm/font/label
  '((t
      :foreground "#751A69"
      :inherit 'font-lock-preprocessor-face))
  "Font for labels.")

(defface llvm/font/local
  '((t
      :inherit 'font-lock-variable-name-face))
  "Font for local variables.")

(defface llvm/font/metadata
  '((t
      :foreground "#A49CFB"
      :inherit 'font-lock-variable-name-face))
  "Font for metadata.")

(defface llvm/font/type
  '((t
      :inherit 'font-lock-type-face))
  "Font for constants.")

;; @see http://lists.gnu.org/archive/html/help-gnu-emacs/2014-03/msg00130.html
(defconst llvm/font/atomic-ordering    'llvm/font/atomic-ordering)
(defconst llvm/font/attribute          'llvm/font/attribute)
(defconst llvm/font/attribute-group-id 'llvm/font/attribute-group-id)
(defconst llvm/font/comdat             'llvm/font/comdat)
(defconst llvm/font/constant           'llvm/font/constant)
(defconst llvm/font/fast-math-flag     'llvm/font/fast-math-flag)
(defconst llvm/font/global             'llvm/font/global)
(defconst llvm/font/keyword            'llvm/font/keyword)
(defconst llvm/font/label              'llvm/font/label)
(defconst llvm/font/local              'llvm/font/local)
(defconst llvm/font/metadata           'llvm/font/metadata)
(defconst llvm/font/type               'llvm/font/type)

(defconst llvm/font-lock-defaults
  (eval-when-compile
    (let ( (NAME "\\(?:[-a-zA-Z$\._][-a-zA-Z$\._0-9]*\\|[0-9]+\\)")

           (__attributes (list))
           (__constants  (list))
           (__keywords   (list))
           (__types      (list))
           (__result     (list)))

      (defun style (pattern font)
        (setq __result
          (cons
            (cons pattern font)
            __result)))

      (defun attributes (&rest args) (setq __attributes (nconc args __attributes)))
      (defun constants  (&rest args) (setq __constants  (nconc args __constants )))
      (defun keywords   (&rest args) (setq __keywords   (nconc args __keywords  )))
      (defun types      (&rest args) (setq __types      (nconc args __types     )))

      ;; This directly follows the language reference.
      ;; @see http://llvm.org/docs/LangRef.html

      ; Identifiers

      (style
        (concat "\\<@" NAME "\\>")
        'llvm/font/global)

      (style
        (concat "\\<%" NAME "\\>")
        'llvm/font/local)

      ; Module Structure

      (keywords
        "declare")

      ; Linkage Types

      (attributes
        "private"
        "internal"
        "available_externally"
        "linkonce"
        "weak"
        "common"
        "appending"
        "extern_weak"
        "linkonce_odr"
        "weak_odr"
        "external")

      ; Calling Conventions

      (attributes
        "ccc"
        "fastcc"
        "coldcc"
        ; cc 10
        ; cc 11
        "webkit_jscc"
        "anyregcc"
        "preserve_mostcc"
        "preserve_allcc")

      ; Visibility Styles

      (attributes
        "default"
        "hidden"
        "protected")

      ; DLL Storage Classes

      (attributes
        "dllimport"
        "dllexport")

      ; Thread Local Storage Models

      (attributes
        "localdynamic"
        "initialexec"
        "localexec")

      ; Structure Types

      (keywords
        "type")

      ; Global Variables

      (attributes
        "constant"
        "global"
        "section")

      ; Functions
      ; define [linkage] [visibility] [DLLStorageClass]
      ;        [cconv] [ret attrs]
      ;        <ResultType> @<FunctionName> ([argument list])
      ;        [unnamed_addr] [fn Attrs] [section "name"] [comdat $<ComdatName>]
      ;        [align N] [gc] [prefix Constant] [prologue Constant] { ... }

      (keywords
        "define")

      ; Aliases
      ; Comdats

      (keywords
        "comdat")

      (style
        (concat
          "\\<"
          (regexp-opt
            '( "any"
               "exactmatch"
               "largest"
               "noduplicates"
               "samesize"))
          "\\>")
        'llvm/font/comdat)

      ; Named Metadata

      (style
        (concat "\\<!" NAME "\\>")
        'llvm/font/metadata)

      ; Parameter Attributes

      (attributes
        "zeroext"
        "signext"
        "inreg"
        "byval"
        "inalloca"
        "sret"
        "align" ; <n>
        "noalias"
        "nocapture"
        "nest"
        "returned"
        "nonnull"
        "dereferenceable") ; (<n>)

      ; Garbage Collector Names

      (keywords
        "gc")

      ; Prefix Data
      ; Prologue Data
      ; Attribute Groups

      (style
        (concat "\\<#" NAME "\\>") ; FIXME: doesn't work
        'llvm/font/attribute-group-id)

      ; Function Attributes

      (attributes
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
        "uwtable")

      ;; ; Target-independent attributes:
      ;; attributes #0 = { alwaysinline alignstack=4 }
      ;; ; Target-dependent attributes:
      ;; attributes #1 = { "no-sse" }
      ;; ;  Function @f has attributes: alwaysinline, alignstack=4, and "no-sse".
      ;; define void @f() #0 #1 { ... }

      (keywords
        "attributes")

      ; Module-Level Inline Assembly

      (keywords
        "module"
        "asm")

      ; Data Layout

      (keywords
        "target"
        "datalayout")

      ; Target Triple

      (keywords
        "target" ; Assuming duplicates are OK
        "triple")

      ; Atomic Memory Ordering Constraints

      (style
        (concat
          "\\<"
          (regexp-opt
            '( "unordered"
               "monotonic"
               "acquire"
               "release"
               "acq_rel"
               "seq_cst"))
          "\\>")
        'llvm/font/atomic-ordering)

      ; Fast-Math Flags

      (style
        (concat
          "\\<"
          (regexp-opt
            '( "nnan"
               "ninf"
               "nsz"
               "arcp"
               "fast"))
          "\\>")
        'llvm/font/fast-math-flag)

      ; Use-list Order Directives

      (keywords
        "uselistorder"
        "uselistorder_bb")

      ; Void Type

      (types
        "void")

      ; Function Type
      ; Integer Type

      (style
        "\\<i[0-9]+\\>"
        'llvm/font/type)

      ; Floating Point Types

      (types
        "half"
        "float"
        "double"
        "fp128"
        "x86_fp80"
        "ppc_fp128")

      ; X86_mmx Type

      (types
        "x86_mmx")

      ; Pointer Type

      (style
        "\\*"
        'font-lock-warning-face)

      ; Vector Type
      ; Label Type

      (types
        "label")

      (style
        (concat "\\<" NAME ":\\>") ; No syntax definition in the reference
        'llvm/font/label)

      ; Metadata Type

      (types
        "metadata")

      ; Aggregate Types
      ; Array Type
      ; Structure Type
      ; Opaque Structure Types
      ; Boolean Constants

      (constants
        "true"
        "false")

      ; Integer Constants

      (style
        "\\<[-]?[0-9]+\\>"
        'llvm/font/constant)

      ; Floating point constants

      (style
        "\\<[-]?[0-9]+\.[0-9]*\\([eE][-+]?[0-9]+\\)?\\>"
        'llvm/font/constant)

      ; Null Pointer Constants

      (constants
        "null")

      ; Structure Constants
      ; Array constants
      ; Vector constants
      ; Zero initialization
      ; Metadata node
      ; Global Variable and Function Addresses

      (constants
        "undef")

      ; Inline Assembler Expressions
      ; Metadata
      ; Terminator Instructions

      (keywords
        "ret"
        "br"
        "switch"
        "indirectbr"
        "invoke"
        "resume"
        "unreachable")

      ; Binary Operations

      (keywords
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

      ; Bitwise Binary Operations

      (keywords
        "shl"
        "lshr"
        "ashr"
        "and"
        "or"
        "xor")

      ; Vector Operations

      (keywords
        "extractelement"
        "insertelement"
        "shufflevector")

      ; Aggregate Operations

      (keywords
        "extractvalue"
        "insertvalue")

      ; Memory and Addressing Operations

      (keywords
        "alloca"
        "load"
        "store"
        "fence"
        "cmpxchg"
        "atomicrmw"
        "getelementptr")

      ; Conversion Operations

      (keywords
        "trunc"
        "zext"
        "sext"
        "fptrunc"
        "fpext"
        "fptoui"
        "fptosi"
        "uitofp"
        "sitofp"
        "ptrtoint"
        "inttoptr"
        "bitcast"
        "addrspacecast"

        "to")

      ; Other Operations

      (keywords
        "icmp"
        "fcmp"
        "phi"
        "select"
        "call"
        "va_arg"
        "landingpad")

      ; Variable Argument Handling Intrinsics

      (keywords
        "llvm.va_start"
        "llvm.va_end"
        "llvm.va_copy")

      ; Accurate Garbage Collection Intrinsics

      (keywords
        "llvm.gcroot"
        "llvm.gcread"
        "llvm.gcwrite")

      ; Code Generator Intrinsics

      (keywords
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

      ; Standard C Library Intrinsics

      (keywords
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

      ; Bit Manipulation Intrinsics

      (keywords
        "llvm.bswap."
        "llvm.ctpop."
        "llvm.ctlz."
        "llvm.cttz.")

      ; Arithmetic with Overflow Intrinsics

      (keywords
        "llvm.sadd.with.overflow."
        "llvm.uadd.with.overflow."
        "llvm.usub.with.overflow."
        "llvm.smul.with.overflow."
        "llvm.umul.with.overflow.")

      ; Specialized Arithmetic Intrinsics

      (keywords
        "llvm.fmuladd.")

      ; Half Precision Floating Point Intrinsics

      (keywords
        "llvm.convert.to.fp16"
        "llvm.convert.from.fp16")

      ; Debugger Intrinsics
      ; Exception Handling Intrinsics
      ; Trampoline Intrinsics

      (keywords
        "llvm.init.trampoline"
        "llvm.adjust.trampoline")

      ; Memory Use Marker Intrinsics

      (keywords
        "llvm.lifetime.start"
        "llvm.lifetime.end"
        "llvm.invariant.start"
        "llvm.invariant.end")

      ; General Intrinsics

      (keywords
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

      ; Stack Map Intrinsics

      ;; End of declarations

      (style (concat "\\<" (regexp-opt __attributes) "\\>") 'llvm/font/attribute)
      (style (concat "\\<" (regexp-opt __constants ) "\\>") 'llvm/font/constant )
      (style (concat "\\<" (regexp-opt __keywords  ) "\\>") 'llvm/font/keyword  )
      (style (concat "\\<" (regexp-opt __types     ) "\\>") 'llvm/font/type     )

      __result))

  "`font-lock-defaults' for `llvm-mode'.")

(provide 'llvm/font-lock-defaults)
