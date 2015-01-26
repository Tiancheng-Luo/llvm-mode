(require 'llvm/face/atomic-ordering)
(require 'llvm/face/attribute)
(require 'llvm/face/attribute-group-id)
(require 'llvm/face/comdat)
(require 'llvm/face/condition)
(require 'llvm/face/constant)
(require 'llvm/face/fast-math-flag)
(require 'llvm/face/global)
(require 'llvm/face/keyword)
(require 'llvm/face/label)
(require 'llvm/face/local)
(require 'llvm/face/metadata)
(require 'llvm/face/type)

(defcustom llvm/assume-camelcase-types t
  "Whether to assume camelcased identifiers are types."
  :group 'llvm/group
  :type 'boolean)

;; FIXME
;; @see http://lists.gnu.org/archive/html/help-gnu-emacs/2014-03/msg00130.html
(defconst llvm/face/atomic-ordering    'llvm/face/atomic-ordering)
(defconst llvm/face/attribute          'llvm/face/attribute)
(defconst llvm/face/attribute-group-id 'llvm/face/attribute-group-id)
(defconst llvm/face/comdat             'llvm/face/comdat)
(defconst llvm/face/condition          'llvm/face/condition)
(defconst llvm/face/constant           'llvm/face/constant)
(defconst llvm/face/fast-math-flag     'llvm/face/fast-math-flag)
(defconst llvm/face/global             'llvm/face/global)
(defconst llvm/face/keyword            'llvm/face/keyword)
(defconst llvm/face/label              'llvm/face/label)
(defconst llvm/face/local              'llvm/face/local)
(defconst llvm/face/metadata           'llvm/face/metadata)
(defconst llvm/face/type               'llvm/face/type)

(defconst llvm/font-lock-keywords
  (eval-when-compile
    (let* ( (NAME "\\(?:[a-zA-Z$\._][a-zA-Z$\._0-9]*\\|[0-9]+\\)")
            (__attributes (list))
            (__constants  (list))
            (__keywords   (list))
            (__types      (list))
            (__result     (list)))

      (defun style (pattern face)
        (setq __result
          (cons
            (cons pattern face)
            __result)))

      (defun style-words (words face)
        (style (concat "\\<" (regexp-opt words) "\\>") face))

      (defun attributes (&rest args) (setq __attributes (nconc args __attributes)))
      (defun constants  (&rest args) (setq __constants  (nconc args __constants )))
      (defun keywords   (&rest args) (setq __keywords   (nconc args __keywords  )))
      (defun types      (&rest args) (setq __types      (nconc args __types     )))

      ;; This directly follows the language reference.
      ;; @see http://llvm.org/docs/LangRef.html

      ; Identifiers

      (style
        (concat "\\<@" NAME "\\>")
        'llvm/face/global)

      (style
        (concat "\\<@\".+\"\\>")
        'llvm/face/global)

      (style
        (concat "\\<%" NAME "\\>")
        'llvm/face/local)

      (if llvm/assume-camelcase-types
        (style
          "\\<%[A-Z][-a-zA-Z$\._0-9]*\\>"
          'llvm/face/type))

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
        "section"
        "unnamed_addr")

      ; Functions
      ; define [linkage] [visibility] [DLLStorageClass]
      ;        [cconv] [ret attrs]
      ;        <ResultType> @<FunctionName> ([argument list])
      ;        [unnamed_addr] [fn Attrs] [section "name"] [comdat $<ComdatName>]
      ;        [align N] [gc] [prefix Constant] [prologue Constant] { ... }

      (keywords
        "define")

      ; Aliases

      (keywords
        "alias")

      ; Comdats

      (keywords
        "comdat")

      (style-words
        '( "any"
           "exactmatch"
           "largest"
           "noduplicates"
           "samesize")
        'llvm/face/comdat)

      ; Named Metadata

      (style
        (concat "\\<!" NAME "\\>")
        'llvm/face/metadata)

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
        (concat "\\<#" NAME "\\>")
        'llvm/face/attribute-group-id)

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

      (style-words
        '( "unordered"
           "monotonic"
           "acquire"
           "release"
           "acq_rel"
           "seq_cst")
        'llvm/face/atomic-ordering)

      ; Fast-Math Flags

      (style-words
        '( "nnan"
           "ninf"
           "nsz"
           "arcp"
           "fast")
        'llvm/face/fast-math-flag)

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
        'llvm/face/type)

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
        'llvm/face/label)

      ; Metadata Type

      (types
        "metadata")

      ; Aggregate Types
      ; Array Type
      ;   XXX: handled after constants to workaround priority
      ; Structure Type
      ; Opaque Structure Types

      (keywords
        "opaque")

      ; Boolean Constants

      (constants
        "true"
        "false")

      ; Integer Constants - handled in `Floating point constants`
      ; Floating point constants

      (style
        "\\<[-]?[0-9]+\\(?:.[0-9]*\\([eE][-+]?[0-9]+\\)?\\)?\\>"
        'llvm/face/constant)

      ; Array Type
      ;   Spacing determined by what llvm parser accepts

      (style
        "\[ *[0-9]+ +x +.+?\]" ; This would not work correctly with nesting
        'llvm/face/type)

      ; Null Pointer Constants

      (constants
        "null")

      ; Structure Constants
      ; Array constants

      (style
        "\\<[c!]?\\(\".*\"\\)\\>" ; Also handles regular strings
        '(1 'llvm/face/constant))

      ; Vector constants
      ; Zero initialization
      ; Metadata node
      ; Global Variable and Function Addresses
      ; Undefined Values

      (constants
        "undef")

      ; Inline Assembler Expressions

      (attributes
        "sideeffect"
        ; "alignstack" exists
        "inteldialect")

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
        "frem"

        "nuw"
        "nsw"

        "exact")

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
        "getelementptr" "inbounds")

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
        "tail"
        "call"
        "va_arg"
        "landingpad")

      (style-words
        '( "eq"
           "ne"
           "ugt"
           "uge"
           "ult"
           "ule"
           "sgt"
           "sge"
           "slt"
           "sle"

           ; "false" TODO
           "oeq"
           "ogt"
           "oge"
           "olt"
           "ole"
           "one"
           "ord"
           "ueq"
           "ugt"
           "uge"
           "ult"
           "ule"
           "une"
           "uno")
           ; "true") TODO
        'llvm/face/condition)

      ;; End of declarations

      (style-words __attributes 'llvm/face/attribute)
      (style-words __constants  'llvm/face/constant )
      (style-words __keywords   'llvm/face/keyword  )
      (style-words __types      'llvm/face/type     )

      __result))

  "`font-lock-keywords' for `llvm-mode'.")

(provide 'llvm/font-lock-keywords)
