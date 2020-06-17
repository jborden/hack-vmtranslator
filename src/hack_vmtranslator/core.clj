(ns hack-vmtranslator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :refer [defparser]]))

(defonce current-function (atom nil))
(defonce current-filename (atom nil))
(defonce call-count (atom nil))

(defparser hack-bytecode-parser
  (slurp "resources/bnf/hackbytecode.bnf"))

(defn gen-tokens
  [s]
  (->> s hack-bytecode-parser rest (into [])))

(defn flatten-vector
  "Given a vector, flatten it"
  [v]
  (->> v flatten (into [])))

(defn sp++
  [& [comment]]
  [(str "@SP" comment)
   "M=M+1"])

(defn sp--
  [& [comment]]
  [(str "@SP" comment)
   "D=M"
   "D=D-1"
   "M=D"])

(defn set-D-to-SP-1-value
  []
  ["@SP // set D to SP - 1"
   "D=M"
   "D=D-1"
   "A=D"
   "D=M // end set D to SP-1" ])

(defn set-D-to-SP-2-value
  []
  ["@SP // set D to SP - 2"
   "D=M"
   "D=D-1"
   "D=D-1"
   "A=D"
   "D=M // end set D to SP - 2"])

(defn position-map
  "Store the current position in @R13 and provide a return label"
  []
  (let [current-pos (gensym "current-pos")]
    {:save-pos
     [(str "@" current-pos)
      "D=A"
      "@R13"
      "M=D"]
     :pos-label [(str "(" current-pos ")")]}))

(def push-constant-conserved-fn
  ["(push_constant_conserved_fn)"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"
   "@R13"
   "A=M"
   "0;JMP"])

(defn push-constant-fn
  [i]
  (let [pos-m (position-map)]
    [;; store where we are at in R13
     (:save-pos pos-m)
     (str "@" i)
     "D=A"
     "@push_constant_conserved_fn"
     "0;JMP"
     (:pos-label pos-m)]))

(defn addr-eq-segment-plus-i
  "Set D (addr) to segment + i"
  [segment i]
  ;; get current i of segment and store it in @R15
  [(condp = segment
     "local" "@LCL"
     "argument" "@ARG"
     "this" "@THIS"
     "that" "@THAT")
   "D=M"
   (str "@" i)
   "D=D+A"
   "A=D"])

(defn push-memory-fn
  [segment i]
  [;; addr = LCL + i
   (addr-eq-segment-plus-i segment i)
   ;; *sp=*addr
   ;; get the value at addr
   "D=M"
   ;; set the value of *SP to *ADDR
   "@SP"
   "A=M"
   "M=D"
   ;; SP++
   (sp++ (str " // end push " segment " " i))])

(defn pop-memory-fn [segment i]
  [(sp-- (str " // pop " segment " " i))
   ;; R14 = *sp
   "@SP"
   "A=M"
   "D=M"
   "@R14"
   "M=D"
   (addr-eq-segment-plus-i segment i)
   "D=A"
   ;; store addr in R15
   "@R15"
   "M=D"
   ;; D = *sp
   "@R14"
   "D=M"
   ;; A = R15 = addr
   "@R15"
   "A=M"
   ;; *addr = *sp
   (str "M=D" " // end pop " segment " " i)])

(defn push-temp-i
  [i]
  [;; addr = 5+i
   (str "@" i " // push temp " i)
   "D=A"
   "@5"
   "D=D+A"
   ;; get the value at addr
   "A=D"
   "D=M"
   ;; set the value of SP to ADDR
   "@SP"
   "A=M"
   "M=D"
   ;; SP++
   (sp++ (str " // end push temp " i))])

(defn pop-temp-i
  [i]
  [(sp-- (str " // pop temp " i))
   ;; R14 = *sp
   "@SP"
   "A=M"
   "D=M"
   "@R14"
   "M=D"
   ;; addr = 5+i
   (str "@" i)
   "D=A"
   "@5"
   "D=D+A"
   ;; *addr = *sp
   ;; store addr in R15
   "@R15"
   "M=D"
   ;; D = *sp
   "@R14"
   "D=M"
   ;; A = R15 = addr
   "@R15"
   "A=M"
   ;; *addr = *sp
   (str "M=D" " // end pop temp " i)])

(defn push-pointer [i]
  [;; get the value of this or that
   (if (= i "0")
     (str "@THIS // push pointer " i)
     (str "@THAT // push pointer " i))
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   (sp++ (str " // end push pointer " i))])

(defn pop-pointer [i]
  [(sp-- (str " // pop pointer" i))
   ;; get the value of sp
   "@SP"
   "A=M"
   "D=M"
   ;; set this/that equal to the value
   (if (= i "0")
     "@THIS"
     "@THAT")
   (str "M=D // end pop pointer " i)])

(defn push-static
  [i basename]
  [;; get the value of @<basename>.<i>
   (str "@" basename "." i " // push static " i)
   "D=M"
   ;; set *sp to this value
   "@SP"
   "A=M"
   "M=D"
   (sp++ (str " // end push static " i))])

(defn pop-static
  [i basename]
  [(sp-- (str " // pop static " i))
   ;; get the value of sp
   "@SP"
   "A=M"
   "D=M"
   ;; set this value to @<basename>.<i>
   (str "@" basename "." i)
   (str "M=D" " // end pop static " i)])


(defn memory-command
  [v env]
  (let [[_ [_ command] [_ segment] [_ i]] v]
    (cond
      (= "push" command)
      (cond (= "constant" segment)
            (push-constant-fn i)
            (contains? #{"local" "argument" "this" "that"} segment)
            (push-memory-fn segment i)
            (= "temp" segment)
            (push-temp-i i)
            (= "pointer" segment)
            (push-pointer i)
            (= "static" segment)
            (push-static i (:basename env)))
      (= "pop" command)
      (cond (= "temp" segment)
            (pop-temp-i i)
            (contains? #{"local" "argument" "this" "that"} segment)
            (pop-memory-fn segment i)
            (= "pointer" segment)
            (pop-pointer i)
            (= "static" segment)
            (pop-static i (:basename env))))))

(defn double-operand-command->asm
  [command]
  [(set-D-to-SP-2-value)
   "@R14 // store SP - 1 in R14"
   "M=D"
   (set-D-to-SP-1-value)
   "@R14"
   (condp = command
     "add" "M=M+D // add operand compute"
     "sub" "M=M-D // sub operand compute"
     "and" "M=D&M // and operand compute"
     "or"  "M=D|M // or operand computer")
   "D=M // store R14"
   "@SP // store R14 in SP - 2"
   "M=M-1"
   "M=M-1"
   "A=M"
   "M=D"
   "@SP // increment SP to top of stack"
   "M=M+1"])

(defn single-operand-command->asm
  [command]
  [(set-D-to-SP-1-value)
   (condp = command
     "not" "M=!D // not"
     "neg" "M=-D // neg")])

;; http://nand2tetris-questions-and-answers-forum.32033.n3.nabble.com/translating-eq-to-asm-td4028370.html
;; 1. Generalize to create functions for eq/gt/lt
;; 2. can be this be as header, or will it eval and need to be footer.. because it is jumping to R13.. which should initialize at 0
(defn base-comparison-fn
  "This function is used generally. It assumes the difference between the two operands has been stored in R14 and the address of the return label has been stored in R13"
  [operator]
  [(str "(" operator "_fn)")
   "@R14 // store difference in D"
   "D=M"
   (str "@" operator "_fn_set_true")
   (condp = operator
     "eq" "D;JEQ"
     "gt" "D;JGT"
     "lt" "D;JLT")
   "@SP // store default result (false) in SP - 1"
   "D=M"
   "D=D-1"
   "A=D"
   "M=0 // SP - 1 = 0"
   "@R13 // R13 is where we left off"
   "A=M"
   "0;JMP"
   (str "(" operator "_fn_set_true) // this is when the comp evals to true")
   "@SP"
   "D=M"
   "D=D-1"
   "A=D"
   "M=-1"
   "@R13 // R13 is where we left off"
   "A=M"
   "0;JMP"])

(defn base-comparison-fn-header
  []
  [(mapv base-comparison-fn  ["eq" "gt" "lt"])])

(defn comparison-command->asm
  [command]
  (let [end-comp-label (gensym "end.comp.fn")]
    [(str "@" end-comp-label " // " command)
     "D=A"
     "@R13"
     "M=D"
     (double-operand-command->asm "sub")
     (condp = command
       "eq" "@eq_fn // go to the eq_fn"
       "gt" "@gt_fn // go to the gt_fn"
       "lt" "@lt_fn // go to the lt_fn")
     "0;JMP"
     (str "(" end-comp-label ") // end " command)]))

(defn alu-command
  [v]
  (let [[_ command] v]
    (cond
      (contains? #{"add" "sub" "and" "or"} command) (double-operand-command->asm command)
      (contains? #{"neg" "not"} command) (single-operand-command->asm command)
      (contains? #{"eq" "gt" "lt"} command) (comparison-command->asm command))))

(defn label-string
  [s]
  (if (seq @current-function)
    (str @current-function "$" s)
    s))

(defn goto-label
  [s]
  (let [label s ;;(label-string s)
        ]
    [(str "@" label " // goto " label)
     (str "0;JMP" " // end goto " label)]))

(defn branching-command
  [[_ command [_ label]]]
  (condp = command
    "label" [(str "(" label ")")]
    "goto" (goto-label label)
    "if-goto" (let [label label;;(label-string label)
                    ]
                [(sp-- (str " // if-goto " label))
                 ;; get the value after the pop
                 "A=M"
                 "D=M"
                 ;; if D > 0 (e.g. not 0, jmp)
                 (str "@" label)
                 (str "D;JNE" " // end if-goto " label)])))

(defn push-0-n-times
  [n]
  (mapv (fn [_] (push-constant-fn "0")) (range 0 n)))

(defn push-pointer-val
  [pointer]
  [(str pointer " // push " pointer)
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   "@SP"
   "M=M+1"])

(defn function-command
  [[_ command [_ function-name] [_ n]]]
  (condp = command
    "call"
    (let [ret-addr-label (gensym "retAddrLabel")]
      [;; push retAddrLabel
       (str "@" ret-addr-label " // call " function-name " " n)
       "D=A"
       "@SP"
       "A=M"
       "M=D"
       "@SP"
       "M=M+1"
       ;; push LCL
       (push-pointer-val "@LCL")
       ;; push ARG
       (push-pointer-val "@ARG")
       ;; push THIS
       (push-pointer-val "@THIS")
       ;; push THAT
       (push-pointer-val "@THAT")
       "@SP // ARG = SP - 5 - nArgs"
       "D=M"
       ;; -5
       (mapv (constantly "D=D-1") (range 0 5))
       ;; - nArgs
       (mapv (constantly "D=D-1") (range (read-string n)))
       "@ARG"
       "M=D"
       ;; LCL = SP
       "@SP"
       "D=M"
       "@LCL"
       "M=D"
       ;; goto functionName
       (goto-label function-name)
       (str "(" ret-addr-label ")" " // end call " function-name " " n)])
    "function"
    (let [_ (reset! current-function function-name) ; set the global state function-name
          ]
      [(str "(" function-name ")")
       (push-0-n-times (read-string n))])
    "return"
    (do
      (reset! current-function nil) ;; not in a function anymore
      [;;endframe = LCL = @R13
       "@LCL // return"
       "D=M"
       "@R13"
       "M=D"
       "@5 // retAddr = *(endFrame - 5) = @R14"
       "D=A"
       "@R13"
       "D=M-D"
       "A=D"
       "D=M"
       "@R14"
       "M=D // *ARG = pop() -- below"
       (sp--)
       "@SP"
       "A=M"
       "D=M"
       "@ARG"
       "A=M"
       "M=D"
       "@ARG // SP = ARG + 1"
       "D=M"
       "D=D+1"
       "@SP"
       "M=D"
       "@R13 // THAT = *(endFrame - 1)"
       "D=M"
       "D=D-1"
       "A=D"
       "D=M"
       "@THAT"
       "M=D"
       "@R13 // THIS = *(endFrame - 2)"
       "D=M"
       "D=D-1"
       "D=D-1"
       "A=D"
       "D=M"
       "@THIS"
       "M=D"
       "@R13 // ARG = *(endFrame - 3)"
       "D=M"
       "D=D-1"
       "D=D-1"
       "D=D-1"
       "A=D"
       "D=M"
       "@ARG"
       "M=D"
       "@R13 // LCL = *(endFrame - 4)"
       "D=M"
       "D=D-1"
       "D=D-1"
       "D=D-1"
       "D=D-1"
       "A=D"
       "D=M"
       "@LCL"
       "M=D"
       "@R14 // goto retAddr = *(endFrame - 5)"
       "D=M"
       "A=D"
       "0;JMP // end return"])))

(defn tokens->asm
  [v env]
  (->> v
       (mapv #(condp = (first %)
                :MEMORY_COMMAND (memory-command % env)
                :ALU_COMMAND (alu-command %)
                :BRANCHING_COMMAND (branching-command %)
                :FUNCTION_COMMAND (function-command %)
                nil))
       ;; shouldn't need this once complete, keep for now
       (filterv (comp not nil?))
       ;;(string/join "\n")
       ))

(def bootstrap-code
  ["@256 // begin bootstrap-code"
   "D=A"
   "@SP"
   "M=D"
   (function-command [nil "call" [nil "Sys.init"] [nil "0"]])])

(defn translate-file
  "Given a filename with hack bytecode, output a .asm output file representing it"
  [f]
  (let [io-file (io/file f)
        dir? (.isDirectory io-file)
        filepath (.getPath io-file)
        output-filename (if dir?
                          (str filepath "/" (-> (.getName io-file)
                                                (string/split #"\.")
                                                first) ".asm")
                          (string/replace filepath #".vm" ".asm"))
        files (-> io-file
                  file-seq)
        compiled-code  (->> files
                            (filter #(re-matches #".*vm" (.getPath %)))
                            (map (fn [f]
                                   [;;(str "// begin file: " (.getName f))
                                    (-> (.getPath f)
                                        slurp
                                        gen-tokens
                                        (tokens->asm {:basename
                                                      (-> (.getName f)
                                                          (string/split #"\.")
                                                          first)}))
                                    ;;(str "// end file: " (.getName f))
                                    ])))
        code (->> (flatten-vector
                   ["@begin_program"
                    "0;JMP"
                    (base-comparison-fn-header)
                    push-constant-conserved-fn
                    "(begin_program)"
                    bootstrap-code
                    compiled-code]))
        no-labels-code (remove (partial re-matches #"\(.*\).*") code)]
    ;;code
    (spit output-filename
          (string/join "\n" code))
    ;;debug-code
    (spit (str output-filename ".asm")
          (string/join "\n" no-labels-code))))
