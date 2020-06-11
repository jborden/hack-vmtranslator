(ns hack-vmtranslator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :refer [defparser]]))

(defparser hack-bytecode-parser
  (slurp "resources/bnf/hackbytecode.bnf"))

(defn gen-tokens
  [s]
  (->> s hack-bytecode-parser rest (into [])))

(defn flatten-vector
  "Given a vector, flatten it"
  [v]
  (->> v flatten (into [])))

(def sp++
  ["@SP"
   "D=M"
   "D=D+1"
   "M=D"])

(def sp--
  ["@SP"
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
    [(str "// push constant " i)
     ;; store where we are at in R13
     (:save-pos pos-m)
     (str "@" i)
     "D=A"
     "@push_constant_conserved_fn"
     "0;JMP"
     (:pos-label pos-m)
     (str "// end push constant " i)]))

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
  [(str "// push " segment " " i)
   ;; addr = LCL + i
   (addr-eq-segment-plus-i segment i)
   ;; *sp=*addr
   ;; get the value at addr
   "D=M"
   ;; set the value of *SP to *ADDR
   "@SP"
   "A=M"
   "M=D"
   ;; SP++
   sp++
   (str "// end push " segment " " i)])

(defn pop-memory-fn [segment i]
  [(str "// pop " segment " " i)
   sp--
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
   "M=D"
   (str "// end pop " segment " " i)])

(defn push-temp-i
  [i]
  [(str "// push temp " i)
   ;; addr = 5+i
   (str "@" i)
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
   sp++
   (str "// end push temp " i)])

(defn pop-temp-i
  [i]
  [(str "// pop temp " i)
   sp--
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
   "M=D"
   (str "// end pop temp " i)])

(defn push-pointer [i]
  [(str "// push pointer " i)
   ;; get the value of this or that
   (if (= i "0")
     "@THIS"
     "@THAT")
   "D=M"
   "@SP"
   "A=M"
   "M=D"
   sp++
   (str "// end push pointer" i)])

(defn pop-pointer [i]
  [(str "// pop pointer " i)
   sp--
   ;; get the value of sp
   "@SP"
   "A=M"
   "D=M"
   ;; set this/that equal to the value
   (if (= i "0")
     "@THIS"
     "@THAT")
   "M=D"])

(defn push-static
  [i basename]
  [(str "// push static " i)
   ;; get the value of @<basename>.<i>
   (str "@" basename "." i)
   "D=M"
   ;; set *sp to this value
   "@SP"
   "A=M"
   "M=D"
   sp++
   (str "// end push static " i)])

(defn pop-static
  [i basename]
  [(str "// pop static " i)
   sp--
   ;; get the value of sp
   "@SP"
   "A=M"
   "D=M"
   ;; set this value to @<basename>.<i>
   (str "@" basename "." i)
   "M=D"
   (str "// end pop static " i)])


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
  [(str "// " command)
   (set-D-to-SP-2-value)
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
   "M=M+1"
   (str "// end " command)])

(defn single-operand-command->asm
  [command]
  [(str "// " command)
   (set-D-to-SP-1-value)
   (condp = command
     "not" "M=!D"
     "neg" "M=-D")
   (str "// end " command )])

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
  ["// comparison fn header"
   (mapv base-comparison-fn  ["eq" "gt" "lt"])
   "// end comparison fn header"])

(defn comparison-command->asm
  [command]
  (let [end-comp-label (gensym "end.comp.fn")]
    [(str "// " command)
     (str "@" end-comp-label " // save end-comp-label pointer in R13")
     "D=A"
     "@R13"
     "M=D"
     (double-operand-command->asm "sub")
     (condp = command
       "eq" "@eq_fn // go to the eq_fn"
       "gt" "@gt_fn // go to the gt_fn"
       "lt" "@lt_fn // go to the lt_fn")
     "0;JMP"
     (str "(" end-comp-label ") // ")
     ;; not sure if we should put a throwaway command here like 'D=0'
     (str "// end " command)]))

(defn alu-command
  [v]
  (let [[_ command] v]
    (cond
      (contains? #{"add" "sub" "and" "or"} command) (double-operand-command->asm command)
      (contains? #{"neg" "not"} command) (single-operand-command->asm command)
      (contains? #{"eq" "gt" "lt"} command) (comparison-command->asm command))))

(defn branching-command
  [[_ command [_ label]]]
  (condp = command
    "label" [(str "// label " label)
             (str "(" label ")")]
    "goto" [(str "// goto " label)
            (str "@" label)
            "0;JMP"
            (str "// end goto " label)]
    "if-goto" [(str "// if-goto " label)
               sp--
               ;; get the value after the pop
               "A=M"
               "D=M"
               ;; if D > 0 (e.g. not 0, jmp)
               (str "@" label)
               "D;JGT"
               (str "// end if-goto " label)]))

(defn tokens->asm
  [v env]
  (->> v
       (mapv #(condp = (first %)
                :MEMORY_COMMAND (memory-command % env)
                :ALU_COMMAND (alu-command %)
                :BRANCHING_COMMAND (branching-command %)
                nil))
       ;; shouldn't need this once complete, keep for now
       (filterv (comp not nil?))
       ;;(string/join "\n")
       ))

(defn translate-file
  "Given a filename with hack bytecode, output a .asm output file representing it"
  [filename]
  (let [io-file (io/file filename)
        filepath (.getPath io-file)
        basename (-> (.getName io-file) (string/split #"\.") first)
        output-filename (string/replace filepath #".vm" ".asm")
        input (slurp filepath)
        tokens (gen-tokens input)
        env {:basename basename}]
    (spit output-filename
          (->> (flatten-vector
                ["@begin_program"
                 "0;JMP"
                 (base-comparison-fn-header)
                 push-constant-conserved-fn
                 "(begin_program)"
                 (tokens->asm tokens env)])
               (string/join "\n")))))
