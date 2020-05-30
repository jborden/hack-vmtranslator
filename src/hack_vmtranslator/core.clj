(ns hack-vmtranslator.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :refer [defparser]]))

(defparser hack-bytecode-parser
  (slurp "resources/bnf/hackbytecode.bnf"))

(defn gen-tokens
  [s]
  (->> s hack-bytecode-parser rest (into [])))

(defn translate-file
  "Given a filename with hack bytecode, output a .asm output file representing it"
  [filename]
  (let [io-file (io/file filename)
        filepath (.getPath io-file)
        output-filename (string/replace filepath #".vm" ".asm")
        input (slurp filepath)
        tokens (gen-tokens input)
        ;; labels-symbols (first-pass tokens)
        ;; first-pass-env (merge labels-symbols predefined-symbols)
        ;; value-symbols (second-pass tokens first-pass-env)
        ;; env (merge first-pass-env value-symbols)
        ]
    tokens
    #_(spit output-filename
            (-> input tokenize
                rest
                (into [])
                (tokens->binary-string env)
                (->> (string/join "\n"))
                (str "\n")))))
