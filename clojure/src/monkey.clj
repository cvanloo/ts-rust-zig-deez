(ns monkey
  (:import [java.lang Character]))

(def kind first)
(def position second)
(def literal #(nth % 2 nil))

(defn letter?
  [^Character ch]
  (or (Character/isLetter ch)
      (= \_ ch)))

(defn digit?
  [^Character ch]
  (Character/isDigit ch))

(defn space?
  [^Character ch]
  (Character/isWhitespace ch))

(defn ch->token
  [ch]
  (case ch
    \{ :lbrace
    \} :rbrace
    \( :lparen
    \) :rparen
    \, :comma
    \; :semicolon
    \+ :plus
    \= :equal
    nil))

(def ch-token? ch->token)

(defn ident->kind
  [ident]
  (case ident
    "fn" :function
    "let" :let
    :ident))

(defn parse
  ([input] (parse input 0))
  ([[ch & rest :as input] pos]
   (cond (empty? input) (list [:eof pos])
         (space? ch) (parse rest (inc pos))
         (ch-token? ch) (cons [(ch->token ch) pos]
                              (lazy-seq (parse rest (inc pos))))
         (digit? ch) (parse input pos digit? :int)
         (letter? ch) (parse input pos letter? nil)
         :else (list [:illegal pos ch])))
  ([input pos pred kind]
   (let [[word rest] (split-with pred input)
         word (apply str word)
         npos (+ pos (count word))
         kind (or kind (ident->kind word))]
     (cons (case kind
             :ident [kind pos word]
             :int [kind pos (Integer/parseInt word)]
             [kind pos])
           (lazy-seq (parse rest npos))))))
