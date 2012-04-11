(ns crawl.HIR
  (:require [clojure.algo.monads :refer [fetch-val m-map set-val m-plus
                                         m-seq fetch-state]]
            [clojure.set :as set]
            [crawl.monad :refer [with-state do-state update-val none-or-more]]
            [crawl.bytecodes :refer :all]))

(def block-end-bytecode (set/union #{ret tableswitch lookupswitch goto goto-w
                                     jsr jsr-w} return-bytecode if-bytecode
                                   invoke-bytecode))
 
(def consume-bytecode
  (do-state
    [{:keys [bytecode current-block bci]} (fetch-state)
     :when (< bci (alength bytecode))
     _ (update-val :block-map assoc bci current-block)
     _ (update-val :bci inc)]
    (aget bytecode bci)))

(def consume-opcode
  (do-state
    [opcode consume-bytecode]
    (bit-and 0xff opcode)))

(defn update-block-map [start end value]
  (fn [{block-map :block-map :as state}]
    [block-map
     (assoc state
            :block-map (reduce #(assoc %1 %2 value)
                               block-map
                               (range start (inc end))))]))

(defn get-block [bci]
  (fn [state]
    [(get-in state [:blocks (get-in state [:block-map bci])])
     state]))

(defn no-block-at? [bci]
  (fn [state]
    (when (or (>= bci (count (:block-map state)))
              (nil? (get (:block-map state) bci))) 
      [true state])))

(defn block-at? [bci]
  (fn [state]
    (when (contains? (:blocks state) bci) 
      [true state])))

(defn expand-block-map [bci block-map]
  (if (> bci (count block-map))
    (reduce #(assoc %1 %2 nil)
            block-map
            (range (count block-map) (inc bci)))
    block-map))

(defn new-block [bci]
  (with-state
    (no-block-at? bci) 
    (update-val :block-map (partial expand-block-map bci))
    (update-val :block-map assoc bci bci) 
    (update-val :blocks assoc-in [bci :start-bci] bci)))

(defn existing-block [bci]
  (do-state
    [_ (block-at? bci)
     block (get-block bci)]
    block))

(defn split-block [bci]
  (do-state
    [old-block (get-block bci)
     :when old-block
     :let [new-block (assoc old-block :start-bci bci) 
           old-block (assoc old-block
                            :end-bci (dec bci)
                            :successors [bci])]
     _ (update-val :blocks assoc
                   (:start-bci old-block) old-block
                   bci new-block)
     _ (update-block-map bci (:end-bci new-block) bci)]
    new-block))

(defn add-block [bci]
  (with-state
    (m-plus (new-block bci) 
            (existing-block bci)
            (split-block bci))))

(defn exception-block [bci]
  (with-state
    (new-block bci)
    (update-val :blocks assoc-in [bci :exception-block] true)))

(def branch-offset
  (do-state
    [a consume-bytecode
     b consume-bytecode
     bci (fetch-val :bci)]
    (bit-or (bit-shift-left a 8)
            (bit-and b 0xff))))

(def wide-branch-offset
  (do-state
    [a consume-bytecode
     b consume-bytecode
     c consume-bytecode
     d consume-bytecode]
    (bit-or (bit-shift-left a 24)
            (bit-shift-left (bit-and b 0xff) 16)
            (bit-shift-left (bit-and c 0xff) 8)
            (bit-and d 0xff))))

(defn set-successors [& targets]
  (do-state
    [current-block (fetch-val :current-block)
     _ (update-val :blocks assoc-in [current-block :successors]
                   (set targets))]
    nil))

(defn set-jsr-successor [target]
  (do-state
    [current-block (fetch-val :current-block)
     _ (update-val :blocks assoc-in [current-block :jsr-successor] target)
     bci (fetch-val :bci)
     _ (update-val :blocks assoc-in [current-block :jsr-return-target] bci)]
    nil))

(defn instruction-len [opcode]
  (cond
    (one-byte-opcode opcode) 1
    (two-byte-opcode opcode) 2
    (three-byte-opcode opcode) 3
    (four-byte-opcode opcode) 4
    (five-byte-opcode opcode) 5))

(def instruction
  (do-state
    [{:keys [bci blocks current-block
             except-check]} (fetch-state)
     opcode consume-opcode 
     :when (and (not (block-end-bytecode opcode))
                (or (= current-block bci)
                    (not (contains? blocks bci))))
     _ (m-seq (repeat (dec (instruction-len opcode)) 
                      consume-bytecode))]
    nil))

(def cross-block-boundary
  (do-state
    [{:keys [current-block bci blocks]} (fetch-state)
     :when (and (contains? blocks bci)
                (not= bci current-block))]
    nil))

(def return-inst
  (do-state
    [opcode consume-opcode 
     :when (return-bytecode opcode)]
    nil))

(def throw-inst
  (do-state
    [opcode consume-opcode 
     :when (= athrow opcode)]
    nil))

(def if-inst
  (do-state
    [bci (fetch-val :bci)
     opcode consume-opcode 
     :when (if-bytecode opcode)
     target branch-offset
     fall-through (fetch-val :bci)
     _ (add-block (+ bci target))
     _ (set-successors fall-through (+ bci target))]
    nil))

(def goto-op
  (do-state
    [bci (fetch-val :bci)
     opcode consume-opcode 
     :when (= goto opcode)
     target branch-offset]  
    (+ bci target)))

(def wide-goto-op
  (do-state
    [bci (fetch-val :bci)
     opcode consume-opcode 
     :when (= goto-w opcode)
     target wide-branch-offset] 
    (+ bci target)))

(def goto-inst
  (do-state
    [target (m-plus goto-op wide-goto-op)
     _ (add-block target)
     _ (set-successors target)]
    nil))

(def jsr-op
  (do-state
    [bci (fetch-val :bci)
     opcode consume-opcode 
     :when (= jsr opcode)
     target branch-offset]  
    (+ bci target)))

(def wide-jsr-op
  (do-state
    [bci (fetch-val :bci)
     opcode consume-opcode 
     :when (= jsr-w opcode)
     target wide-branch-offset] 
    (+ bci target)))

(def jsr-inst
  (do-state
    [target (m-plus jsr-op wide-jsr-op)
     bci (fetch-val :bci)
     _ (add-block target)
     _ (set-jsr-successor target)
     _ (set-successors target)]
    (cond
      ; TODO check options to see if jsr/ret are supported, throw exception
      ; if not
      
      ; TODO convert to JsrNotSupportedBailout
      (= target 0) (throw (Exception. "jsr target bci 0 not allowed")))))

(def ret-inst
  (do-state
    [opcode consume-opcode 
     :when (= ret opcode)
     current-block (fetch-val :current-block)
     _ (update-val :blocks assoc-in [current-block :ends-with-ret] true)
     _ consume-bytecode]
    nil))

(defn invoke-target [opcode]
  (if (= opcode invoke-interface)
    wide-branch-offset
    branch-offset))

(def invoke-inst
  (do-state
    [opcode consume-opcode 
     :when (invoke-bytecode opcode)
     _ (invoke-target opcode) 
     bci (fetch-val :bci)
     _ (set-successors bci)]
    nil))

(def padding
  (do-state
    [bci (fetch-val :bci)
     :let [padding-bytes (bit-and 3 (- 4 (bit-and bci 3)))]
     _ (m-seq (repeat padding-bytes consume-bytecode))]
    nil))

(def tableswitch-inst
  (do-state
    [bci (fetch-val :bci)
     opcode consume-opcode
     :when (= tableswitch opcode)
     _ padding
     default-target wide-branch-offset
     low wide-branch-offset
     high wide-branch-offset
     targets (m-seq (repeat (inc (- high low)) wide-branch-offset))
     :let [successors (map #(+ bci %) (cons default-target targets))]
     _ (apply set-successors successors)]
    nil))

(def lookup-target
  (do-state
    [_ wide-branch-offset
     target wide-branch-offset]
    target))

(def lookupswitch-inst
  (do-state
    [bci (fetch-val :bci)
     opcode consume-opcode
     :when (= lookupswitch opcode)
     _ padding
     default-target wide-branch-offset
     npairs wide-branch-offset
     targets (m-seq (repeat npairs lookup-target))
     :let [successors (map #(+ bci %) (cons default-target targets))]
     _ (apply set-successors successors)]
    nil))

(def block-terminator
  (with-state
    (m-plus cross-block-boundary
            return-inst
            throw-inst
            if-inst
            goto-inst
            tableswitch-inst
            lookupswitch-inst
            jsr-inst
            ret-inst
            invoke-inst)))

(def start-block
  (do-state
    [bci (fetch-val :bci)
     block-map (fetch-val :block-map)
     _ (add-block bci)
     _ (set-val :current-block bci)]
    bci))

(def end-block
  (do-state
    [{:keys [bci current-block]} (fetch-state)
     _ (update-val :blocks assoc-in [current-block :end-bci] (dec bci))
     _ (set-val :current-block nil)]
    nil))

(def basic-block
  (with-state
    start-block
    (none-or-more instruction)
    block-terminator
    end-block))

(def basic-blocks
  (do-state
    [method (fetch-val :method) 
     _ (m-map #(exception-block (.handlerBCI %))
              (.exceptionHandlers method))
     _ (none-or-more basic-block)]
    nil))

(defn make-hir [method options]
  (let [initial-state {:method method
                       :bytecode (.code method)
                       ; :constants (.getConstantPool method)
                       :use-except-prob (.useExceptionProbability options)
                       :bci 0
                       :block-map (vector-of :int)}
        blocks (->> (basic-blocks initial-state)
                 second
                 :blocks)]
    nil))
