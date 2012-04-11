(ns crawl.test.HIR
  (:require [clojure.test :refer :all]
            [crawl.HIR :refer :all]
            [crawl.monad :refer :all]
            [crawl.bytecodes :refer :all]))

(defn make-code [& xs]
  (->> xs
    (map #(if (> % 128) (- % 256) %))
    (map byte)
    (byte-array)))

(defn drop-code [result]
  (update-in result [1] dissoc :bytecode))

(deftest test-consume-bytecode
         (is (= [10 {:bci 1 :block-map {0 5} :current-block 5}] 
                (drop-code
                  (consume-bytecode {:bytecode (make-code 10 11 12)
                                     :bci 0 :current-block 5}))))
         (is (= nil
                (consume-bytecode {:bytecode (make-code) :bci 10}))))

(deftest test-update-block-map
         (is (= [[0 1 2 3 4 5 6] {:block-map [0 1 9 9 9 9 6]}]
                ((update-block-map 2 5 9) {:block-map (vec (range 7))}))))

(deftest test-get-block
         (is (= :test-block
                (first
                  ((get-block 3) {:block-map [0 0 0 15 0 0]
                                  :blocks {15 :test-block}})))))

(deftest test-no-block-at?
         (is (= [true {}] ((no-block-at? 10) {})))
         (is (nil? ((no-block-at? 2) {:block-map [0 0 0 0]}))))

(deftest test-block-at?
         (is (= [true {:blocks {10 :some-block}}]
                ((block-at? 10) {:blocks {10 :some-block}})))
         (is (nil? ((block-at? 10) {:blocks {2 :some-block}}))))

(deftest test-new-block
         (is (= {:blocks {2 {:start-bci 2}}
                 :block-map [nil nil 2]}
                (second ((new-block 2) {:block-map []}))))
         (is (nil? ((new-block 2) {:block-map [0 0 0 0]}))))

(deftest test-existing-block
         (is (= {:blocks {2 :some-block}}
                (second ((existing-block 2) {:blocks {2 :some-block}}))))
         (is (nil? ((existing-block 2) {:blocks {0 :some-block}}))))

(deftest test-split-block
         (is (= [{:start-bci 3 :end-bci 5}
                 {:blocks {3 {:start-bci 3 :end-bci 5}
                           1 {:successors [3] :start-bci 1 :end-bci 2}}
                  :block-map [0 1 1 3 3 3 6]}]
                ((split-block 3) {:block-map [0 1 1 1 1 1 6]
                                  :blocks {1 {:start-bci 1
                                              :end-bci 5}}})))
         (is (nil? ((split-block 3) {:blocks {3 :some-block}}))))

(deftest test-end-block
         (is (= [nil {:blocks {0 {:end-bci 3
                                  :start-bci 0}}
                      :block-map [0 0 0 0]
                      :current-block nil
                      :bci 4}]
                (end-block {:bci 4
                            :block-map [0 0 0 0]
                            :current-block 0
                            :blocks {0 {:start-bci 0}}}))))

(deftest test-exception-block
         (is (= {:block-map [nil nil 2]
                 :blocks {2 {:start-bci 2 :exception-block true}}}
                (second ((exception-block 2) {:block-map []}))))
         (is (nil? ((exception-block 2) {:block-map [0 0 0 0]}))))

(deftest test-branch-offset
         (is (= [256 {:bci 2
                      :current-block 4
                      :block-map [4 4]}]
                (drop-code
                  (branch-offset {:bytecode (make-code 1 0 9) 
                                  :bci 0
                                  :current-block 4
                                  :block-map []}))))
         (is (= [-300 {:bci 2
                       :current-block 4
                       :block-map [4 4]}]
                (drop-code
                  (branch-offset {:bytecode (make-code -2 -44 9) 
                                  :bci 0
                                  :current-block 4
                                  :block-map []})))))

(deftest test-wide-branch-offset
         (is (= [256 {:bci 5
                      :current-block 4
                      :block-map [nil 4 4 4 4]}]
                (drop-code
                  (wide-branch-offset {:bytecode (make-code 0 0 0 1 0 9) 
                                       :bci 1
                                       :current-block 4
                                       :block-map [nil]}))))
         (is (= [-300 {:bci 5
                       :current-block 4
                       :block-map [nil 4 4 4 4]}]
                (drop-code
                  (wide-branch-offset {:bytecode (make-code 0 -1 -1 -2 -44 9) 
                                       :bci 1
                                       :current-block 4
                                       :block-map [nil]})))))

(deftest test-instruction
         (is (= nil
                (instruction {:bytecode (make-code) :bci 2000})))
         (is (= nil
                (instruction {:bytecode (make-code return pop-stack) 
                              :bci 0})))
         (is (= [nil {:bci 1 :block-map {0 5}
                      :blocks {5 :some-block} :current-block 5}]
                (drop-code
                  (instruction {:bytecode (make-code iconst-0 return) :bci 0
                                :blocks {5 :some-block}
                                :current-block 5})))) 
         (is (= [nil {:bci 1 :block-map {0 5} :current-block 5}]
                (drop-code
                  (instruction {:bytecode (make-code iconst-0 return) :bci 0
                                :current-block 5})))) 
         (is (= [nil {:bci 1 :block-map {0 5} :current-block 5
                      :can-trap #{0} :except-check true}]
                (drop-code
                  (instruction {:bytecode (make-code iastore 0) :bci 0
                                :current-block 5 :except-check true})))) 
         (is (= [nil {:bci 2 :block-map {0 5 1 5} :current-block 5}]
                (drop-code
                  (instruction {:bytecode (make-code bipush 0) :bci 0
                                :current-block 5})))) 
         (is (= [nil {:bci 3 :block-map {0 5 1 5 2 5}
                      :current-block 5}]
                (drop-code
                  (instruction {:bytecode (make-code sipush 5 5) :bci 0
                                :current-block 5})))))

(deftest test-cross-block-boundary
         (is (= [nil {:bci 4
                      :current-block 0
                      :blocks {4 :some-block}}]
                (cross-block-boundary {:bci 4
                                       :current-block 0
                                       :blocks {4 :some-block}})))
         (is (nil? (cross-block-boundary {:bci 4
                                          :current-block 0})))
         (is (nil? (cross-block-boundary {:bci 4
                                          :current-block 4
                                          :blocks {4 :some-block}}))))

(deftest test-return-inst
         (is (= [nil {:blocks {2 {:start-bci 2}}
                      :block-map [2]
                      :current-block 2
                      :bci 1}]
                (drop-code
                  (return-inst {:current-block 2
                                :bytecode (make-code return) 
                                :bci 0
                                :block-map []
                                :blocks {2 {:start-bci 2}}})))))

(deftest test-throw-inst
         (is (= [nil {:blocks {2 {:start-bci 2}}
                      :block-map [2]
                      :can-trap #{0}
                      :current-block 2
                      :bci 1}]
                (drop-code
                  (throw-inst {:current-block 2
                               :bytecode (make-code athrow) 
                               :bci 0
                               :block-map []
                               :blocks {2 {:start-bci 2}}})))))

(deftest test-if-inst
         (is (= [nil {:block-map [0 0 0 nil nil 5]
                      :blocks {0 {:start-bci 0 :successors #{3 5}}
                               5 {:start-bci 5}}
                      :bci 3
                      :current-block 0}]
                (drop-code
                  (if-inst {:bytecode (make-code if-null 0 5 pop-stack return
                                                 pop-stack) 
                            :block-map []
                            :blocks {0 {:start-bci 0}}
                            :bci 0
                            :current-block 0})))))

(deftest test-goto-op
         (is (nil? (goto-op {:bytecode (make-code pop-stack 0 1) 
                             :bci 0
                             :current-block 10})))
         (is (= [4 {:bci 3
                    :current-block 10
                    :block-map [10 10 10]}] 
                (drop-code
                  (goto-op {:bytecode (make-code goto 0 4) 
                            :bci 0
                            :block-map []
                            :current-block 10})))))

(deftest test-goto-inst
         (is (= [nil {:block-map [0 0 0 nil nil 5]
                      :blocks {0 {:start-bci 0 :successors #{5}}
                               5 {:start-bci 5}}
                      :bci 3
                      :current-block 0}]
                (drop-code
                  (goto-inst {:bytecode (make-code goto 0 5 pop-stack return
                                                   pop-stack) 
                              :blocks {0 {:start-bci 0}}
                              :block-map []
                              :bci 0
                              :current-block 0})))))

(deftest test-jsr-inst
         (is (= [nil {:block-map [0 0 0 0 0 nil nil 7]
                      :blocks {0 {:start-bci 0 :successors #{7}
                                  :jsr-return-target 5
                                  :jsr-successor 7}
                               7 {:start-bci 7}}
                      :bci 5
                      :current-block 0}]
                (drop-code
                  (jsr-inst {:bytecode (make-code jsr-w 0 0 0 7 pop-stack
                                                  return pop-stack) 
                             :blocks {0 {:start-bci 0}}
                             :block-map []
                             :bci 0
                             :current-block 0})))))

(deftest test-ret-inst
         (is (= [nil {:blocks {2 {:start-bci 2
                                  :ends-with-ret true}}
                      :block-map [2 2]
                      :current-block 2
                      :bci 2}]
                (drop-code
                  (ret-inst {:current-block 2
                             :bytecode (make-code ret 90) 
                             :bci 0
                             :block-map []
                             :blocks {2 {:start-bci 2}}})))))

(deftest test-invoke-inst
         (is (= [nil {:blocks {2 {:start-bci 2
                                  :successors #{3}}}
                      :can-trap #{0}
                      :block-map [2 2 2]
                      :current-block 2
                      :bci 3}]
                (drop-code
                  (invoke-inst {:current-block 2
                                :bytecode (make-code invoke-special 0 12) 
                                :bci 0
                                :block-map []
                                :blocks {2 {:start-bci 2}}}))))
         (is (= [nil {:blocks {2 {:start-bci 2
                                  :successors #{5}}}
                      :can-trap #{0}
                      :block-map [2 2 2 2 2]
                      :current-block 2
                      :bci 5}]
                (drop-code
                  (invoke-inst {:current-block 2
                                :bytecode (make-code invoke-interface 0 0 0 12) 
                                :bci 0
                                :block-map []
                                :blocks {2 {:start-bci 2}}})))))

(deftest test-padding
         (is (= [nil {:bci 4}]
                (drop-code
                  (padding {:bci 4 :bytecode (make-code 0 0 0 0 0)})))) 
         (is (= [nil {:bci 4 :current-block 2 :block-map {3 2}}]
                (drop-code
                  (padding {:bci 3 :bytecode (make-code 0 0 0 0 0)
                            :current-block 2})))) 
         (is (= [nil {:bci 4 :current-block 2 :block-map {2 2 3 2}}]
                (drop-code
                  (padding {:bci 2 :bytecode (make-code 0 0 0 0 0)
                            :current-block 2})))) 
         (is (= [nil {:bci 4 :current-block 2 :block-map {1 2 2 2 3 2}}]
                (drop-code
                  (padding {:bci 1 :bytecode (make-code 0 0 0 0 0)
                            :current-block 2})))))

(deftest test-tableswitch-inst
         (is (= [nil {:blocks {1 {:successors #{4 5 6 7 8}}}
                      :bci 32
                      :block-map [nil nil nil 1 1 1 1 1 1 1 1 1 1 1 1 1
                                  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
                      :current-block 1}]
                (drop-code
                  (tableswitch-inst {:bytecode (make-code
                                                 0 0 0
                                                 tableswitch
                                                 0 0 0 1
                                                 0 0 1 0 0 0 1 3
                                                 0 0 0 2
                                                 0 0 0 3
                                                 0 0 0 4
                                                 0 0 0 5) 
                                     :bci 3
                                     :block-map [nil nil nil]
                                     :current-block 1})))))

(deftest test-lookupswitch-inst
         (is (= [nil {:blocks {1 {:successors #{4 5 7 8}}}
                      :bci 36
                      :block-map [nil nil nil 1 1 1 1 1 1 1 1 1 1 1 1 1
                                  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
                      :current-block 1}]
                (drop-code
                  (lookupswitch-inst {:bytecode (make-code
                                                  0 0 0
                                                  lookupswitch
                                                  0 0 0 1  0 0 0 3
                                                  1 1 1 3  0 0 0 2
                                                  1 1 1 3  0 0 0 4
                                                  2 2 2 3  0 0 0 5) 
                                      :bci 3
                                      :block-map [nil nil nil]
                                      :current-block 1})))))

(deftest test-basic-block
         (is (= {0 {:end-bci 4, :successors #{22}, :start-bci 0} 
                 5 {:end-bci 8, :successors #{9 15}, :start-bci 5}
                 9 {:end-bci 14, :successors #{22}, :start-bci 9}
                 15 {:end-bci 18, :successors #{19 22}, :start-bci 15}
                 19 {:end-bci 21 :start-bci 19}
                 22 {:end-bci 25, :successors #{5 26}, :start-bci 22}
                 26 {:end-bci 27 :start-bci 26}}
                (:blocks
                  (second
                    ((none-or-more basic-block)
                       {:bytecode (make-code
                                    #_0 iconst-0 istore-2 goto 0 20
                                    #_5 iload-0 ifle 0 9
                                    #_9 iinc 2 1 goto 0 10
                                    #_15 iload-0 ifge 0 6
                                    #_19 iinc 2 -1 iload-1 ifgt -1 -18
                                    #_26 iload-2 ireturn) 
                        :block-map []
                        :bci 0}))))))

