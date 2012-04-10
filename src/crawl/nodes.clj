(ns crawl.nodes
  (:use [clojure.pprint :only [pprint]])
  (:import (com.oracle.graal.nodes BeginNode LocalNode FrameState InvokeNode
                                   ReturnNode IfNode ConstantNode EndNode
                                   MergeNode PhiNode LoopBeginNode
                                   DeoptimizeNode LoopEndNode AnchorNode
                                   MaterializeNode)
           (com.oracle.graal.nodes.java MonitorEnterNode NewInstanceNode
                                        MethodCallTargetNode MonitorExitNode
                                        StoreFieldNode LoadFieldNode
                                        ArrayLengthNode StoreIndexedNode
                                        NewObjectArrayNode CheckCastNode
                                        LoadIndexedNode RegisterFinalizerNode
                                        InstanceOfNode NewTypeArrayNode)
           (com.oracle.graal.nodes.extended ValueAnchorNode LookupSwitchNode
                                            RuntimeCallNode)
           (com.oracle.graal.nodes.calc CompareNode IntegerAddNode
                                        IntegerSubNode IntegerMulNode
                                        LeftShiftNode RightShiftNode XorNode
                                        UnsignedRightShiftNode AndNode
                                        ConvertNode OrNode NullCheckNode
                                        NormalizeCompareNode)
           ))

(defn stamp [s]
  {:non-null (.nonNull s)
   :declared-type (.declaredType s)
   :exact-type (.exactType s)
   :kind (.kind s)
   :scalar-type (.scalarType s)
   :object-type (.objectType s)})

(defn node [n]
  {:type :node
   :node-class (.getNodeClass n)})

(defn frame-state [fstate]
  (assoc (node fstate)
         :type :frame-state
         :locals-size (.localsSize fstate)
         :stack-size (.stackSize fstate)
         :rethrow-exception (.rethrowException fstate)
         :during-call (.duringCall fstate)
         :outer-frame-state (.outerFrameState fstate) 
         :values (.values fstate) 
         :virtual-obj-mappings (.virtualObjectMappings fstate) 
         :bci (.bci fstate)
         :method (.method fstate)))

(defn scheduled-node [n]
  (assoc (node n) 
         :type :scheduled-node
         :scheduled-next (.scheduledNext n)))

(defn value-node [n]
  (assoc (scheduled-node n)
         :type :value-node
         :stamp (stamp (.stamp n)) 
         :dependencies (.dependencies n)))

(defn fixed-node [n]
  (assoc (value-node n)
         :type :fixed-node
         :probability (.probability n)))

(defn fixed-with-next-node [n]
  (assoc (fixed-node n)
         :type :fixed-with-next-node))

(defn abstract-state-split [n]
  (assoc (fixed-with-next-node n)
         :type :abstract-state-split
         :state-after (.stateAfter n)))

(defn begin-node [n]
  (assoc (abstract-state-split n)
         :type :begin-node))

(defn access-monitor-node [n]
  (assoc (abstract-state-split n)
         :type :access-monitor-node
         :object (.object n)
         :eliminated (.eliminated n)))

(defn monitor-enter-node [n]
  (assoc (access-monitor-node n)
         :type :monitor-enter-node))

(defn monitor-exit-node [n]
  (assoc (access-monitor-node n)
         :type :monitor-exit-node))

(defn floating-node [n]
  (assoc (value-node n)
         :type :floating-node))

(defn local-node [n]
  (assoc (floating-node n)
         :type :local-node
         :index (.index n)))

(defn new-instance-node [n]
  (assoc (fixed-with-next-node n)
         :type :new-instance-node
         :instance-class (.instanceClass n)))

(defn value-anchor-node [n]
  (assoc (fixed-with-next-node n)
         :type :value-anchor-node
         :object (.object n)))

(defn call-target-node [n]
  (assoc (value-node n)
         :type :call-target-node
         :arguments (.arguments n)))

(defn method-call-target-node [n]
  (assoc (call-target-node n)
         :type :method-call-target-node
         :return-type (.returnType n)
         :target-method (.targetMethod n)
         :invoke-kind (.invokeKind n)
         :return-stamp (.returnStamp n)))

(defn invoke-node [n]
  (assoc (abstract-state-split n)
         :type :invoke-node
         :bci (.bci n)
         :megamorph (.megamorph n)
         :leaf-graph-id (.leafGraphId n)
         :call-target (.callTarget n)
         :use-for-inlining (.useForInlining n)))

(defn return-node [n]
  (assoc (fixed-node n)
         :type :return-node
         :result (.result n)))

(defn access-field-node [n]
  (assoc (abstract-state-split n)
         :type :access-field-node
         :object (.object n)
         :field (.field n)))

(defn store-field-node [n]
  (assoc (access-field-node n)
         :type :store-field-node
         :value (.value n)))

(defn boolean-node [n]
  (assoc (floating-node n)
         :type :boolean-node))

(defn compare-node [n]
  (assoc (boolean-node n)
         :type :compare-node
         :x (.x n)
         :y (.y n)
         :condition (.condition n)
         :unordered-is-true (.unorderedIsTrue n)))

(defn control-split-node [n]
  (assoc (fixed-node n)
         :type :control-split-node
         :block-successors (->> (.blockSuccessorCount n)
                             range
                             (map #(.blockSuccessor n %))
                             vec) 
         :branch-probability (->> (.blockSuccessorCount n)
                               range
                               (map #(.probability n %))
                               vec)))

(defn if-node [n]
  (assoc (control-split-node n)
         :type :control-split-node
         :compare (.compare n)))

(defn constant-node [n]
  (assoc (boolean-node n)
         :type :constant-node
         :value (.value n)))

(defn end-node [n]
  (assoc (fixed-node n)
         :type :end-node))

(defn merge-node [n]
  (assoc (begin-node n)
         :type :merge-node
         :ends (.cfgPredecessors n)))

(defn phi-node [n]
  (assoc (floating-node n)
         :type :phi-node
         :phi-type (.type n)
         :merge (.merge n)
         :values (.values n)))

(defn load-field-node [n]
  (assoc (access-field-node n)
         :type :load-field-node))

(defn array-length-node [n]
  (assoc (fixed-with-next-node n)
         :type :array-length-node
         :array (.array n)))

(defn loop-begin-node [n]
  (assoc (merge-node n)
         :type :loop-begin-node
         :loop-frequency (.loopFrequency n)
         :next-end-index (dec (.nextEndIndex n))))

(defn access-array-node [n]
  (assoc (abstract-state-split n)
         :type :access-array-node
         :array (.array n)))

(defn access-indexed-node [n]
  (assoc (access-array-node n)
         :type :access-indexed-node
         :index (.index n)
         :length (.length n)
         :element-type (.elementKind n)
         :leafGraphId (.leafGraphId n)))

(defn store-indexed-node [n]
  (assoc (access-indexed-node n)
         :type :store-indexed-node
         :value (.value n)))

(defn deoptimize-node [n]
  (assoc (fixed-node n)
         :type :deoptimize-node
         :message (.message n)
         :action (.action n)
         :reason (.reason n)
         :leafGraphId (.leafGraphId n)))

(defn switch-node [n]
  (assoc (control-split-node n)
         :type :switch-node
         :value (.value n)))

(defn lookup-switch-node [n]
  (assoc (switch-node n)
         :type :lookup-switch-node
         :keys (->> (.keysLength n)
                 range
                 (map #(.keyAt n %))
                 vec)))

(defn new-array-node [n]
  (assoc (fixed-with-next-node n)
         :type :new-array-node
         :length (.length n)))

(defn new-object-array-node [n]
  (assoc (new-array-node n)
         :type :new-object-array-node
         :element-class (.elementType n)))

(defn loop-end-node [n]
  (assoc (end-node n)
         :type :loop-end-node
         :loop-begin (.loopBegin n)
         :safepoint-polling (.hasSafepointPolling n)
         :end-index (.endIndex n)))

(defn binary-node [n]
  (assoc (floating-node n)
         :type :binary-node
         :x (.x n)
         :y (.y n)))

(defn arithmatic-node [n]
  (assoc (binary-node n)
         :type :arithmatic-node
         :is-strict-fp (.isStrictFP n)))

(defn integer-arithmatic-node [n]
  (assoc (arithmatic-node n)
         :type :integer-arithmatic-node))

(defn integer-add-node [n]
  (assoc (integer-arithmatic-node n)
         :type :integer-add-node))

(defn anchor-node [n]
  (assoc (fixed-with-next-node n)
         :type :anchor-node
         ; TODO: get the guards
         :guards nil))

(defn type-check-node [n]
  (assoc (boolean-node n)
         :type :type-check-node
         :object (.object n)
         :target-class-instruction (.targetClassInstruction n)
         :target-class (.targetClass n)
         :hints (.hints n)
         :hints-exact (.hintsExact n)))

(defn check-cast-node [n]
  (assoc (type-check-node n)
         :type :check-cast-node
         :anchor (.anchor n)
         :emit-code (.emitCode n)))

(defn integer-sub-node [n]
  (assoc (integer-arithmatic-node n)
         :type :integer-sub-node))

(defn integer-mul-node [n]
  (assoc (integer-arithmatic-node n)
         :type :integer-mul-node))

(defn load-indexed-node [n]
  (assoc (access-indexed-node n)
         :type :load-indexed-node))

(defn register-finalizer-node [n]
  (assoc (abstract-state-split n)
         :type :register-finalizer-node
         :object (.object n)))

(defn shift-node [n]
  (assoc (binary-node n)
         :type :shift-node))

(defn left-shift-node [n]
  (assoc (shift-node n)
         :type :left-shift-node))

(defn right-shift-node [n]
  (assoc (shift-node n)
         :type :right-shift-node))

(defn instance-of-node [n]
  (assoc (type-check-node n)
         :type :instance-of-node
         :negated (.negated n)))

(defn logic-node [n]
  (assoc (binary-node n)
         :type :logic-node))

(defn xor-node [n]
  (assoc (logic-node n)
         :type :xor-node))

(defn unsigned-right-shift-node [n]
  (assoc (shift-node n)
         :type :unsigned-right-shift-node))

(defn conditional-node [n]
  (assoc (binary-node n)
         :type :conditional-node
         :condition (.condition n)))

(defn materialize-node [n]
  (assoc (conditional-node n)
         :type :materialize-node))

(defn and-node [n]
  (assoc (logic-node n)
         :type :and-node))

(defn convert-node [n]
  (assoc (floating-node n)
         :type :convert-node
         ; TODO: get from and to
         ; :from (.from n)
         ; :to (.to n)
         :value (.value n)
         :opcode (.opcode n)))

(defn or-node [n]
  (assoc (logic-node n)
         :type :or-node))

(defn new-type-array-node [n]
  (assoc (new-array-node n)
         :type :new-type-array-node
         :element-type (.elementType n)))

(defn normalize-compare-node [n]
  (assoc (binary-node n)
         :type :normalize-compare-node
         :is-unordered-less (.isUnorderedLess n)))

(defn null-check-node [n]
  (assoc (boolean-node n)
         :type :null-check-node
         :object (.object n)
         :expected-null (.expectedNull n)))

(defn abstract-call-node [n]
  (assoc (abstract-state-split n)
         :type :abstract-call-node
         :arguments (.arguments n)))

(defn runtime-call-node [n]
  (assoc (abstract-call-node n)
         :type :runtime-call-node
         :call (.call n)))

(defn cnode
  "Convert a Graal node to a hash-map"
  [n] 
  (condp = (class n)
    BeginNode (begin-node n)
    MonitorEnterNode (monitor-enter-node n)
    LocalNode (local-node n)
    FrameState (frame-state n)
    NewInstanceNode (new-instance-node n)
    ValueAnchorNode (value-anchor-node n)
    MethodCallTargetNode (method-call-target-node n)
    InvokeNode (invoke-node n)
    MonitorExitNode (monitor-exit-node n)
    ReturnNode (return-node n)
    StoreFieldNode (store-field-node n)
    CompareNode (compare-node n)
    IfNode (if-node n)
    ConstantNode (constant-node n)
    EndNode (end-node n)
    MergeNode (merge-node n)
    PhiNode (phi-node n)
    LoadFieldNode (load-field-node n)
    ArrayLengthNode (array-length-node n)
    LoopBeginNode nil ;(loop-begin-node n)
    StoreIndexedNode (store-indexed-node n)
    DeoptimizeNode (deoptimize-node n)
    LookupSwitchNode (lookup-switch-node n)
    NewObjectArrayNode (new-object-array-node n)
    LoopEndNode (loop-end-node n)
    IntegerAddNode (integer-add-node n)
    AnchorNode (anchor-node n)
    CheckCastNode (check-cast-node n)
    IntegerSubNode (integer-sub-node n)
    LoadIndexedNode (load-indexed-node n)
    IntegerMulNode (integer-mul-node n)
    RegisterFinalizerNode (register-finalizer-node n)
    LeftShiftNode (left-shift-node n)
    RightShiftNode (right-shift-node n)
    InstanceOfNode (instance-of-node n)
    XorNode (xor-node n)
    UnsignedRightShiftNode (unsigned-right-shift-node n)
    MaterializeNode (materialize-node n)
    AndNode (and-node n)
    ConvertNode (convert-node n)
    OrNode (or-node n)
    NewTypeArrayNode (new-type-array-node n)
    NormalizeCompareNode (normalize-compare-node n)
    NullCheckNode (null-check-node n)
    RuntimeCallNode (runtime-call-node n)
    (throw (Exception. (str "Unknown node class: " (class n))))))

