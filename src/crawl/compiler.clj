(ns crawl.compiler
  (:use [crawl.nodes :only [cnode]])
  (:import (com.oracle.max.cri.ci CiTarget CiBailout
                                  CiTargetMethod)
           (com.oracle.max.cri.ri RiResolvedMethod)
           (com.oracle.graal.cri GraalRuntime)
           (com.oracle.graal.compiler.target Backend)
           (com.oracle.max.cri.xir RiXirGenerator)))

(defprotocol CrawlCompilerProto
  (compileMethod [this method graph osrBCI cache plan optimisticOpts])
  (graphInfo [this graph]))

(deftype CrawlCompiler [#^GraalRuntime runtime
                        #^CiTarget target
                        #^Backend backend
                        #^RiXirGenerator xir]
  CrawlCompilerProto
  (compileMethod [this m g o c p opts]
      (prn :nodes (count (seq (.getNodes g))) (class (first (seq (.getNodes g)))))
      (throw (CiBailout. "boo!!")))
  (graphInfo [this g]
      (require 'crawl.nodes)
      (when (bound? (var cnode))
        (doseq [node (.getNodes g)]
          (cnode node)))))
