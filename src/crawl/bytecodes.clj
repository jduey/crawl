(ns crawl.bytecodes)

(defmacro opcodes [& opcodes]
  `(do
     ~@(map (fn [sym code]
            `(def ~sym ~code)) 
          opcodes
          (range (count opcodes)))))

(opcodes nop aconst-null iconst-m1 iconst-0 iconst-1 iconst-2 iconst-3
         iconst-4 iconst-5 lconst-0 lconst-1 fconst-0 fconst-1 fconst-2
         dconst-0 dconst-1 bipush sipush ldc ldc-w ldc2-w iload lload
         fload dload aload iload-0 iload-1 iload-2 iload-3 lload-0 lload-1
         lload-2 lload-3 fload-0 fload-1 fload-2 fload-3 dload-0 dload-1
         dload-2 dload-3 aload-0 aload-1 aload-2 aload-3 iaload laload
         faload daload aaload baload caload saload istore lstore fstore
         dstore astore istore-0 istore-1 istore-2 istore-3 lstore-0 lstore-1
         lstore-2 lstore-3 fstore-0 fstore-1 fstore-2 fstore-3 dstore-0
         dstore-1 dstore-2 dstore-3 astore-0 astore-1 astore-2 astore-3
         iastore lastore fastore dastore aastore bastore castore sastore
         pop-stack pop2 dup dup-x1 dup-x2 dup2 dup2-x1 dup2-x2 swap iadd ladd
         fadd dadd isub lsub fsub dsub imul lmul fmul dmul idiv ldiv fdiv
         ddiv irem lrem frem drem ineg lneg fneg dneg ishl lshl ishr lshr
         iushr lushr iand land ior lor ixor lxor iinc i2l i2f i2d l2i l2f l2d
         f2i f2l f2d d2i d2l d2f i2b i2c i2s lcmp fcmpl fcmpg dcmpl dcmpg
         ifeq ifne iflt ifge ifgt ifle if-icmpeq if-icmpne if-icmplt
         if-icmpge if-icmpgt if-icmple if-acmpeq if-acmpne goto jsr ret
         tableswitch lookupswitch ireturn lreturn freturn dreturn areturn
         return getstatic putstatic getfield putfield invoke-virtual
         invoke-special invoke-static invoke-interface xxxunusedxxx new-obj
         newarray anewarray arraylength athrow checkcast instanceof
         monitorenter monitorexit wide multianewarray if-null if-nonnull
         goto-w jsr-w breakpoint)

(def illegal 255) 
(def end 256) 

; collections of opcode types
(def return-bytecode #{ireturn lreturn freturn dreturn areturn return})
(def if-bytecode #{ifeq ifne iflt ifge ifle ifgt if-icmpeq if-icmpne
                   if-icmplt if-icmpge if-icmpgt if-icmple if-acmpeq 
                   if-acmpne if-null if-nonnull})
(def invoke-bytecode #{invoke-interface invoke-special invoke-static
                       invoke-virtual})

(def one-byte-opcode #{nop aconst-null iconst-m1 iconst-0 iconst-1 iconst-2
                       iconst-3 iconst-4 iconst-5 lconst-0 lconst-1 fconst-0
                       fconst-1 fconst-2 dconst-0 dconst-1 iload-0 iload-1 
                       iload-2 iload-3 lload-0 lload-1 lload-2 lload-3 
                       fload-0 fload-1 fload-2 fload-3 dload-0 dload-1 dload-2 
                       dload-3 aload-0 aload-1 aload-2 aload-3 iaload laload
                       faload daload aaload baload caload saload istore-0
                       istore-1 istore-2 istore-3 lstore-0 lstore-1 lstore-2
                       lstore-3 fstore-0 fstore-1 fstore-2 fstore-3 dstore-0
                       dstore-1 dstore-2 dstore-3 astore-0 astore-1 astore-2
                       astore-3 iastore lastore fastore dastore aastore 
                       bastore castore sastore pop-stack pop2 dup dup-x1 dup-x2
                       dup2 dup2-x1 dup2-x2 swap iadd ladd fadd dadd isub
                       lsub fsub dsub imul lmul fmul dmul idiv ldiv fdiv
                       ddiv irem lrem frem drem ineg lneg fneg dneg ishl
                       lshl ishr lshr iushr lushr iand land ior lor ixor
                       lxor i2l i2f i2d l2i l2f l2d f2i f2l f2d d2i d2l 
                       d2f i2b i2c i2s lcmp fcmpl fcmpg dcmpl dcmpg ireturn 
                       lreturn freturn dreturn areturn return arraylength 
                       athrow monitorenter monitorexit breakpoint})

(def two-byte-opcode #{bipush ldc iload lload fload dload aload istore
                       lstore fstore dstore astore ret newarray})

(def three-byte-opcode #{sipush ldc-w ldc2-w iinc ifeq ifne iflt ifge ifgt
                         ifle if-icmpeq if-icmpne if-icmplt if-icmpge
                         if-icmpgt if-icmple if-acmpeq if-acmpne goto
                         jsr getstatic putstatic getfield putfield
                         invoke-virtual invoke-special invoke-static new-obj
                         anewarray checkcast instanceof if-null if-nonnull})

(def four-byte-opcode #{multianewarray})

(def five-byte-opcode #{invoke-interface goto-w jsr-w})

