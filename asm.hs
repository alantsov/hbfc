module Asm (compile) where
import Model

compile :: [BFOut] -> String
compile bf = let items = map compileItem bf
              in prefix ++ (unlines items) ++ suffix

compileItem (Add n)  = " movl $buffer, %eax\n\
\ addl pointer, %eax\n\
\ movb $" ++ (show n) ++ ", %bl\n\
\ add %bl, (%eax)\n"

compileItem SetZero = " movl $buffer, %eax\n\
\ addl pointer, %eax\n\
\ movb $0, (%eax)\n"

compileItem (Shift n) | n < 0 = " subl $" ++ (show (abs n)) ++ ", pointer\n"
                      | otherwise = " addl $" ++ (show n) ++  ", pointer\n"

compileItem ReadOut = " movl $3, %eax\n\
\ movl $0, %ebx\n\
\ movl $buffer, %ecx\n\
\ addl pointer, %ecx\n\
\ movl $1, %edx\n\
\ int  $0x80\n"

compileItem WriteOut = " movl $4, %eax\n\
\ movl $1, %ebx\n\
\ movl $buffer, %ecx\n\
\ addl pointer, %ecx\n\
\ movl $1, %edx\n\
\ int  $0x80\n"

compileItem (LoopOut bf n) = let metka = "begin" ++ (show n)
                                 metka2 = "end" ++ (show n)
                                 items = unlines $ map compileItem bf
                                 code = " movl $buffer, %eax\n\
\ addl pointer, %eax\n\
\ cmpb $0, (%eax)\n\
\ je " ++ metka2 ++ "\n\n"
                            in metka ++ ":\n" ++ code ++ items ++ " jmp " ++ metka ++ "\n" ++ metka2 ++ ":\n"

prefix = ".data\n\
\ new_row:\n\
\    .string \"\\n\"\n\
\ pointer:\n\
\        .long 0\n\
\ .bss\n\
\ buffer:\n\
\        .space 256\n\
\ .text\n\
\ .globl  main\n\
\ .type   main, @function\n\
\ main:\n"

suffix = "/* new row */\n\
\ movl $4, %eax\n\
\ movl $1, %ebx\n\
\ movl $new_row, %ecx\n\
\ movl $1, %edx\n\
\ int  $0x80\n\
\ /* exit */\n\
\ movl    $1, %eax\n\
\ movl    $0, %ebx\n\
\ int     $0x80\n\
\ .size   main, . - main\n"


