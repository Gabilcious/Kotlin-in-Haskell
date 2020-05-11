all : interpreter

interpreter : src/Main.hs src/ErrM.hs src/LexKotlin.hs src/ParKotlin.hs src/PrintKotlin.hs src/Interpreter.hs src/TypeCheck.hs
	ghc --make $< -o $@ -isrc

clean :
	-rm interpreter
