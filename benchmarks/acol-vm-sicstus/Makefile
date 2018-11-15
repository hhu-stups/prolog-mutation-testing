.PHONY: benchmarks

install:
	splfr --resource=driver --source=c_driven.pl driver.c stack.c --cflag="-Wall" --cflag="-O3"
test:
	sicstus -l bc_interpreter.pl --goal "plunit:run_tests,halt."
