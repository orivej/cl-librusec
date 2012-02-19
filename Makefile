all: construct-db

construct-db: build.lisp lisp/*
	sbcl --noinform --disable-debugger --load build.lisp

clean:
	-rm construct-db

distclean: clean
	-rm quicklisp.lisp
	-rm -rf quicklisp
