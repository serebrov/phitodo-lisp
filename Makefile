.PHONY: run build clean install-quicklisp

PROJECT_DIR := $(shell pwd)
QUICKLISP_SETUP := $(HOME)/quicklisp/setup.lisp

# Install Quicklisp if not present
install-quicklisp:
	@if [ ! -f "$(QUICKLISP_SETUP)" ]; then \
		echo "Installing Quicklisp..."; \
		curl -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp; \
		sbcl --load /tmp/quicklisp.lisp \
		     --eval '(quicklisp-quickstart:install)' \
		     --eval '(ql:add-to-init-file)' \
		     --quit; \
		rm /tmp/quicklisp.lisp; \
	else \
		echo "Quicklisp already installed."; \
	fi

run: $(QUICKLISP_SETUP)
	sbcl --load "$(QUICKLISP_SETUP)" \
	     --eval '(push #P"$(PROJECT_DIR)/" asdf:*central-registry*)' \
	     --eval '(ql:quickload "phitodo-tui")' \
	     --eval '(phitodo-tui:main)'

build: $(QUICKLISP_SETUP)
	sbcl --load "$(QUICKLISP_SETUP)" \
	     --eval '(push #P"$(PROJECT_DIR)/" asdf:*central-registry*)' \
	     --eval '(ql:quickload "phitodo-tui")' \
	     --eval '(phitodo-tui.main:build-executable)'

$(QUICKLISP_SETUP):
	@echo "Quicklisp not found. Run 'make install-quicklisp' first."
	@exit 1

clean:
	rm -f phitodo-tui
	find . -name "*.fasl" -delete
