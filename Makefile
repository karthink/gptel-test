.PHONY: test
test:
	emacs -Q -L .. -L . \
        $$(find .. -mindepth 1 -maxdepth 2 -name "gptel-*.el" -exec echo "-l {} \\"  \;) \
	-l ert --batch -f ert-run-tests-batch-and-exit
