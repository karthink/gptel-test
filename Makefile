.PHONY: test deps all-deps

test:
	emacs -Q -L .. -L . \
	$$(find .. -mindepth 1 -maxdepth 1 \( -name '.*.el' -prune -o -name '*.el' -type f -printf ' -l %p' \)) \
	$$(find .  -mindepth 1 -maxdepth 1 \( -name '.*.el' -prune -o -name '*.el' -type f -printf ' -l %p' \)) \
	-l ert --batch -f ert-run-tests-batch-and-exit

deps:
	emacs -Q --batch -L . -l gptel-test-deps.el -f gptel-test-install-deps

all-deps:
	emacs -Q --batch -L . -l gptel-test-deps.el -f gptel-test-install-deps-optional
