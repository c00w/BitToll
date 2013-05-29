docs:
	appcfg.py update docs --oauth2

test:
	cabal-dev install
	vagrant provision
	py.test testsuite/front_test.py -s

serve_docs:
	dev_appserver.py docs

vagrant:
	vagrant up

test_us:
	py.test testsuite/node_test.py -s

.PHONY: docs test serve_docs vagrant
