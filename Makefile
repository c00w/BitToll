docs:
	appcfg.py update docs --oauth2

test:
	cabal-dev install
	vagrant provision
	py.test testsuite/ -s

serve_docs:
	dev_appserver.py docs

vagrant:
	vagrant up

.PHONY: docs test serve_docs vagrant
