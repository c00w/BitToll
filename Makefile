docs:
	appcfg.py update docs --oauth2

test:
	cabal-dev install
	vagrant up
	vagrant provision
	py.test testsuite/front_test.py -s

serve_docs:
	dev_appserver.py docs

vagrant:
	vagrant up

test_us:
	py.test testsuite/node_test.py -s

deploy_us:
	production/update.sh atlantis.m.bittoll.com

lint:
	hlint haskell --report.html

.PHONY: docs test serve_docs vagrant lint test_us deploy_us
