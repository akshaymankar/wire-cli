demo_host := 127.0.0.1

build:
	cabal build

build-cli:
	cabal build exe:wire-cli

build-gui:
	cabal build exe:wire-gui

build-unit:
	cabal build wire-cli:test:unit

build-integration:
	cabal build wire-cli:test:integration

test-unit: build-unit
	$$(cabal-plan list-bin test:unit)

test-int-demo: build-integration build-cli
	$$(cabal-plan list-bin test:integration) \
		--wire-cli-path=$$(cabal-plan list-bin wire-cli) \
		--backend-uri=http://$(demo_host):8080 \
		--backdoor-brig-uri=http://$(demo_host):8082 \
		--email-template=foo\$${random}@example.com

test-int-kube: test-int-kube-env-vars build-integration build-cli
	$$(cabal-plan list-bin test:integration) \
		--wire-cli-path=$$(cabal-plan list-bin wire-cli) \
		--backend-uri=http://$(kube_nginz_host):$(kube_nginz_port) \
		--backdoor-nginz-user=$(kube_backdoor_nginz_user) \
		--backdoor-nginz-password=$(kube_backdoor_nginz_password) \
		--email-template=foo\$${random}@example.com

test-int-kube-env-vars: check-var-kube_nginz_host check-var-kube_nginz_port check-var-kube_backdoor_nginz_user check-var-kube_backdoor_nginz_password

check-var-%:
	if [ -z "$$$(*)" ]; then echo "Please set $(*)" && false; fi

run-gui:
	cabal run wire-gui
