demo_host := 127.0.0.1

build:
	cabal build

exe:
	cabal build exe:wire-cli

test-unit:
	cabal test --test-show-details=always unit

test-int-demo: exe
	cabal test --test-show-details=always integration --test-options="--wire-cli-path=$$(cabal-plan list-bin wire-cli) --backend-uri=http://$(demo_host):8080 --backdoor-brig-uri=http://$(demo_host):8082 --email-template=foo\$${random}@example.com"

test-int-kube: test-int-kube-env-vars exe
	cabal test --test-show-details=always integration --test-options="--wire-cli-path=$$(cabal-plan list-bin wire-cli) --backend-uri=http://$(kube_nginz_host):$(kube_nginz_port) --backdoor-nginz-user=$(kube_backdoor_nginz_user) --backdoor-nginz-password=$(kube_backdoor_nginz_password) --email-template=foo\$${random}@example.com"

test-int-kube-env-vars: check-var-kube_nginz_host check-var-kube_nginz_port check-var-kube_backdoor_nginz_user check-var-kube_backdoor_nginz_password

check-var-%:
	if [ -z "$$$(*)" ]; then echo "Please set $(*)" && false; fi

run-gui:
	cabal run wire-gui
