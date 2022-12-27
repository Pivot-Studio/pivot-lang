.PHONY: devlinux devmac vm install fmt test clean
devlinux:
	@echo "export KAGARI_LIB_ROOT=$(pwd)/planglib">>~/.bashrc
	@echo "export PL_ROOT=$(pwd)/target/release/">>~/.bashrc
	@echo "请重启终端和vsc应用更改"

devmac:
	@echo "export KAGARI_LIB_ROOT=$(pwd)/planglib">>~/.bash_profile
	@echo "export PL_ROOT=$(pwd)/target/release/">>~/.bash_profile
	@echo "请重启终端和vsc应用更改"

vm:
	@cd vm && cargo build --release

install:
	@cargo install --path=.

fmt:
	@cargo +stable fmt

test:
	@cargo test --all

clean:
	@rm -f *.ll && rm -f *.bc && rm -rf *.dSYM && rm -f testout* && rm -f out*  && rm -f *.o
