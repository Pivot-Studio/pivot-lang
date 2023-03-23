.PHONY: devlinux devmac vm install fmt test clean bench
devlinux:
	@echo "export KAGARI_LIB_ROOT=$$(pwd)/planglib">>~/.bashrc
	@echo "export PL_ROOT=$$(pwd)/target/release/">>~/.bashrc
	@echo "环境变量已加入~/.bashrc，请重启终端和vsc应用更改"

devmac:
	@echo "export KAGARI_LIB_ROOT=$$(pwd)/planglib">>~/.bash_profile
	@echo "export PL_ROOT=$$(pwd)/target/release/">>~/.bash_profile
	@echo "环境变量已加入~/.bash_profile，请重启终端和vsc应用更改"

vm:
	@cd vm && cargo build --release

vmdebug:
	@cd vm && cargo build
	@cp target/debug/libvm.a target/release/libvm.a

install:
	@cargo install --path=.

fmt:
	@cargo +stable fmt

test: vmdebug
	@cargo test --all

clean:
	@rm -rf out*
	@cd target && rm -f *.ll && rm -f *.bc && rm -rf *.dSYM && rm -f testout* && rm -f out*  && rm -f *.o

bench:
	@cd immix && cargo bench 
	@cd vm && cargo bench

bench-simple-gc:
	@cd vm && cargo bench --features=simple_gc --no-default-features

cmake-clean:
	@find . -name CMakeCache.txt -type f -delete
