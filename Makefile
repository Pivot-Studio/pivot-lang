.PHONY: devlinux devmac vm install fmt test clean bench
devlinux:
	@echo "export KAGARI_LIB_ROOT=$$(pwd)/planglib" >> ~/.bashrc
	@echo "export PL_ROOT=$$(pwd)/target/release/" >> ~/.bashrc
	@source ~/.bashrc
	@echo "环境变量已加入~/.bashrc，请重启终端和vsc应用更改"

devmac:
	@echo "export KAGARI_LIB_ROOT=$$(pwd)/planglib" >> ~/.bash_profile
	@echo "export PL_ROOT=$$(pwd)/target/release/" >> ~/.bash_profile
	@source ~/.bash_profile
	@echo "环境变量已加入~/.bashrc，请重启终端和vsc应用更改"

vm:
	@cd vm && cargo build --release --locked

vmdebug:
	@mkdir -p target/release
	@cd vm && cargo build
	@cp target/debug/libvm.a target/release/libvm.a
	@touch target/debug/libvm.so && cp target/debug/libvm.so target/release/libvm.so
	@touch target/debug/libvm.dylib && cp target/debug/libvm.dylib target/release/libvm.dylib
	@touch target/debug/libuv.so && cp target/debug/libuv.so target/release/libuv.so
	@touch target/debug/libuv.dylib && cp target/debug/libuv.dylib target/release/libuv.dylib

install:
	@cargo install --path=. --locked

fmt:
	@cargo +stable fmt

test: vmdebug
	@cargo nextest run --workspace --all-features

clean:
	@rm -rf out*
	@cd target && rm -f *.ll && rm -f *.bc && rm -rf *.dSYM && rm -f testout* && rm -f out*  && rm -f *.o && rm -rf test*

bench:
	@cd immix && cargo bench 
	@cd vm && cargo bench

bench-simple-gc:
	@cd vm && cargo bench --features=simple_gc --no-default-features

cmake-clean:
	@find . -name CMakeCache.txt -type f -delete

lsp-wasm:
	@wasm-pack build --target bundler --no-default-features --scope pivot-lang

renew-expect:
	@UPDATE_EXPECT=1 cargo test --all

mdbook-install:
	@cargo install mdbook
	@cargo install mdbook-mermaid
	@cargo install mdbook-admonish
	@cargo install mdbook-linkcheck
	@cargo install mdbook-toc

init-submodules:
	@git submodule update --init --recursive

