export EDITOR="nvim"
if [[ "$HAS_NVIM_APPIMAGE" -eq 1 ]]; then
    export EDITOR="$HOME/.bin/nvim.appimage"
fi
export GOPATH="$HOME"
export GOBIN="$HOME/.bin"
export FZF_DEFAULT_COMMAND='rg --files -S --no-ignore --hidden --follow --glob "!.git/*"'
export WORKON_HOME="$HOME/.envs"
export LESS=FRSX
export VIRTUAL_ENV_DISABLE_PROMPT=1
export MAIL=$HOME/.mbox
export RE_UUID="[a-z0-9]{8}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{4}-[a-z0-9]{12}"
export RANGER_LOAD_DEFAULT_RC=0
export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library
export BROWSER="firefox"
export LC_ALL=en_GB.UTF-8
export WASMTIME_HOME="$HOME/.wasmtime"
export PATH="$WASMTIME_HOME/bin:$PATH"

