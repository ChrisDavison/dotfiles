if [ -n $(uname -r | grep -i 'microsoft') ]; then
    IS_WSL=1
fi

if [[ -x "$HOME/.bin/nvim.appimage" ]]; then
    HAS_NVIM_APPIMAGE=1
fi
