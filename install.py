#!/usr/bin/env python3
from pathlib import Path
import os
import subprocess
import stat
import tempfile


HOMEDIR = Path("~").expanduser()
CODEDIR = HOMEDIR / "code"


def run_bash_after_command(cmd):
    with tempfile.NamedTemporaryFile() as temp:
        cmd = f"{cmd} -o {temp.name}"
        subprocess.run(cmd.split(), check=True)
        subprocess.run(f"bash {temp.name} -y".split(), check=True)



def install_rust():
    print()
    print("Installing rust & cargo, via rustup")
    if (HOMEDIR / ".cargo" / "bin" / "cargo").exists():
        print("...Rust already installed")
        return
    run_bash_after_command("curl -sSfL https://sh.rustup.rs")
    subprocess.run("bash ~/.cargo/bin/env".split(), check=True)


def install_golang():
    print()
    print("UNIMPLEMENTED: install_golang")


def install_rust_utils():
    print()
    print("Installing rust utilities")
    cargo = Path("~/.cargo/bin/cargo").expanduser()
    description_and_pkg = [
        ("fd (find)", "fd-find", "fd"),
        ("exa (better ls)", "exa", "exa"),
        ("bat (better cat)", "bat", "bat"),
        ("rg (better grep)", "ripgrep", "rg")
    ]
    for desc, pkg, bin in description_and_pkg:
        target = HOMEDIR / ".cargo" / "bin" / bin
        if target.exists():
            print(f"...{bin} already installed")
            continue
        print(f"...installing {desc}")
        subprocess.run([cargo, "install", "-q", pkg], check=True, capture_output=True)

    if (HOMEDIR / ".cargo" / "bin" / "zoxide").exists():
        print("...zoxide already installed.")
        return
    run_bash_after_command("curl -fsSL https://raw.githubusercontent.com/ajeetdsouza/zoxide/master/install.sh")


def install_starship_prompt():
    print()
    print("Installing starship prompt")
    starship = HOMEDIR / ".cargo" / "bin" / "starship"
    if starship.exists():
        print("...Starship already installed.")
        return
    run_bash_after_command("curl -fsSL https://starship.rs/install.sh")

def install_fzf():
    print()
    print("Installing FZF")
    fzfdir = HOMEDIR / ".fzf"
    if fzfdir.exists():
        print("...FZF already installed.")
        return
    subprocess.run(["git", "clone", "--depth", "1", "https://github.com/junegunn/fzf.git", str(fzfdir)],
                   check=True)
    target = fzfdir / "install"
    subprocess.run([str(target), "--bin"], check=True)

    print("Refresh FZF keybinds")
    target = HOMEDIR / ".config" / "fish" / "functions" / "fzf_key_bindings.fish"
    if target.exists():
        target.unlink()
    source = HOMEDIR / ".fzf" / "shell" / "key-bindings.fish"
    source.symlink_to(target)


def clone_dotfiles():
    print()
    print("Cloning dotfiles")
    target = CODEDIR / "dotfiles"
    if target.exists():
        print("...Dotfiles already cloned.")
        return
    subprocess.run(["git", "clone", "git@github.com:chrisdavison/dotfiles", str(target)], check=True)


def clone_repos():
    print()
    print("UNIMPLEMENTED: clone_repos")


def symlink_files():
    print()
    print("Symlinking files")
    for file in [".bashrc", ".gitconfig", ".sqliterc", ".tmux.conf", ".vimrc", ".zshrc"]:
        source = CODEDIR / "dotfiles" / file
        target = HOMEDIR / file
        if target.exists():
            target.unlink()
        print(f" -- {file} > {target}")
        source.symlink_to(target)


def symlink_directories():
    print()
    print("Symlinking directories")
    for direc in [".vim"]:
        source = CODEDIR / "dotfiles" / direc
        target = HOMEDIR / direc
        if target.exists():
            target.unlink()
        print(f" -- {direc} > {target}")
        source.symlink_to(target, target_is_directory=True)

    print("Symlinking directories to .config/")
    for direc in (CODEDIR / "dotfiles" / ".config").glob("*"):
        if not direc.is_dir():
            continue
        target = HOMEDIR / ".config" / direc
        if target.exists():
            target.unlink()
        direc.symlink_to(target, target_is_directory=True)


def symlink_binaries():
    print()
    BINDIR = HOMEDIR / ".bin"
    BINDIR.mkdir(exist_ok=True)
    print("Symlinking binaries")
    for bin in (CODEDIR / "dotfiles" / "bin").glob('*'):
        target = BINDIR / bin.stem
        if target.exists():
            target.unlink()
        print(f" -- {bin.stem} > {target}")
        bin.symlink_to(target)
        target.chmod(target.stat.st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


if __name__ == "__main__":
    clone_dotfiles()
    # symlink_files()
    # symlink_directories()
    # symlink_binaries()
    install_rust()
    install_golang()
    install_fzf()
    install_starship_prompt()
    install_rust_utils()
    clone_repos()
