# Dotfiles

- [Getting clipboard (copy paste) to work with TMux on 
  OSX](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard)

To copy dotfiles into `$HOME`, run:

```sh
rsync -rv --exclude-from=excludes.txt . ~
```

To install useful software, and my own repos, run:

```sh
./install_software.sh
./install_repos.sh
```


