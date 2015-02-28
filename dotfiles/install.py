#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
import subprocess

LINK_BASE = os.path.expandvars("$HOME")


#def get_output(*args, **kwargs):
#    return subprocess.check_output(*args, stdout=subprocess.DEVNULL, **kwargs)


def get_files():
    #dirn = get_output(["dirname", "${BASH_SOURCE[0]}"], universal_newlines=True)
    dirn = subprocess.check_output(["dirname", "${BASH_SOURCE[0]}"], universal_newlines=True)
    dot_dir = subprocess.check_output(["cd", dirn, "&&", "pwd"], shell=True, universal_newlines=True)

    all_files = {}

    newpath = os.path.join

    def default_link_paths(files):
        with_link = []
        for f in files:
            path_to_link = newpath(LINK_BASE, os.path.basename(f))
            with_link.append((f, path_to_link))
        return with_link

    # Bash files
    bash_dir = newpath(dot_dir, "bash")
    bashrc = newpath(bash_dir, ".bash/bashrc")
    bash_profile = newpath(bash_dir, ".profile")
    all_files["bash"] = default_link_paths([bashrc, bash_profile])
    #bash_files = { "bash": [bashrc, bash_profile] }

    # Git files
    git_dir = newpath(dot_dir, "git")
    git_conf = newpath(dot_dir, ".gitconfig")
    all_files["git"] = default_link_paths([git_conf])
    #git_files = { "git": [git_conf] }

    # Haskell files
    haskell_dir = newpath(dot_dir, "haskell")
    ghci_conf = newpath(haskell_dir, ".ghci")
    all_files["ghci"] = default_link_paths([ghci_conf])

    # Tmux files
    tmux_dir = newpath(dot_dir, "tmux")
    tmux_conf = newpath(tmux_dir, ".tmux.conf")
    tmuxinator = newpath(tmux_dir, ".tmuxinator")
    all_files["tmux"] = default_link_paths([tmux_conf])
    all_files["tmuxinator"] = default_link_paths([tmuxinator])

    # Vim files
    vim_dir = newpath(dot_dir, "vim")
    vimrc = newpath(vim_dir, ".vimrc")
    all_files["vim"] = default_link_paths([vimrc])

    return all_files


def link_files(req_prog, files):
    if subprocess.call(["which", req_prog], stdout=subprocess.DEVNULL):
        print("{} not found, skipping related dotfiles".format(req_prog))
        return 1
    else:
        for (target, link_path) in files:
            if os.path.exists(link_path):
                print("Skipping file {} - File exists".format(link_path))
                continue
            else:
                print("Would link! (Add linking)")


def main():
    print("Linking dotfiles...")
    prog_files = get_files()
    for (prog, files) in prog_files.items():
        print("Linking files for {}".format(prog))
        link_files(prog, files)


if __name__ == '__main__':
    main()
