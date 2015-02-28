#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
import subprocess

from collections import namedtuple
from functools import partial


LINK_BASE = os.path.expandvars("$HOME")


linkp = namedtuple("linkp", ["parent", "name", "link_type"])


newpath = os.path.join


def resolve_linkp_path(link):
    """Return the full path of link's name."""
    if isinstance(link.parent, str):
        return newpath(link.parent, link.name)
    else:
        return newpath(resolve_linkp_path(link.parent), link.name)


def get_link_path(link):
    """Get the target and path to link to for a particular linkp"""
    target = resolve_linkp_path(link)
    link_type = link.link_type
    if link_type == "default":
        link_path = newpath(LINK_BASE, os.path.basename(link.name))
    else:
        raise NotImplementedError
    return (target, link_path)


#def get_output(*args, **kwargs):
#    return subprocess.check_output(*args, stdout=subprocess.DEVNULL, **kwargs)


def get_files():
    #dirn = get_output(
    #        ["dirname", "${BASH_SOURCE[0]}"],
    #        universal_newlines=True)
    dirn = subprocess.check_output(
        ["dirname", "${BASH_SOURCE[0]}"],
        universal_newlines=True)
    dot_dir = subprocess.check_output(
        ["cd", dirn, "&&", "pwd"],
        shell=True, universal_newlines=True)

    all_files = {}

    def default_link_paths(files):
        with_link = []
        for f in files:
            path_to_link = newpath(LINK_BASE, os.path.basename(f))
            with_link.append((f, path_to_link))
        return with_link

    # Bash files
    def map_partial(fn, *args):
        result = []
        for a in args:
            if isinstance(a, list):
                result.append(fn(*a))
            else:
                result.append(fn(a))
        return result

    def linkp_default(parent, name, link_type="default"):
        return linkp(parent, name, link_type)

    def create_links(base, *args):
        return map_partial(partial(linkp_default, base), *args)

    bash_dir = linkp(dot_dir, "bash", None)
    #bash_files = map_partial(partial(linkp_default, bash_dir),
    #        [".bash/bashrc"],
    #        [".profile"])
    bash_files = create_links(
        bash_dir,
        ".bash/.bashrc",
        ".profile")

    all_files["bash"] = bash_files
    #bash_files = map(partial(linkp,
    ##bash_dir = newpath(dot_dir, "bash")
    #bashrc = linkp(bash_dir, ".bash/.bashrc", "default")
    ##bashrc = newpath(bash_dir, ".bash/.bashrc")
    #bash_profile = linkp(bash_dir, ".profile", "default")
    #bash_profile = newpath(bash_dir, ".profile")
    ##all_files["bash"] = default_link_paths([bashrc, bash_profile])
    #bash_files = { "bash": [bashrc, bash_profile] }

    # Git files
    git_dir = linkp(dot_dir, "git", None)
    git_files = create_links(
        git_dir,
        ".gitconfig")

    all_files["git"] = git_files
    #git_dir = newpath(dot_dir, "git")
    #git_conf = newpath(dot_dir, ".gitconfig")
    #all_files["git"] = default_link_paths([git_conf])
    #git_files = { "git": [git_conf] }

    # Haskell files
    haskell_dir = linkp(dot_dir, "haskell", None)
    ghci_files = create_links(
        haskell_dir,
        ".ghci")
    all_files["ghci"] = ghci_files
    #haskell_dir = newpath(dot_dir, "haskell")
    #ghci_conf = newpath(haskell_dir, ".ghci")
    #all_files["ghci"] = default_link_paths([ghci_conf])

    # Tmux files
    tmux_dir = linkp(dot_dir, "tmux", None)
    tmux_files = create_links(
        tmux_dir,
        ".tmux.conf")
    tmuxinator_files = create_links(
        tmux_dir,
        ".tmuxinator")

    all_files["tmux"] = tmux_files
    all_files["tmuxinator"] = tmuxinator_files

    #tmux_dir = newpath(dot_dir, "tmux")
    #tmux_conf = newpath(tmux_dir, ".tmux.conf")
    #tmuxinator = newpath(tmux_dir, ".tmuxinator")
    #all_files["tmux"] = default_link_paths([tmux_conf])
    #all_files["tmuxinator"] = default_link_paths([tmuxinator])

    # Vim files
    vim_dir = linkp(dot_dir, "vim", None)
    vim_files = create_links(
        vim_dir,
        ".vimrc")
    all_files["vim"] = vim_files
    #vim_dir = newpath(dot_dir, "vim")
    #vimrc = newpath(vim_dir, ".vimrc")
    #all_files["vim"] = default_link_paths([vimrc])

    return all_files


def link_files(req_prog, files):
    if subprocess.call(["which", req_prog], stdout=subprocess.DEVNULL):
        print("{} not found, skipping related dotfiles".format(req_prog))
        return 1
    else:
        for (target, link_path) in map(get_link_path, files):
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
