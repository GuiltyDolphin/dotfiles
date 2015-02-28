#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import subprocess

from collections import namedtuple
from functools import partial

import getopt

LINK_BASE = os.path.expandvars("$HOME")

Link = namedtuple("Link", ["parent", "name", "link_type"])

newpath = os.path.join

dot_dir = os.path.dirname(os.path.realpath(__file__))

def resolve_link_path(link):
    """Return the full path of link's name."""
    if isinstance(link.parent, str):
        return newpath(link.parent, link.name)
    else:
        return newpath(resolve_link_path(link.parent), link.name)


def get_link_path(link):
    """Get the target and path to link to for a particular Link"""
    target = resolve_link_path(link)
    link_type = link.link_type
    if link_type == "default":
        link_path = newpath(LINK_BASE, os.path.basename(link.name))
    else:
        raise NotImplementedError
    return (target, link_path)


def get_files():
    """Retrieve the files to be linked"""
    dirn = subprocess.check_output(
        ["dirname", "${BASH_SOURCE[0]}"],
        universal_newlines=True)
    #dot_dir = subprocess.check_output(
    #    ["cd", dirn, "&&", "pwd"],
    #    shell=True, universal_newlines=True)

    all_files = {}

    # Bash files
    def map_partial(fn, *args):
        result = []
        for a in args:
            if isinstance(a, list):
                result.append(fn(*a))
            else:
                result.append(fn(a))
        return result

    def link_default(parent, name, link_type="default"):
        return Link(parent, name, link_type)

    def create_links(base, *args):
        return map_partial(partial(link_default, base), *args)

    bash_dir = Link(dot_dir, "bash", None)
    bash_files = create_links(
        bash_dir,
        ".bash/.bashrc",
        ".profile")

    all_files["bash"] = bash_files

    # Git files
    git_dir = Link(dot_dir, "git", None)
    git_files = create_links(
        git_dir,
        ".gitconfig")

    all_files["git"] = git_files

    # Haskell files
    haskell_dir = Link(dot_dir, "haskell", None)
    ghci_files = create_links(
        haskell_dir,
        ".ghci")
    all_files["ghci"] = ghci_files

    # Tmux files
    tmux_dir = Link(dot_dir, "tmux", None)
    tmux_files = create_links(
        tmux_dir,
        ".tmux.conf")
    tmuxinator_files = create_links(
        tmux_dir,
        ".tmuxinator")

    all_files["tmux"] = tmux_files
    all_files["tmuxinator"] = tmuxinator_files

    # Vim files
    vim_dir = Link(dot_dir, "vim", None)
    vim_files = create_links(
        vim_dir,
        ".vimrc")
    all_files["vim"] = vim_files

    return all_files


def link_files(req_prog, files):
    try:
        subprocess.check_output(["which", req_prog])
    except subprocess.CalledProcessError:
        print("{} not found, skipping related dotfiles".format(req_prog))
        return 1
    else:
        for (target, link_path) in map(get_link_path, files):
            if os.path.exists(link_path):
                reason = os.path.samefile(
                    target, link_path) and "Already linked" or "File exists"
                print("Skipping file {} - {}".format(link_path, reason))
                continue
            else:
                print("Would link! (Add linking)")
                # try:
                #   subprocess.call(["ln", "-s", target, link_path])
                # catch CalledProcessError:
                #   print("Failed to link {} to {}".format(target, link_path))
                # else:
                #   print("Linked file: {} to {}".format(target, link_path))


def setup_solarized_colors(color_dir):
    color_path = newpath(color_dir, ".dir_colors")
    file_url = "https://raw.githubusercontent.com"
    + "/seebi/dircolors-solarized/master/dircolors.256dark"
    if os.path.exists(color_path):
        print("{} already exists".format(color_path))
    else:
        print("{} not found, downloading from github...".format(color_path))
    try:
        subprocess.check_call(["wget", file_url, "-O", color_path])
    except subprocess.CalledProcessError:
        print("Could not download file")
    else:
        print("File successfully downloaded.")


def main():
    print("Linking dotfiles...")
    prog_files = get_files()
    for (prog, files) in prog_files.items():
        print("Linking files for {}".format(prog))
        link_files(prog, files)


if __name__ == '__main__':
    main()
