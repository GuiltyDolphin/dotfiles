#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# install.py
# Part of GuiltyDolphin's dotfiles
# Hosted at https://www.github.com/GuiltyDolphin/config

import os
import sys
import subprocess

from functools import partial
from itertools import chain

import getopt  # Use this rather than argparse (for compatibility)

import logging as log
import time
import shutil

# This is just to fix the input incompatability with python2
try:
    input = raw_input
except NameError:
    pass

newpath = os.path.join

INVOKE_DIR = os.path.dirname(os.path.realpath(__file__))

DOT_DIR = newpath(INVOKE_DIR, "dotfiles")

HOME = os.path.expandvars("$HOME")

# The default directory under which files will be linked
LINK_BASE = HOME

CONFIG_DIR = INVOKE_DIR


class Link(object):
    """Create hierarchical symlink representations"""
    def __init__(self, parent, name):
        """Create a link object

        parent (string | Link) - Parent used for determining
        filepaths.
        name (string) - the final target path to the file
        """
        self._parent = parent
        self._name = name

    @property
    def target_path(self):
        """Get the path of the file to be linked"""
        if isinstance(self._parent, str):
            return newpath(self._parent, self._name)
        else:
            return newpath(self._parent.target_path, self._name)

    @property
    def link_path(self):
        """Return the path to be linked to.

        Override this to provide custom link paths"""
        return newpath(LINK_BASE, os.path.basename(self._name))

    @property
    def link_parameters(self):
        """Return the target and path to be linked"""
        target = self.target_path
        link_path = self.link_path
        return (target, link_path)


class EmacsLink(Link):
    """Link for emacs configuration files"""

    @property
    def link_path(self):
        return newpath(LINK_BASE, ".emacs.d", self._name)


class KeyboardLink(Link):
    """Link for keyboard configuration"""
    @property
    def link_path(self):
        return newpath(LINK_BASE, ".keyboard")


class DefaultLink(Link):
    """Default link for standard dotfiles"""
    pass


def require_yes_no(prompt):
    """Return a boolean value based on a user input of 'yes' or 'no'."""
    response = input(prompt + " [y/n]: ")
    affirmatives = ('y', 'yes')
    negatives = ('n', 'no')
    while not response.lower() in affirmatives + negatives:
        response = input("Response must be y[es] or n[o]: ")
    return response in affirmatives


def get_files():
    """Retrieve the files to be linked"""
    all_files = {}

    def make_default_links(base, *args):
        """Create a series of links using base as the parent"""
        link_type = type(base)
        return map(partial(link_type, base), args)

    # Bash files
    bash_dir = DefaultLink(DOT_DIR, "bash")
    bash_files = make_default_links(
        bash_dir,
        ".bash/.bashrc",
        ".profile")

    all_files["bash"] = bash_files

    # Emacs files
    emacs_dir = EmacsLink(DOT_DIR, "emacs")
    emacs_custom_dir = EmacsLink(emacs_dir, "custom")
    emacs_custom_files = make_default_links(
        emacs_dir,
        "custom")
    emacs_files = chain(emacs_custom_files, make_default_links(
        emacs_custom_dir,
        "init.el"))

    all_files["emacs"] = emacs_files

    # Git files
    git_dir = DefaultLink(DOT_DIR, "git")
    git_files = make_default_links(
        git_dir,
        ".gitconfig")

    all_files["git"] = git_files

    # Haskell files
    haskell_dir = DefaultLink(DOT_DIR, "haskell")
    ghci_files = make_default_links(
        haskell_dir,
        ".ghci")
    all_files["ghci"] = ghci_files

    # Tmux files
    tmux_dir = DefaultLink(DOT_DIR, "tmux")
    tmux_files = make_default_links(
        tmux_dir,
        ".tmux.conf")
    tmuxinator_files = make_default_links(
        tmux_dir,
        ".tmuxinator")

    all_files["tmux"] = tmux_files
    all_files["tmuxinator"] = tmuxinator_files

    # Vim files
    vim_dir = DefaultLink(DOT_DIR, "vim")
    vim_files = make_default_links(
        vim_dir,
        ".vimrc")
    all_files["vim"] = vim_files

    return all_files


def setup_keyboard():
    """Link the keyboard directory to allow use of custom layout"""
    log.info("Setting up keyboard...")
    keyboard_dir = KeyboardLink(CONFIG_DIR, "keyboard")
    create_link(keyboard_dir)


def backup_overwrite(to_backup, backup_parent):
    """Create a backup of link_path in an appropriate directory"""
    if not os.path.exists(backup_parent):
        log.info("Creating directory {}".format(backup_parent))
        os.makedirs(backup_parent)
    backup_dir = newpath(
        backup_parent, ".dotfile_backup", time.strftime("%F"))
    if not os.path.exists(backup_dir):
        log.info("Creating directory {}".format(backup_dir))
        os.makedirs(backup_dir)
    else:
        log.debug("Directory {} already exists".format(backup_dir))
    target = newpath(backup_dir, to_backup)
    if os.path.exists(target):
        log.debug("File {} already exists, appending timestamp".format(target))
        target = target + "-" + time.strftime("%s")
    shutil.move(to_backup, target)
    log.debug("File {} moved to {}".format(to_backup, target))


def link_existing(link):
    """Attempt to create a symlink to an existing destination"""
    (target, link_path) = link.link_parameters
    if os.path.samefile(target, link_path):
        log.debug("Skipping file {} - Already linked".format(link_path))
    else:
        overwrite = require_yes_no(
            "File {} already exists, backup and overwrite?".format(
                link_path))
        if overwrite:
            backup_overwrite(link_path, HOME)
            create_soft_link(link)
        else:
            log.debug("Skipping file {} - File exists".format(link_path))


def create_link(link):
    """Create an individual symlink on the filesystem"""
    if os.path.exists(link.link_path):
        link_existing(link)
    else:
        create_soft_link(link)


# Maybe move this (and related functions) to the Link class?
# (There isn't much point in them if there isn't state!)
def create_links(files):
    """Make symlinks for files on the filesystem.

    files should be a list of Links"""
    for link in files:
        create_link(link)


def create_soft_link(link):
    """Create a soft link to target with name 'name'."""
    link_params = link.link_parameters
    try:
        subprocess.call(["ln", "-s"] + list(link_params))
    except subprocess.CalledProcessError:
        log.error("Failed to link {} to {}".format(*link_params))
    else:
        log.debug("Linked file: {} to {}".format(*link_params))


def link_files(req_prog, links, install):
    """Create symlinks for files that have a program requirement"""
    try:
        subprocess.check_output(["which", req_prog])
    except subprocess.CalledProcessError:
        if install:
            log.info("{} not found, attempting to install".format(req_prog))
            install_prog(req_prog)
        else:
            log.warn("{} not found, skipping related dotfiles".format(req_prog))
    else:
        create_links(links)


def install_prog(prog):
    def apt_install(prog):
        try:
            subprocess.check_call(["sudo apt-get install {} -y".format(prog)])
        except subprocess.CalledProcessError:
            log.warn("Could not install {}".format(prog))
            return False
        else:
            log.info("Successfully installed {}".format(prog))
            return True

    def retrieve_from_github(repo, path):
        try:
            subprocess.check_call(["git clone https://github.com/{} {}".format(repo, path)])
        except subprocess.CalledProcessError:
            log.warn("Could not clone repository: {}".format(repo))
            return False
        else:
            log.debug("Sucessfully cloned repository {}".format(repo))
            return True

    if prog in ["tmuxinator"]:
        try:
            subprocess.check_output(["which", "gem1.9.1"])
        except subprocess.CalledProcessError:
            log.warn("rubygems1.9.1 not found, cannot install tmuxinator")
            install_gem = require_yes_no("Install ruby1.9.1")
            if install_gem:
                if apt_install("ruby1.9.1"):
                    install_prog(prog)
            else:
                log.debug("Not installing ruby1.9.1")
        else:
            try:
                subprocess.check_call(["gem install --local {}".format(prog)])
            except subprocess.CalledProcessError:
                log.warn("Could not install {}".format(prog))
            else:
                log.info("Installed {}".format(prog))
    elif prog == "tmux":
        log.debug("Creating temporary directory")
        tmp = temp.mkdtemp()
        if retrieve_from_github("tmux/tmux.git", tmp):
            try:
                subprocess.check_call(["cd {}/tmux && sh autogen.sh && ./configure && make".format(tmp)])
            except subprocess.CalledProcessError:
                log.warn("Could not install tmux")
            else:
                log.debug("Successfully installed tmux")
            finally:
                log.debug("Removing temporary directory")
                shutil.rmtree(tmp)
    elif prog in ["git"]:
        apt_install(prog)


def setup_solarized_colors(color_dir):
    """Download files needed for solarized color in terminal"""
    log.info("Setting up colors...")
    color_path = newpath(color_dir, ".dir_colors")
    file_url = ("https://raw.githubusercontent.com"
                "/seebi/dircolors-solarized/"
                "master/dircolors.256dark")
    if os.path.exists(color_path):
        log.debug("{} already exists".format(color_path))
    else:
        log.info("{} not found, downloading from github...".format(
            color_path))
        try:
            subprocess.check_call(["wget", "-q", file_url, "-O", color_path])
        except subprocess.CalledProcessError:
            log.error("Could not download file {}".format(file_url))
        else:
            log.debug("File successfully downloaded.")


def get_options(sys_args):
    """Retrieve user options"""
    def usage():
        """Print the script's usage"""
        print("""Installer for GuiltyDolphin's dotfiles.
Hosted at https://www.github.com/GuiltyDolphin/config

Usage: python install.py [long option] [option] ...
long options:
  --help (-h) - Display this help message
  --link - Create symlinks to dotfiles
  --setup-colors - Download solarized colors
  --full - Setup everything
  --verbose (-v) - Verbose output
""")

    try:
        opts, _ = getopt.getopt(
            sys_args, "hv",
            ["help", "link", "setup-colors", "full", "verbose"])
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(2)

    options = {"link": True,
               "color": False,
               "verbose": False,
               "install": False}
    setups = ["link", "color"]
    for opt, _ in opts:
        if opt in ("-h", "--help"):
            usage()
            sys.exit()
        elif opt == "--link":
            options["link"] = True
        elif opt == "--setup-colors":
            options["color"] = True
        elif opt == "--install":
            options["install"] = True
        elif opt == "--full":
            for setup in setups:
                options[setup] = True
        elif opt in ("-v", "--verbose"):
            options["verbose"] = True
        else:
            raise NotImplementedError
    return options


def main():
    options = get_options(sys.argv[1:])
    if options["verbose"]:
        log.basicConfig(format="%(levelname)s: %(message)s", level=log.DEBUG)
    else:
        log.basicConfig(format="%(message)s", level=log.INFO)

    if options["link"]:
        log.info("Linking dotfiles...")
        prog_files = get_files()
        for (prog, links) in prog_files.items():
            log.debug("Linking files for {}".format(prog))
            link_files(prog, links, options["install"])
        setup_keyboard()
    if options["color"]:
        setup_solarized_colors(HOME)


if __name__ == '__main__':
    main()
