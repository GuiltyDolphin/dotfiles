#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# install.py
# Part of GuiltyDolphin's dotfiles
# Hosted at https://www.github.com/GuiltyDolphin/config

import subprocess


class Installer(object):

    def __init__(self, **kwargs):
        """
        Options:

        'install_command' : a function that should take a string and
        return a command that would be accepted by
        'subprocess.check_call'. The command should install the
        specified program.
        """
        self._prog_name = kwargs.get("prog_name")
        self._prog_requirements = kwargs.get("prog_requirements", [])
        self._install_command = kwargs.get("install_program", apt_command)

    @property
    def installed(self):
        """ True if the program is installed and has the correct
        version.
        """
        try:
            subprocess.check_output(["which"], self._prog_name)
            return True
        except subprocess.CalledProcessError:
            return False

    @property
    def requirements_satisfied(self):
        """ True if all of the requirements needed to install
        the program have been satisfied
        """
        return all(map(is_satisfied, self._prog_requirements))

    @property
    def install_command(self):
        """ A command accepted by 'subprocess.check_call' which is
        run to install the program.
        """
        return self._install_command(self._prog_name)

    def install(self):
        """ Runs the installer for the program.

        Returns True if the program is installed successfully.
        Returns False if if the program does not install successfully.
        """
        if self.installed:
            return True
        else:
            if self.satisfy_requirements:
                try:
                    subprocess.check_call(self.install_command)
                except subprocess.CalledProcessError:
                    return False

    def satisfy_requirements(self):
        for req in self._prog_requirements:
            if not satisfy(req):
                return False
        return True


def apt_command(prog):
    return ["apt-get", "install", prog, "-y"]


def is_satisfied(x):
    typ = type(x)
    if typ == bool:
        return x
    elif typ == Installer:
        return x.installed
    else:
        return False


def satisfy(x):
    """ Attempt to install/satisfy the specified program.
    """
    typ = type(x)
    if typ == Installer:
        return x.install()
    return False


ruby_opts = {
    "prog_name": "ruby",
    "prog_requirements": []
    }

ruby_prog = Installer(**ruby_opts)

tmuxinator_opts = {
    "prog_name": "tmuxinator",
    "prog_requirements": [ruby_prog]
    }

tmuxinator_prog = Installer(**tmuxinator_opts)
