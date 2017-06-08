#!/usr/bin/env sh

# Usage: install-guix.sh

# Installation script for Guix
# Only run this script if Guix has not yet been installed on the system
# This script will likely need to run with root privileges

if [ $(which guix) ]; then
  echo "Already installed"
  exit
fi

binaryFile='guix-binary-0.13.0.x86_64-linux.tar.xz';

downloadBinary() {
  sigFile="$binaryFile.sig"
  if [ ! -e $binaryFile ]; then
    wget "ftp://alpha.gnu.org/gnu/guix/$binaryFile"
  fi
  wget "ftp://alpha.gnu.org/gnu/guix/$sigFile" -O $sigFile
  gpg --keyserver pgp.mit.edu --recv-keys 3CE464558A84FDC69DB40CFB090B11993D9AEBB5
  gpg --verify $sigFile
}

unpackTar() {
  if [ ! -e /gnu ]; then
    tar --warning=no-timestamp -xf $binaryFile \
    && mv var/guix /var/ \
    && mv gnu /
  fi
}

initializeRootProfile() {
  # Make root’s profile available under ~/.guix-profile
  ln -sf /var/guix/profiles/per-user/root/guix-profile ~root/.guix-profile
  # Source etc/profile to augment PATH and other relevant environment variables
  GUIX_PROFILE=$HOME/.guix-profile source $GUIX_PROFILE/etc/profile
}

# Create the group and user accounts for build users
createGroupAndUserAccountsForBuildUsers() {
  if ! grep -i '^guixbuild:' /etc/group; then
    groupadd --system guixbuild
  fi
  for i in `seq -w 1 10`; do
    if ! id "guixbuilder$i" >/dev/null 2>&1; then
      useradd -g guixbuild -G guixbuild \
        -d /var/empty -s `which nologin` \
        -c "Guix build user $i" --system  \
        guixbuilder$i;
    fi
  done
}

setupDaemon() {
  # Run the daemon, and set it to automatically start on boot
  # If host distro uses the systemd init system
  if [ $(which systemctl) ]; then
    cp ~root/.guix-profile/lib/systemd/system/guix-daemon.service \
      /etc/systemd/system/
    systemctl start guix-daemon && systemctl enable guix-daemon
  # If host distro uses the Upstart init system
  elif [ $(which initctl) ]; then
    initctl reload-configuration
    cp ~root/.guix-profile/lib/upstart/system/guix-daemon.conf /etc/init/
    if ! echo "$(status guix-daemon)" | grep 'running'; then
      start guix-daemon
    fi
  fi
}

# Make the guix command available to other users on the machine
makeGuixCommandAvailable() {
  mkdir -p /usr/local/bin
  cd /usr/local/bin
  ln -s /var/guix/profiles/per-user/root/guix-profile/bin/guix
}

# Make the Info version of the manual available
makeInfoAvailable() {
  mkdir -p /usr/local/share/info
  cd /usr/local/share/info
  for i in /var/guix/profiles/per-user/root/guix-profile/share/info/* ; do
    if [ ! -e "$i" ]; then
      ln -s $i
    fi
  done
}

# Authorize substitutes from hydra.gnu.org
authorizeSubstitutes() {
  guix archive --authorize < ~root/.guix-profile/share/guix/hydra.gnu.org.pub
}

setupGuix() {
  cd /tmp \
  && downloadBinary \
  && unpackTar \
  && initializeRootProfile \
  && createGroupAndUserAccountsForBuildUsers \
  && setupDaemon \
  && makeGuixCommandAvailable \
  && makeInfoAvailable \
  && authorizeSubstitutes
}

setupGuix
