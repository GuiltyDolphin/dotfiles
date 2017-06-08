#!/usr/bin/env sh

# Usage: guix-application-setup.sh

# Per-user Guix initial configuration script.
# Run this script (after ensuring Guix is up-to-date) to configure Guix as
# described in
# https://www.gnu.org/software/guix/manual/html_node/Application-Setup.html

setupLocales() {
  echo "Setting up locales..."
  guix package -i glibc-locales
}

setupNameServiceSwitch() {
  echo "Setting up Name Service Switch..."
  nscd
}

setupFonts() {
  echo "Setting up fonts..."
  for fontPackage in gs-fonts font-dejavu font-gnu-freefont-ttf; do
    echo "Installing $fontPackage"
    guix package -i $fontPackage
  done
  mkdir -p ~/.guix-profile/share/fonts/truetype
  xset +fp ~/.guix-profile/share/fonts/truetype
}

setupCertificates() {
  echo "Setting up certificates..."
  guix package -i nss-certs
}

applicationSetup() {
  setupLocales \
  && setupNameServiceSwitch \
  && setupFonts \
  && setupCertificates
}

applicationSetup
