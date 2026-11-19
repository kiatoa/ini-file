#!/bin/sh

set -e
cd '/home/matt/data/ini-file'

mkdir -p "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11'
install -m 644 '/home/matt/data/ini-file/ini-file.static.o' "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.o'
install -m 644 '/home/matt/data/ini-file/ini-file.link' "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.link'

mkdir -p "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11'
install -m 755 '/home/matt/data/ini-file/ini-file.so' "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.so'

mkdir -p "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11'
install -m 755 '/home/matt/data/ini-file/ini-file.import.so' "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.import.so'

mkdir -p "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11'
rm -f "${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.egg-info'
cat >"${DESTDIR}"'/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.egg-info' <<'ENDINFO'
((installed-files
   "/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.o"
   "/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.link"
   "/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.so"
   "/home/matt/software/chicken/ck54.24ww29/lib/chicken/11/ini-file.import.so")
 (version "0.3.4")
 (author "Evan Hanson")
 (license "BSD")
 (dependencies regex)
 (components (extension ini-file)))
ENDINFO
