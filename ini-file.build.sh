#!/bin/sh

set -e
PATH='/home/matt/software/chicken/ck54.24ww29/bin':$PATH
export CHICKEN_CC='gcc'
export CHICKEN_CXX='g++'
export CHICKEN_CSC='/home/matt/software/chicken/ck54.24ww29/bin/csc'
export CHICKEN_CSI='/home/matt/software/chicken/ck54.24ww29/bin/csi'
cd '/home/matt/data/ini-file'

'/home/matt/software/chicken/ck54.24ww29/bin/chicken-do' '/home/matt/data/ini-file/ini-file.so' '/home/matt/data/ini-file/ini-file.import.scm' : 'ini-file.scm' '/home/matt/data/ini-file/ini-file.egg'  : '/home/matt/software/chicken/ck54.24ww29/bin/csc' '-host' '-D' 'compiling-extension' '-J' '-s' '-regenerate-import-libraries' '-setup-mode' '-I' '/home/matt/data/ini-file' '-C' '-I/home/matt/data/ini-file' '-O2' '-d1' 'ini-file.scm' '-o' '/home/matt/data/ini-file/ini-file.so'

'/home/matt/software/chicken/ck54.24ww29/bin/chicken-do' '/home/matt/data/ini-file/ini-file.static.o' '/home/matt/data/ini-file/ini-file.link' : 'ini-file.scm' '/home/matt/data/ini-file/ini-file.egg'  : '/home/matt/software/chicken/ck54.24ww29/bin/csc' '-regenerate-import-libraries' '-M' '-setup-mode' '-static' '-I' '/home/matt/data/ini-file' '-emit-link-file' '/home/matt/data/ini-file/ini-file.link' '-host' '-D' 'compiling-extension' '-c' '-unit' 'ini-file' '-D' 'compiling-static-extension' '-C' '-I/home/matt/data/ini-file' '-O2' '-d1' 'ini-file.scm' '-o' '/home/matt/data/ini-file/ini-file.static.o'

'/home/matt/software/chicken/ck54.24ww29/bin/chicken-do' '/home/matt/data/ini-file/ini-file.import.so' : 'ini-file.import.scm'  : '/home/matt/software/chicken/ck54.24ww29/bin/csc' '-setup-mode' '-s' '-host' '-I' '/home/matt/data/ini-file' '-C' '-I/home/matt/data/ini-file' '-O2' '-d0' 'ini-file.import.scm' '-o' '/home/matt/data/ini-file/ini-file.import.so'
