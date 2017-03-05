#!/bin/bash

# To translate Markdown to *roff(troff).
mkdir .ignore
ls -1 *.md | awk -F '.' '{print "pandoc -s -t man " $0 " -o " ".ignore/"$2"."$1"\ncp .ignore/"$2"."$1" /usr/share/man/man"$1}' | bash
# cp .ignore/* /usr/share/man/man

