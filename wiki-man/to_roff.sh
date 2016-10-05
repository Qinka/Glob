#!/bin/bash

# To translate Markdown to *roff(troff).

ls -1 *.md | awk -F '.' '{print "pandoc -s -t man " $0 " -o " $2"."$1}' | bash
