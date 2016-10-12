#!/bin/bash

test_files=$(find test -name 'test*.scm')

for f in ${test_files}
do
    echo "#|   ${f}    |#"
    scheme-script ${f}
done

