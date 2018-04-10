#!/bin/bash

stack build && stack exec -- testProject-exe && open images/example_sketch/latest.png
